####################   Hotel bookings case study   ####################

# following these instructions to learn about the tidymodels package and 
# functions: https://www.tidymodels.org/start/case-study/

# build a model to predict which actual hotel stays included children 
# and/or babies, and which did not


# Load packages -----------------------------------------------------------

library(tidymodels)  

# Helper packages
library(readr)       # for importing data
library(vip)         # for variable importance plots


# Load & analyze data -----------------------------------------------------

hotels <- 
  read_csv("https://tidymodels.org/start/case-study/hotels.csv") %>%
  mutate(across(where(is.character), as.factor))

# check the dimension of the imported dataset
dim(hotels)

# get a more detailed overview
glimpse(hotels)

# check the percentage of stays with children
hotels %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))


# Data Splitting & Resampling ---------------------------------------------

set.seed(123)
splits      <- initial_split(hotels, strata = children)

hotel_other <- training(splits)
hotel_test  <- testing(splits)

# training set proportions by children
hotel_other %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))
#> # A tibble: 2 × 3
#>   children     n   prop
#>   <fct>    <int>  <dbl>
#> 1 children  3027 0.0807
#> 2 none     34473 0.919

# test set proportions by children
hotel_test  %>% 
  count(children) %>% 
  mutate(prop = n/sum(n))
#> # A tibble: 2 × 3
#>   children     n   prop
#>   <fct>    <int>  <dbl>
#> 1 children  1011 0.0809
#> 2 none     11489 0.919


# validation set generation
set.seed(234)
val_set <- validation_split(hotel_other, 
                            strata = children, 
                            prop = 0.80)

val_set


# 1st model:  penalized logistic regression -------------------------------

# build model
lr_mod <- 
  logistic_reg(penalty = tune(), mixture = 1) %>% 
  set_engine("glmnet")

# create recipe
holidays <- c("AllSouls", "AshWednesday", "ChristmasEve", "Easter", 
              "ChristmasDay", "GoodFriday", "NewYearsDay", "PalmSunday")

lr_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date, holidays = holidays) %>% 
  step_rm(arrival_date) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_predictors())

# create workflow
lr_workflow <- 
  workflow() %>% 
  add_model(lr_mod) %>% 
  add_recipe(lr_recipe)

# create tuning grid
lr_reg_grid <- tibble(penalty = 10^seq(-4, -1, length.out = 30))

lr_reg_grid %>% top_n(-5) # lowest penalty values
lr_reg_grid %>% top_n(5)  # highest penalty values


# train and tune model
lr_res <- 
  lr_workflow %>% 
  tune_grid(val_set,
            grid = lr_reg_grid,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

lr_plot <- 
  lr_res %>% 
  collect_metrics() %>% 
  ggplot(aes(x = penalty, y = mean)) + 
  geom_point() + 
  geom_line() + 
  ylab("Area under the ROC Curve") +
  scale_x_log10(labels = scales::label_number())

lr_plot 

top_models <-
  lr_res %>% 
  show_best(metric = "roc_auc", n = 15) %>% 
  arrange(penalty) 
top_models

lr_best <- 
  lr_res %>% 
  collect_metrics() %>% 
  arrange(penalty) %>% 
  slice(12)
lr_best

lr_auc <- 
  lr_res %>% 
  collect_predictions(parameters = lr_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Logistic Regression")

autoplot(lr_auc)



# 2nd model: tree-based ensemble ------------------------------------------


