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

# Build the model and improve training time
cores <- parallel::detectCores()
cores

rf_mod <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 1000) %>% 
  set_engine("ranger", num.threads = cores) %>% 
  set_mode("classification")

# create recipe and workflow
rf_recipe <- 
  recipe(children ~ ., data = hotel_other) %>% 
  step_date(arrival_date) %>% 
  step_holiday(arrival_date) %>% 
  step_rm(arrival_date) 

rf_workflow <- 
  workflow() %>% 
  add_model(rf_mod) %>% 
  add_recipe(rf_recipe)

# train and tune model
rf_mod

extract_parameter_set_dials(rf_mod) # show what will be tuned

set.seed(345)
rf_res <- 
  rf_workflow %>% 
  tune_grid(val_set,
            grid = 25,
            control = control_grid(save_pred = TRUE),
            metrics = metric_set(roc_auc))

rf_res %>% 
  show_best(metric = "roc_auc")

autoplot(rf_res)

rf_best <- 
  rf_res %>% 
  select_best(metric = "roc_auc") # final tuning parameters
rf_best

rf_res %>% 
  collect_predictions()

rf_auc <- 
  rf_res %>% 
  collect_predictions(parameters = rf_best) %>% 
  roc_curve(children, .pred_children) %>% 
  mutate(model = "Random Forest")


bind_rows(rf_auc, lr_auc) %>% 
  ggplot(aes(x = 1 - specificity, y = sensitivity, col = model)) + 
  geom_path(lwd = 1.5, alpha = 0.8) +
  geom_abline(lty = 3) + 
  coord_equal() + 
  scale_color_viridis_d(option = "plasma", end = .6)


# Last fit ----------------------------------------------------------------

# the last model
last_rf_mod <- 
  rand_forest(mtry = 8, min_n = 7, trees = 1000) %>% 
  set_engine("ranger", num.threads = cores, importance = "impurity") %>% 
  set_mode("classification")

# the last workflow
last_rf_workflow <- 
  rf_workflow %>% 
  update_model(last_rf_mod)

# the last fit
set.seed(345)
last_rf_fit <- 
  last_rf_workflow %>% 
  last_fit(splits)

last_rf_fit

last_rf_fit %>% 
  collect_metrics()

last_rf_fit %>% 
  extract_fit_parsnip() %>% 
  vip(num_features = 20)

last_rf_fit %>% 
  collect_predictions() %>% 
  roc_curve(children, .pred_children) %>% 
  autoplot()
