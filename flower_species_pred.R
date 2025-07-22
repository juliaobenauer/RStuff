### Predicting Species with tidymodels ###


# load packages
library(tidymodels)

# Prepare data
iris_binary <- iris %>%
  mutate(Species = as.factor(ifelse(Species == "setosa", "setosa", "other")))

# Split data
set.seed(123)
iris_split <- initial_split(iris_binary, prop = 0.75, strata = Species)
iris_train <- training(iris_split)
iris_test  <- testing(iris_split)

# Create recipe and model
iris_recipe <- recipe(Species ~ ., data = iris_train) %>%
  step_normalize(all_numeric_predictors())

rf_model <- rand_forest(mtry = 2, trees = 100, min_n = 5) %>%
  set_engine("ranger") %>%
  set_mode("classification")

# Create workflow and fit model
rf_workflow <- workflow() %>%
  add_model(rf_model) %>%
  add_recipe(iris_recipe)

rf_fit <- rf_workflow %>% fit(data = iris_train)

# Make predictions and evaluate
rf_predictions <- predict(rf_fit, iris_test) %>%
  bind_cols(iris_test)

rf_predictions %>% 
  metrics(truth = Species, estimate = .pred_class) %>% 
  print()

rf_predictions %>% 
  conf_mat(truth = Species, estimate = .pred_class) %>% 
  print()

