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


