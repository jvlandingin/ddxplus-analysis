
# Setup ---------------------------------------------------------------

source("1 - Libraries.R")
theme_set(theme_classic())

# Import Data -------------------------------------------------------------


# source("2 - Data import and preprocessing.R")
# release_train_patients <- readr::read_rds(file = here("data","raw","release_train_patients.Rds"))
conditions_tibble <- readr::read_rds(file = here("data","processed","conditions_tibble.Rds"))


# Feature engineering ---------------------------------------------------------

# run feature engineering (2hrs)
# source("3 - Feature engineering.R")
# read

training_data <- readr::read_rds(file = here("data", "processed", "training.Rds"))




