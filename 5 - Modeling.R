
library(tidymodels); tidymodels_prefer()
library(here)
library(randomForest)
library(themis)

# 1. Load Dataset/s -----------------------------------------------------------

ddxplus_training_4 <- readr::read_rds(
  file = here("data", 
              "processed",
              "ddxplus_training_4.Rds")
)

# Before splitting the data, we convert the differential_case_weight
# column to importance weights

ddxplus_training_5 <- ddxplus_training_4 %>% 
  mutate(differential_case_weight = 
           importance_weights(differential_case_weight))
rm(ddxplus_training_4)


# 2. Sampled data supervised learning ----------------------------------------


## 2.1. Data Split --------------------------------------------------------------

rm(ddxplus_sample)

patients_n <- length(unique(ddxplus_training_5$patientId)) # count of all patients

set.seed(123)
sample_patients <- 
  sample(unique(ddxplus_training_5$patientId), 
         size = floor(patients_n * 0.05)) # Sample only 5% of the patients
ddxplus_sample <-
  ddxplus_training_5 %>% 
  filter(patientId %in% sample_patients)

rm(ddxplus_training_5)

set.seed(502)
ddxplus_split <- group_initial_split(ddxplus_sample,
                                     group = patientId,
                                     prop = 0.70)
ddxplus_train <- training(ddxplus_split)
ddxplus_test  <-  testing(ddxplus_split)

rm(ddxplus_sample, ddxplus_split, sample_patients, patients_n)

## 2.2. Specify model (Parsnip) --------------------------------------------------------

# parsnip_addin()
library(ranger)

rand_forest_ranger_spec <-
  rand_forest(mtry = 15, min_n = 5, trees = 1000) %>%
  set_engine('ranger') %>%
  set_mode('classification')


## 2.3. Feature engineering (recipe) ---------------------------------------------

recipe_2 <-
  recipe(diagnosis ~ ., 
         data = ddxplus_train) %>% 
  update_role(patientId, new_role = "Id") %>% 
  step_other(INITIAL_EVIDENCE,
             douleurxx_endroitducorps,
             douleurxx_irrad,
             lesions_peau_endroitducorps, threshold = 0.01) %>% 
  step_other(douleurxx_carac, threshold = 0.05) %>% 
  step_other(oedeme_endroitducorps, threshold = 0.001) %>% 
  step_novel(all_string_predictors()) %>%
  step_mutate(across(where(is.character),
                     .fns = as.factor)) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.65)


## 2.4. Create the workflow --------------------------------------------------

wf_2 <-
  workflow() %>% 
  add_case_weights(differential_case_weight) %>% 
  add_recipe(recipe_2) %>% 
  add_model(rand_forest_ranger_spec)


## 2.5. Model fitting -----------------------------------------------------------

training_start <- Sys.time()

fit_2 <- 
  wf_2 %>% 
  fit(ddxplus_train)

training_end <- Sys.time()
training_duration <- training_end - training_start
training_duration
# 1.1 hours

readr::write_rds(fit_2,
                 file = here("analysis objects",
                             "workflow_sample_fit.Rds"))

rm(wf_2, recipe_2, ddxplus_train)



# 3. Complete data supervised learning ----------------------------------------


## 3.1. Data Split --------------------------------------------------------------


## 3.2. Specify model (Parsnip) --------------------------------------------------------

# parsnip_addin()
library(ranger)

rand_forest_ranger_spec <-
  rand_forest(mtry = 15, min_n = 5, trees = 1000) %>%
  set_engine('ranger') %>%
  set_mode('classification')


## 3.3. Feature engineering (recipe) ---------------------------------------------

recipe_complete <-
  recipe(diagnosis ~ ., 
         data = ddxplus_training_5) %>% 
  update_role(patientId, new_role = "Id") %>% 
  step_other(INITIAL_EVIDENCE,
             douleurxx_endroitducorps,
             douleurxx_irrad,
             lesions_peau_endroitducorps, threshold = 0.01) %>% 
  step_other(douleurxx_carac, threshold = 0.05) %>% 
  step_other(oedeme_endroitducorps, threshold = 0.001) %>% 
  step_novel(all_string_predictors()) %>%
  step_mutate(across(where(is.character),
                     .fns = as.factor)) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) %>%
  step_pca(all_numeric_predictors(), threshold = 0.75)


## 3.4. Create the workflow --------------------------------------------------

wf <-
  workflow() %>% 
  add_case_weights(differential_case_weight) %>% 
  add_recipe(recipe_complete) %>% 
  add_model(rand_forest_ranger_spec)


## 3.5. Model fitting -----------------------------------------------------------

training_start <- Sys.time()

fit <- 
  wf %>% 
  fit(ddxplus_training_5)

training_end <- Sys.time()
training_duration <- training_end - training_start
training_duration
# Cancelled. Ran 10 hours straight - not yet finished


readr::write_rds(fit,
                 file = here("analysis objects",
                             "workflow_fit.Rds"))




























