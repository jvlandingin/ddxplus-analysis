

# Pull data ---------------------------------------------------------------



release_train_patients_raw <- 
  readr::read_rds(file = here("data","processed","release_train_patients_raw.Rds"))

evidences_tibble <- 
  readr::read_rds(file = here("data","processed","evidences_tibble.Rds"))

conditions_tibble <- 
  readr::read_rds(file = here("data","processed","conditions_tibble.Rds"))

evidence_classes_df <-
  readr::read_rds(file = here("analysis objects",
                              "evidence_classes_df.Rds"))

rm(evidences_tibble)

# Data prep ---------------------------------------------------------------

# Add patient Id
release_train_patients <- release_train_patients_raw %>% 
  mutate(patientId = 1:nrow(.), .before = AGE)
rm(release_train_patients_raw)


# only needed for training
# around 30 sec runtime
ddx2 <-
  translate_release_to_eng(release_train_patients,
                           conditions_tibble)
# run below if only for prediction
ddx2 <- release_train_patients
rm(release_train_patients)

Rprof()
ddx3.1 <- unnest_evidences(ddx2[1:10000,],evidence_classes_df)
rm(ddx2)

Rprof(NULL)
summaryRprof()

# not needed for prediction
ddx3.2 <- 
  unnest_differential(
    data = ddx3.1,
    count_diagnosis_keep = 5)
ddx3.2 <- ddx3.1
rm(ddx3.1)


ddx3.3 <- feature_engineering_final_touch(ddx3.2)
rm(ddx3.2)

ddx4 <- treat_nas(ddx3.3)
rm(ddx3.3)

# Prediction

prediction_data <-
  ddx4 %>% 
  select(-diagnosis,
         -PATHOLOGY,
         -differential_case_weight) %>% 
  distinct(patientId, .keep_all = TRUE)

metric_checking_df <-
  ddx4 %>% 
  select(patientId, diagnosis, differential_case_weight)

predictions_prob_df <- 
  predict(sample_fit, 
          prediction_data,
          type = "prob")

predictions_prob_df_test <- predictions_prob_df %>% 
  mutate(patientId = prediction_data$patientId) %>% 
  relocate(patientId) %>% 
  slice_head(n = 1, 
             by = patientId)

plan(multisession, workers = 8)
gathered_metrics <- gather_differential_metrics(test_data = metric_checking_df,
                                                test_data_prob_predict = predictions_prob_df_test)
gathered_metrics
mean(gathered_metrics$metric1)
mean(gathered_metrics$metric2)
mean(gathered_metrics$correlation, na.rm = TRUE)


# Function building -------------------------------------------------------

prepare_for_prediction <-
  function(data, 
           conditions_tibble,
           evidence_classes_df,
           prediction_only = F){
    # Prepare raw data for prediction
    # Args:
    # 1. data: raw data containing
    
    # Add patient Id
    release_train_patients <- data %>% 
      mutate(patientId = 1:nrow(.), .before = AGE)
    rm(data)
    message("Message: Added patient ID...(Done 1/5)")
    
    # only needed for training
    # around 30 sec runtime
    if (prediction_only == F){
      ddx2 <-
        translate_release_to_eng(release_train_patients,
                                 conditions_tibble)
      message("Message: Translated pathology and differential diagnosis to english...(Done 2/5)")
    } else {
      # run below if only for prediction
      ddx2 <- release_train_patients
      message("Message: Skipped pathology and differential diagnosis translation to english...(Done 2/5)")
    }
    rm(release_train_patients)

    
    ddx3.1 <- unnest_evidences(ddx2,evidence_classes_df)
    message("Message: Unnested evidences...(Done 3.1/5)")
    rm(ddx2)
    
    if (prediction_only == F){
      # not needed for prediction
      ddx3.2 <- 
        unnest_differential(
          data = ddx3.1,
          count_diagnosis_keep = 5)
      message("Message: Unnested differential diagnosis...(Done 3.2/5)")
    } else {
      ddx3.2 <- ddx3.1
      message("Message: Skipped differential diagnosis unnest...(Done 3.2/5)")
    }
    rm(ddx3.1)
    
    
    ddx3.3 <- feature_engineering_final_touch(ddx3.2)
    message("Message: Finished feature data prep...(Done 3.3/5)")
    rm(ddx3.2)
    
    ddx4 <- treat_nas(ddx3.3)
    message("Message: Treated NAs...(Done 4/5)")
    rm(ddx3.3)
    
    prediction_data <-
      ddx4 %>% 
      select(-diagnosis,
             -PATHOLOGY,
             -differential_case_weight) %>% 
      distinct(patientId, .keep_all = TRUE)
    
    metric_checking_df <-
      ddx4 %>% 
      select(patientId, diagnosis, differential_case_weight)
    
    message("Message: Done with data preparation YAY :D (Done 5/5)")
    
    list(data_for_prediction = prediction_data,
         actual_differential_diagnosis = metric_checking_df)
    
  }

test_prep <- prepare_for_prediction(data = release_train_patients_raw[1000:1200,],
                       conditions_tibble = conditions_tibble,
                       evidence_classes_df = evidence_classes_df)


predictions_prob_df <- 
  predict(sample_fit, 
          test_prep$data_for_prediction,
          type = "prob")

predictions_prob_df_test <- predictions_prob_df %>% 
  mutate(patientId = test_prep$data_for_prediction$patientId) %>% 
  relocate(patientId) %>% 
  distinct(patientId, 
           .keep_all = TRUE)

gathered_metrics <- 
  gather_differential_metrics(
    test_data = test_prep$actual_differential_diagnosis,
    test_data_prob_predict = predictions_prob_df_test)
gathered_metrics

mean(gathered_metrics$metric1)
mean(gathered_metrics$metric2)
mean(gathered_metrics$correlation, na.rm = TRUE)
