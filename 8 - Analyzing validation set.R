

# Pull validation set -----------------------------------------------------

release_validate_patients <-
  readr::read_csv(
    file = here("data",
                "raw",
                "DDXPlus",
                "release_validate_patients.csv")
  )


# Pull other data ---------------------------------------------------------

evidences_tibble <-
  readr::read_rds(
    file = here("data",
                "processed",
                "evidences_tibble.Rds")
  )

conditions_tibble <-
  readr::read_rds(
    file = here("data",
                "processed",
                "conditions_tibble.Rds")
  )

evidence_classes_df <-
  readr::read_rds(
    file = here("analysis objects",
                "evidence_classes_df.Rds")
  )


# Pull fitted workflow ----------------------------------------------------

sample_fit <-
  readr::read_rds(
    file = here("analysis objects",
                "workflow_sample_fit.Rds")
  )

# Analysis ----------------------------------------------------------------

test_prep <- prepare_for_prediction(data = release_validate_patients,
                                    conditions_tibble = conditions_tibble,
                                    evidence_classes_df = evidence_classes_df)


predictions_prob_df <- 
  predict(sample_fit, 
          test_prep$data_for_prediction,
          type = "prob")

readr::write_rds(
  predictions_prob_df,
  file = here("analysis objects",
              "validation_set_predictions_prob_df.Rds")
)

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


output_differential <- nest_differential(
  test_data_prob_predict = predictions_prob_df_test)
output_differential
output_differential$differential_diagnosis[1]
