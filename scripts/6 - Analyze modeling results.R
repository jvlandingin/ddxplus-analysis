

# Overview ----------------------------------------------------------------

# In this script, we analyze the results from modeling (script #5)

# We answer the following:

# What are different ways we can define our metrics?

# 1. How many conditions from the real differential diagnosis were
# captured in the predicted differential diagnosis? 
# Measure via proportions e.g. 4/5

# 2. How many conditions in the pred diff_diag are not in 
# the real diff_diag?
# Measure via proportions e.g. 1/5

# 3. Among the real conditions found in the predicted differential diagnosis,
# how similar is the differential ranking of those conditions? 
# We can use correlation to measure this similarity.

# Import data ----------------------------------------------------------------

# Read the fitted workflow
sample_fit <- 
  readr::read_rds(
    file = here("analysis objects",
                "workflow_sample_fit.Rds")
  )


# Check fitted workflow ---------------------------------------------------

rf_fit <- extract_fit_parsnip(sample_fit)

prep_fit <- extract_mold(sample_fit)

prep_recipe <- recipe_2 %>% 
  prep()
prep_recipe$steps[[8]]$res$sdev # this is the PCA results

sum(prep_recipe$steps[[8]]$res$sdev)
summary(prep_recipe$steps[[8]]$res)

# The cumulative proportion from PCA was only 30%. Let us run again but
# next time set a threshold of 80%

# Get predictions ---------------------------------------------------------

prediction_data <-
  ddxplus_test %>% 
  select(-diagnosis) %>% 
  distinct(patientId, .keep_all = TRUE)
  

predictions_prob_df <- 
  predict(sample_fit, 
          prediction_data[1:100,],
          type = "prob")

# rm(sample_fit)

# readr::write_rds(predictions_prob_df,
#                  file = here("analysis objects",
#                              "workflow_sample_fit_predictions.Rds"))

predictions_prob_df <- 
  readr::read_rds(
    file = here("analysis objects",
                "workflow_sample_fit_predictions.Rds")
  )


# Data preparation --------------------------------------------------------

metric1_computation_df <- 
  ddxplus_test %>% 
  select(patientId, 
         diagnosis,
         differential_case_weight) %>% 
  mutate(prob_rank = 
           rank(desc(differential_case_weight)),
         .by = patientId)

pred_df <- predictions_prob_df %>% 
  mutate(patientId = ddxplus_test$patientId) %>% 
  relocate(patientId) %>% 
  slice_head(n = 1, by = patientId) # only get 1 sample per patient

pred_df_longer <- pred_df %>% 
  pivot_longer(-patientId, 
               names_to = "diagnosis",
               values_to = "prob") %>% 
  filter(prob > 0) %>% 
  group_by(patientId) %>% 
  arrange(desc(prob), .by_group = TRUE) %>% 
  ungroup()

# only keep diagnosis that has probability more than 5%
# or if they are within top 5 diagnoses
pred_df_longer <- pred_df_longer %>% 
  mutate(prob_rank = rank(-prob),
         .by = patientId) %>% 
  filter(prob > 0.05 | prob_rank <= 5) 

# remove .pred_ from diagnosis
pred_df_longer <- pred_df_longer %>% 
  mutate(diagnosis = str_remove(diagnosis,
                                pattern = ".pred_"))

pred_df_longer %>% 
  summarize(diff_diag_size = n(), 
            .by = patientId) %>% 
  ggplot(aes(diff_diag_size)) + 
  geom_histogram()


# Checking metric #1 ------------------------------------------------------


# test

truth <- metric1_computation_df$diagnosis[
  metric1_computation_df$patientId == 38
  ]
estimate <-
  pred_df_longer$diagnosis[
    pred_df_longer$patientId == 38
  ]

matches <- intersect(truth, estimate)
truth_diag_counnt <- length(truth)

matching_diagnosis_prop <-
  function(truth, estimate){
    
    matches <- intersect(truth, estimate)
    truth_diag_counnt <- length(truth)
    
    metric <- length(matches)/truth_diag_counnt
    
    return(metric)
    
  }

matching_diagnosis_prop(truth, estimate)
patients_testSet <- unique(ddxplus_test$patientId)


library(furrr)
plan(multisession, workers = 8)
matching_diag_prop_metric <- map(patients_testSet,
    .f = function(patientId){
      
      truth <- metric1_computation_df$diagnosis[
        metric1_computation_df$patientId == patientId
      ]
      estimate <-
        pred_df_longer$diagnosis[
          pred_df_longer$patientId == patientId
        ]
      
      matching_diagnosis_prop(truth, estimate)
      
    }, .progress = TRUE)
matching_diag_prop_metric <- unlist(matching_diag_prop_metric)

mean(matching_diag_prop_metric)
hist(matching_diag_prop_metric)
mean(matching_diag_prop_metric == 1) # of patients that had complete count of diagnosis


# Checking metric #2 ------------------------------------------------------

truth <- metric1_computation_df$diagnosis[
  metric1_computation_df$patientId == 38
]
estimate <-
  pred_df_longer$diagnosis[
    pred_df_longer$patientId == 38
  ]


plan(multisession, workers = 8)
setdiff_diag_prop_metric <-
  future_map_dbl(patients_testSet,
      .f = function(patientId){
        
        truth <- metric1_computation_df$diagnosis[
          metric1_computation_df$patientId == patientId
        ]
        estimate <-
          pred_df_longer$diagnosis[
            pred_df_longer$patientId == patientId
          ]
        
        different_n <- length(setdiff(estimate, truth))
        estimate_n <- length(estimate)
        
        different_n/estimate_n
        
      }, .progress = TRUE)

mean(setdiff_diag_prop_metric)
hist(setdiff_diag_prop_metric)
mean(setdiff_diag_prop_metric == 0)




# Checking metric #3 ------------------------------------------------------

truth_join_pred <- metric1_computation_df %>% 
  inner_join(pred_df_longer,
             by = c("patientId", "diagnosis")) %>% 
  mutate(prob_rank.y = rank(desc(prob)),
         .by = patientId)
rankCorrelation_diffDiag_metric_df <- truth_join_pred %>% 
  summarize(correlation =
              cor(prob_rank.x,
                  prob_rank.y),
            .by = patientId) %>% 
  filter(!is.na(correlation))
  
mean(rankCorrelation_diffDiag_metric_df$correlation)
hist(rankCorrelation_diffDiag_metric_df$correlation)
mean(rankCorrelation_diffDiag_metric_df$correlation == 1)




# Function for gathering metrics ------------------------------------------

# only get 1 row per pattientId
predictions_prob_df_test <- predictions_prob_df %>% 
  mutate(patientId = ddxplus_test$patientId) %>% 
  relocate(patientId) %>% 
  slice_head(n = 1, 
             by = patientId)


plan(multisession, workers = 12)
gathered_metrics <- gather_differential_metrics(test_data = ddxplus_test,
                                                test_data_prob_predict = predictions_prob_df_test)
gathered_metrics

mean(gathered_metrics$metric1)
mean(gathered_metrics$metric2)
mean(gathered_metrics$correlation, na.rm = TRUE)


