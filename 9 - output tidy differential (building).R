
ddx2$DIFFERENTIAL_DIAGNOSIS[1]

tidy_pred <- tidy_prediction(test_data_prob_predict = predictions_prob_df_test)

test <- tidy_pred %>% 
  group_by(patientId) %>% 
  mutate(prob = prob * (1/sum(prob, 
                              na.rm = TRUE))) %>% 
  summarize(diagnosis = list(diagnosis),
            prob = list(prob))

# test <- tidy_pred %>% 
#   group_by(patientId) %>% 
#   summarize(diagnosis = list(diagnosis),
#             prob = list(prob))

dirtify_differential <-
  function(diagnosis_vec,
           prob_vec){
    # combines a vector of differential diagnosis and a vector of 
    # probabilities from one patient, and converts to formal
    # similar to raw release patients from ddxplus.
    # Args:
    #   1. diagnosis_vec: vector of differential diagnosis
    #   2. prob_vec: vector of probabilities of diagnosis_vec
    
    test_map <-
      map(1:length(diagnosis_vec[[1]]),
          .f = function(i){
            
            diagnosis <- diagnosis_vec[[1]][i]
            prob <- prob_vec[[1]][i]
            paste0("['",diagnosis,"', ",prob,"]")
            
          })
    test_map
    
    concateneted_diffdiag <- paste0(test_map, collapse = ", ")
    paste0("[",concateneted_diffdiag,"]")
  }
    
Rprof()
test_output_diff_diag <- test %>% 
  mutate(differential_diagnosis = 
           dirtify_differential(diagnosis, prob),
         .by = patientId, .keep = "unused")
Rprof(NULL)
summaryRprof()

dirtify_differential_tidy <- 
  function(data, diagnosis, prob){
    data %>% 
      mutate(differential_diagnosis = 
               dirtify_differential(diagnosis, prob),
             .by = patientId, .keep = "unused")
  }

test %>% 
  slice(1:10) %>% 
  dirtify_differential_tidy(diagnosis = diagnosis,
                            prob = prob)

test_output_diff_diag


test_output_diff_diag$differential_diagnosis[2]




# Function use ------------------------------------------------------------

nest_differential <-
  function(test_data, test_data_prob_predict){
    
    message("MESSAGE: Pivoting differential predictions...")
    tidy_pred <- tidy_prediction(
      test_data = test_data,
      test_data_prob_predict = test_data_prob_predict)
    message("MESSAGE: Pivoted differential predictions...")
    
    test <- tidy_pred %>% 
      group_by(patientId) %>% 
      summarize(diagnosis = list(diagnosis),
                prob = list(prob))
    
    message("MESSAGE: Nesting differential predictions...")
    test %>% 
      dirtify_differential(diagnosis = diagnosis,
                                prob = prob)
    
  }

test <- nest_differential(test_data = test_prep$actual_differential_diagnosis,
                  test_data_prob_predict = predictions_prob_df_test)
test

