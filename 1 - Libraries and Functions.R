
# Libraries ---------------------------------------------------------------

# Here, we load the paackages to be used

library(here)
library(tidyverse)
library(jsonlite)
library(furrr)
library(tidymodels); tidymodels_prefer()
library(themis)
library(ranger)
library(tidytext)
conflicted::conflicts_prefer(stringr::fixed)

# Functions ---------------------------------------------------------------


## Script 2 ---------------------------------------------------------------------


translate_release_to_eng <-
  function(release_patients_data,
           conditions_tibble){
    # Translates the DIFFERENTIAL_DIAGNOSIS and PATHOLOGY of 
    # release data from french to english
    # Args:
    # 1. release_patients_data: release patient data
    # 2. conditions_tibble: tibble that contains condition_name (fr) and
    # its english translation cond_name_eng 
    
    release_train_patients <- release_patients_data
    
    # 1. Translate pathology to english 
    
    conditions_translate_helper <- 
      select(conditions_tibble, condition_name, cond_name_eng) %>% 
      rename(PATHOLOGY = condition_name)
    
    release_train_patients_eng <- release_train_patients %>% 
      left_join(conditions_translate_helper) %>% 
      mutate(PATHOLOGY = cond_name_eng, .keep = "unused")
    
    # 2. Translate differential diagnosis to english
    
    diff_diag <- release_train_patients$DIFFERENTIAL_DIAGNOSIS
    
    diff_diag_eng <- stringr::str_replace_all(
      string = diff_diag,
      pattern = setNames(
        fixed(conditions_translate_helper$cond_name_eng),
        conditions_translate_helper$PATHOLOGY
      )
    )
    
    
    release_train_patients_eng$DIFFERENTIAL_DIAGNOSIS <- diff_diag_eng
    
    # # 3. Add patient Id to data
    # release_train_patients_eng <- 
    #   release_train_patients_eng %>% 
    #   mutate(patientId = 1:nrow(.)) %>% 
    #   relocate(patientId)
    
    release_train_patients_eng
    
    
  }


## Script 3.1 ------------------------------------------------------------

cook_empty_tibble <-
  function(data, evidence_classes_df){
    # helper function for unnest_evidences
    # create an empty tibble to be binded with the df with unnested evidences
    # this is to ensure that in the new df, all evidences are accounted for
    # Args:
    # 1. data: the output from unnest_evidences
    # 2. evidence_classes_df: a df containing evidence with their supposed classes
    
    missing_cols <- setdiff(evidence_classes_df$evidence,
                            colnames(data))
    
    empty_matrix <- matrix(NA,
                           ncol = length(missing_cols),
                           nrow = nrow(data))
    colnames(empty_matrix) <- missing_cols
    empty_tibble <- as_tibble(empty_matrix)
    
    empty_tibble
    
  }

unnest_evidences <-
  function(data, evidence_classes_df){
    # Unnest the evidences from the EVIDENCES column
    # Each evidence will have its own column
    # Args:
    # 1. evidence_classes_df: a df containing evidence with their supposed classes
    
    test <- data %>% 
      unnest_tokens(token = "regex", 
                    output = "evidence_raw",
                    input = EVIDENCES,
                    pattern = ', ',
                    to_lower = FALSE) %>% 
      mutate(evidence_raw = str_remove_all(evidence_raw,
                                           pattern = "\\[|\\'|\\]")) %>% 
      mutate(evidence = str_extract(evidence_raw,
                                    pattern = "[_[:alnum:]]*(?=_@_)"),
             evidence_value = str_extract(evidence_raw,
                                          pattern = "(?<=_@_).*"),
             evidence_value = ifelse(is.na(evidence_value),
                                     1,
                                     evidence_value),
             evidence = if_else(is.na(evidence),
                                evidence_raw,
                                evidence)) %>% 
      relocate(evidence_raw, evidence, evidence_value)
    
    
    test2 <- test %>% 
      select(-evidence_raw) %>% 
      distinct(evidence, patientId, .keep_all = TRUE) %>% #only get first example from multiple evidence variable
      pivot_wider(names_from = evidence,
                  values_from = evidence_value) 
    
    # Re-add missing evidence columns
    emp <- cook_empty_tibble(data = test2,
                             evidence_classes_df = evidence_classes_df)
    
    out <- bind_cols(test2, emp)
    
    # suppose column classes
    walk(evidence_classes_df$evidence,
         .f = function(colname){
           class(out[[colname]]) <<- 
             evidence_classes_df$class[evidence_classes_df$evidence == colname]
         }
    )
    
    out %>% 
      relocate(all_of(evidence_classes_df$evidence),
               .after = INITIAL_EVIDENCE)
    
  }

## Script 3.2 ------------------------------------------------------------

divide_data_to_batches <- function(data,rows_per_batch){
  # number of rows per batch
  batch_rows <- rows_per_batch
  # list of dataframes
  batches <- split(data, 
                   (seq(nrow(data))-1) %/% batch_rows + 1) 
  batches
}


separate_diff_diagnosis <- 
  function(data, count_diagnosis_keep = 5){
    # Create function for getting separated diagnosis
    
    # check if patientId is in the data
    if ("patientId" %in% colnames(data[[1]])){
      data <- data %>% 
        mutate(patientId = 1:nrow(.), .before = AGE)
    }
    
    separated_diag_df <- data %>% 
      select(patientId, DIFFERENTIAL_DIAGNOSIS) %>% 
      group_by(patientId, DIFFERENTIAL_DIAGNOSIS) %>% 
      # From DIFFERENTIAL_DIAGNOSIS extract diagnoses and differential score to
      # two columns namely: diagnosis and differential_score, 
      # with each diagnosis separated to different rows. 
      reframe(
        # extract diagnoses from DIFFERENTIAL_DIAGNOSIS
        diagnosis = unlist(str_extract_all(
          DIFFERENTIAL_DIAGNOSIS, 
          pattern = "(?<=\\[\\')([:graph:]|\\s)+?(?=\\'\\,)"
        )),
        # extract differential sccore from DIFFERENTIAL_DIAGNOSIS
        differential_score = unlist(str_extract_all(
          DIFFERENTIAL_DIAGNOSIS, 
          pattern = "(?<=\\,\\s)[\\d\\.e\\-]+?(?=\\])"
        ))
      ) %>% 
    # only keep the top 5 differential diagnoses per patient
    group_by(patientId) %>% 
      mutate(
        differential_score = as.numeric(differential_score),
        differential_rank = rank(-differential_score, ties.method = "min")
      ) %>% 
      filter(differential_rank <= count_diagnosis_keep) %>%
      ungroup() %>% 
      select(-DIFFERENTIAL_DIAGNOSIS)
    
    
    new_df <- data %>% 
      right_join(separated_diag_df, 
                 by = join_by("patientId")) %>% 
      relocate(diagnosis,
               differential_score, 
               differential_rank,
               .after = DIFFERENTIAL_DIAGNOSIS) %>% 
      arrange(patientId)
    
    return(new_df)
  }

separate_diff_diagnosis_optimized <- function(data, count_diagnosis_keep = 5){
  separated_diag_df <- data %>%
    mutate(diagnosis_list = str_extract_all(DIFFERENTIAL_DIAGNOSIS, "(?<=\\[\\')([:graph:]|\\s)+?(?=\\'\\,)")) %>%
    mutate(score_list = str_extract_all(DIFFERENTIAL_DIAGNOSIS, "(?<=\\,\\s)[\\d\\.e\\-]+?(?=\\])")) %>%
    rowwise() %>%
    mutate(diagnosis = list(unlist(diagnosis_list))) %>%
    mutate(differential_score = list(unlist(score_list))) %>%
    unnest_longer(col = c(diagnosis, differential_score)) %>%
    select(-diagnosis_list, -score_list) %>%
    group_by(patientId) %>%
    mutate(
      differential_score = as.numeric(differential_score),
      differential_rank = rank(-differential_score, ties.method = "min")
    ) %>%
    filter(differential_rank <= count_diagnosis_keep) %>%
    select(patientId, diagnosis, differential_score, differential_rank)
  
  new_df <- data %>%
    right_join(separated_diag_df, by = "patientId") %>%
    relocate(diagnosis, differential_score, differential_rank, .after = DIFFERENTIAL_DIAGNOSIS) %>%
    arrange(patientId)
  
  return(new_df)
}

unnest_differential <-
  function(data, count_diagnosis_keep = 5){
    
    test_optimized <- 
      separate_diff_diagnosis_optimized(
        data = data,
        count_diagnosis_keep = count_diagnosis_keep)
    
    # Computing weights
    
    # Proportion
    out <- test_optimized %>% 
      group_by(patientId) %>% 
      mutate(
        differential_case_weight = 
          differential_score/max(differential_score, 
                                 na.rm = TRUE),
        .after = differential_rank) %>% 
      ungroup()
    
    return(out)
    
  }


## Script 3.3 --------------------------------------------------------------

feature_engineering_final_touch <-
  function(data){
    # remove columns not necessary for training or prediction
    # also convert characters to factors
    
    col_remove_index <- colnames(data) %in% c("DIFFERENTIAL_DIAGNOSIS", 
                                                "differential_score",
                                                "differential_rank")
    cols_to_remove <- colnames(data)[col_remove_index]
      
    ddxplus_training_3.3 <- data %>% 
      select(-any_of(cols_to_remove))
    # convert character columns to factor
    ddxplus_training_3.3 <- ddxplus_training_3.3 %>% 
      mutate(across(.cols = where(is.character),
                    .fns = as.factor))
    
    ddxplus_training_3.3
  }


## Script 4 ----------------------------------------------------------------

treat_nas <- function(data){
  out <- data %>% 
    # convert factor NAs to "NA"
    mutate(across(where(is.factor), 
                  .fns = ~if_else(is.na(.), "NA", .))) %>% 
    # convert numeric NAs to 0
    mutate(across(where(is.numeric), 
                  .fns = ~if_else(is.na(.), 0, .)))
  out
}


## Script 6 -------------------------------------------------------------

tidy_prediction <-
  function(test_data_prob_predict, 
           test_data = NULL){
    # get the tidy differential diagosis from probability prediction
    # Args:
    # 1. test_data: The test data set with each differential diagnosis 
    # having its own row. Also, evidences are pivoted wider.
    # 2. test_data_prob_predict: test data with probability predictions
    # Output:
    # a df with the following columns
    # metric1: Proportions of diagnoses in the predicted differential diagnoses 
    # that are in the actual differential diagnoses
    # metric2: Proportions of diagnoses in the predicted differential diagnoses
    # that are not in the actual differential diagnoses
    
    if (!is.null(test_data)){
      pred_df <- test_data_prob_predict %>% 
        mutate(patientId = test_data$patientId) %>% 
        relocate(patientId) %>% 
        slice_head(n = 1, by = patientId) # only get 1 sample per patient
    } else {
      pred_df <- test_data_prob_predict
    }
    
    
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
    
    pred_df_longer
    
  }

gather_differential_metrics <-
  function(test_data, test_data_prob_predict){
    # collect the metrics of predicted probabilities
    # Args:
    # 1. test_data: The test data set with each differential diagnosis 
    # having its own row. Also, evidences are pivoted wider.
    # 2. test_data_prob_predict: test data with probability predictions
    # Output:
    # a df with the following columns
    # metric1: Proportions of diagnoses in the predicted differential diagnoses 
    # that are in the actual differential diagnoses
    # metric2: Proportions of diagnoses in the predicted differential diagnoses
    # that are not in the actual differential diagnoses
    # correlation: Pearson correlation coefficient of order/rank of probabilities
    # of the predicted differential diagnoses and the actual differential diagnoses
    
    metric1_computation_df <- 
      test_data %>% 
      select(patientId, 
             diagnosis,
             differential_case_weight) %>% 
      mutate(prob_rank = 
               rank(desc(differential_case_weight)),
             .by = patientId)
    
    if (nrow(test_data_prob_predict) == nrow(test_data)){
      pred_df <- test_data_prob_predict %>% 
        mutate(patientId = test_data$patientId) %>% 
        relocate(patientId) %>% 
        slice_head(n = 1, by = patientId) # only get 1 sample per patient
    } else {
      pred_df <- test_data_prob_predict
    }
    
    
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
    
    
    # Checking metric #1
    
  
    
    matching_diagnosis_prop <-
      function(truth, estimate){
        
        matches <- intersect(truth, estimate)
        truth_diag_counnt <- length(truth)
        
        metric <- length(matches)/truth_diag_counnt
        
        return(metric)
        
      }

    patients_testSet <- unique(test_data$patientId)
    
    require(furrr)
    plan(multisession, workers = 8)
    matching_diag_prop_metric <- future_map_dbl(patients_testSet,
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
   
     metric1_df <- tibble(patientId = patients_testSet,
           metric1 = matching_diag_prop_metric)
    
    message("Message: Done with metric 1...(1/3)")
    
    # Checking metric #2 
    
    
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
    
    metric2_df <- tibble(patientId = patients_testSet,
                         metric2 = setdiff_diag_prop_metric)
    
    message("Message: Done with metric 2...(2/3)")
    
    # Checking metric #3 
    
    truth_join_pred <- metric1_computation_df %>% 
      inner_join(pred_df_longer,
                 by = c("patientId", "diagnosis")) %>% 
      mutate(prob_rank.y = rank(desc(prob)),
             .by = patientId)
    rankCorrelation_diffDiag_metric_df <- truth_join_pred %>% 
      summarize(correlation =
                  if (length(prob_rank.x)  == 1){
                    warning("Warning: Rank correlation with only one diagnosis converted from NA to 1")
                    1
                  } else {
                    cor(prob_rank.x,
                        prob_rank.y)
                  }
                  ,
                .by = patientId)
      # filter(!is.na(correlation))
    

    metric3_df <- rankCorrelation_diffDiag_metric_df
    
    message("Message: Done with metric 3...(3/3)")
    message("Message: Tidying metrics...")
    
    # list(metric_df = metric1_df %>% 
    #        left_join(metric2_df) %>% 
    #        left_join(metric3_df),
    #      predictions = pred_df_longer)
    
    metric1_df %>% 
      left_join(metric2_df) %>% 
      left_join(metric3_df)
    
  }


## Script 7 ----------------------------------------------------------------


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



## Script 9 ----------------------------------------------------------------

dirtify_differential_helper <-
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

dirtify_differential <- 
  function(data, diagnosis, prob){
    data %>% 
      mutate(differential_diagnosis = 
               dirtify_differential_helper(diagnosis, prob),
             .by = patientId, .keep = "unused")
  }

nest_differential <-
  function(test_data_prob_predict,
           test_data = NULL){
    
    # Error handling
    if (!"patientId" %in% colnames(test_data_prob_predict)){
      stop("Error: 'patientId' column not found in data")
    }
    
    message("MESSAGE: Pivoting differential predictions...")
    tidy_pred <- tidy_prediction(
      test_data = test_data,
      test_data_prob_predict = test_data_prob_predict)
    message("MESSAGE: Pivoted differential predictions...")
    
    test <- tidy_pred %>% 
      group_by(patientId) %>% 
      mutate(prob = prob * (1/sum(prob, 
                                  na.rm = TRUE))) %>% 
      summarize(diagnosis = list(diagnosis),
                prob = list(prob))
    
    message("MESSAGE: Nesting differential predictions...")
    test %>% 
      dirtify_differential(diagnosis = diagnosis,
                           prob = prob)
    
  }

