library(here)
library(tidyverse)
library(furrr)

# Description -------------------------------------------------------------

# The goal of this script is for feature engineering.
# The response variable is prepared for training. 
# Specifically, the differential diagnosis column which contains multiple
# conditions. Thus, we divide those conditi %>% %>% ons to multiple rows.



# Load datasets -----------------------------------------------------------

ddxplus_training_3.1 <- readr::read_rds(
  file = here("data","processed","ddxplus_training_3.1.Rds")
)

# Differential diagnosis treatment ----------------------------------------


# training_data_sample <- 
#   training_data %>% 
#   slice_sample(n = 10000)
# 
# 
# test <- training_data_sample %>% 
#   mutate(patientId = 1:nrow(.)) %>% 
#   select(patientId, DIFFERENTIAL_DIAGNOSIS) %>% 
#   group_by(patientId, DIFFERENTIAL_DIAGNOSIS) %>% 
#   # From DIFFERENTIAL_DIAGNOSIS extract diagnoses and differential score to
#   # two columns namely: diagnosis and differential_score, 
#   # with each diagnosis separated to different rows. 
#   reframe(
#     # extract diagnoses from DIFFERENTIAL_DIAGNOSIS
#     diagnosis = unlist(str_extract_all(
#       DIFFERENTIAL_DIAGNOSIS, 
#       pattern = "(?<=\\[\\')([:graph:]|\\s)+?(?=\\'\\,)"
#     )),
#     # extract differential sccore from DIFFERENTIAL_DIAGNOSIS
#     differential_score = unlist(str_extract_all(
#       DIFFERENTIAL_DIAGNOSIS, 
#       pattern = "(?<=\\,\\s)[\\d\\.]+?(?=\\])"
#     ))
#   ) %>% 
#   # --------------------------------------
# # only keep the top 5 differential diagnoses per patient
# group_by(DIFFERENTIAL_DIAGNOSIS) %>% 
#   mutate(
#     differential_score = as.numeric(differential_score),
#     differential_rank = rank(-differential_score, ties.method = "min")
#   ) %>% 
#   filter(differential_rank <= 5) %>% 
#   ungroup()
# test
# 
# sample_training <- training_data_sample %>% 
#   mutate(patientId = 1:nrow(.)) %>% 
#   left_join(test) %>% 
#   relocate(diagnosis,
#            differential_score, 
#            differential_rank,
#            .after = DIFFERENTIAL_DIAGNOSIS) %>% 
#   arrange(patientId) %>% 
#   print(n = 100)

# readr::write_csv(sample_training, 
#                  file = "sample_training.csv")


# Create function for getting separated diagnosis -------------------------

separate_diff_diagnosis <- 
  function(data, count_diagnosis_keep = 5){
    
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
      # --------------------------------------
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

# separate_diff_diagnosis(training_data_sample)


# Create the final file ---------------------------------------------------

# divide the dataframe to batches
# 100 iterations
rows_per_batch <- nrow(ddxplus_training_3.1)/100

batches <- divide_data_to_batches(ddxplus_training_3.1[1:10000,], 
                                  rows_per_batch = rows_per_batch)

# setup multiprocessing
plan(multisession, workers = 12)
processed_diag_batches <- future_imap(batches, 
                                      .f = function(batch, idx){
                                        idx <- as.numeric(idx) + 1
                                        separate_diff_diagnosis_possibly <- 
                                          possibly(.f = separate_diff_diagnosis, 
                                                   otherwise = NA,
                                                   quiet = FALSE)
                                        
                                        output <- separate_diff_diagnosis_possibly(batch)
                                        readr::write_rds(output,
                                                         file = here("data",
                                                                     "processed",
                                                                     "processed_diffDiag_batches",
                                                                     paste0("sep_diffDiag_batch",idx,".Rds")))
                                        output
                                      },
                                      .progress = TRUE)

length(processed_diag_batches)


processed_diff_diagnosis <- bind_rows(processed_diag_batches)


# ddxplus_training_3.2 <- 
#   readr::read_rds(
#     file = here("data",
#                 "processed",
#                 "ddxplus_training_3.2.Rds")
#   )

rm(processed_diag_batches, batches, ddxplus_training_3.1, processed_diff_diagnosis)


# Computing weights

# Proportion
ddxplus_training_3.2 <- processed_diff_diagnosis %>% 
  group_by(patientId) %>% 
  mutate(differential_case_weight = differential_score/max(differential_score),
         .after = differential_rank) %>% 
  ungroup()

readr::write_rds(ddxplus_training_3.2, 
                 file = here("data",
                             "processed",
                             "ddxplus_training_3.2.Rds"))
