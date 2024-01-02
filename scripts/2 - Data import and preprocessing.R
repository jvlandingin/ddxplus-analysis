

# 1. Training dataset
ddxplus_data_location <- here("data","raw","DDXPlus")

release_train_patients_raw <- read_csv(file  = here(ddxplus_data_location,
                                                    "release_train_patients.csv"))

# Add patient Id
release_train_patients <- release_train_patients_raw %>% 
  mutate(patientId = 1:nrow(.), .before = AGE)

# readr::write_rds(release_train_patients,
#                  file = here("data","processed","release_train_patients_raw.Rds"))


rm(release_train_patients_raw)

# 2. Release evidences

evidences_json <- jsonlite::read_json(path = here(ddxplus_data_location, 
                                                  "release_evidences.json"))

evidence_names <- names(evidences_json)

evidences_questions <- map(evidence_names, 
                           .f = function(code){
                             
                             evidences_json[[code]][["question_en"]]
                             
                           })

# helper function
extract_vector <- function(col_name, 
                           json = evidences_json, 
                           code_names =  evidence_names){
  map(code_names, 
      .f = function(code){
        json[[code]][[col_name]]
      })
}

col_names <- evidences_json$fievre %>% names()

output <- map(col_names, .f = ~ extract_vector(.))

new_pos_val <- map(output[[8]], .f = unlist)
pos_values <- tibble(possible_values = new_pos_val)

evidences_tibble_raw <- tibble(name = unlist(output[[1]]), 
                               code_question = unlist(output[[2]]),
                               question_en = unlist(output[[4]]),
                               is_antecedent = unlist(output[[5]]),
                               data_type = unlist(output[[9]]))

evidences_tibble <- bind_cols(evidences_tibble_raw, pos_values)

readr::write_rds(evidences_tibble,
                 file = here("data","processed","evidences_tibble.Rds"))


rm(evidences_json, evidence_names, evidences_questions, extract_vector,
   col_names, output, new_pos_val, pos_values, evidences_tibble_raw)

# 3. Release conditions

conditions_json <- jsonlite::read_json(path = here(ddxplus_data_location, 
                                                   "release_conditions.json"))

condition_code_names <- names(conditions_json)

condition_col_names <- names(conditions_json$`Pneumothorax spontané`)

conditions_output <- map(condition_col_names, 
                         .f = ~ extract_vector(col_name = ., 
                                               json = conditions_json, 
                                               code_names = condition_code_names))

conditions_tibble <- tibble(condition_name = unlist(conditions_output[[1]]), 
                            cond_name_eng = unlist(conditions_output[[3]]),
                            `icd10-id` = unlist(conditions_output[[4]]),
                            symptoms = unlist(conditions_output[[5]]),
                            antecedents = unlist(conditions_output[[6]]),
                            severity = unlist(conditions_output[[7]]))

# readr::write_rds(x = conditions_tibble,
#                  file = here("data","processed","conditions_tibble.Rds"))

rm(conditions_json, condition_code_names, condition_col_names,
   conditions_output)


# Further processing ------------------------------------------------------

# 1. Translate pathology to english 

release_train_patients

conditions_translate_helper <- 
  select(conditions_tibble, condition_name, cond_name_eng) %>% 
  rename(PATHOLOGY = condition_name)

release_train_patients_eng <- release_train_patients %>% 
  left_join(conditions_translate_helper) %>% 
  mutate(PATHOLOGY = cond_name_eng, .keep = "unused")

# 2. Translate differential diagnosis to english

diff_diag <- release_train_patients$DIFFERENTIAL_DIAGNOSIS

plan(multisession, workers = 8)
diff_diag_eng <- future_map_chr(
  diff_diag,
  .f = function(x) {
    stringr::str_replace_all(
      x,
      setNames(
        fixed(conditions_translate_helper$cond_name_eng),
        conditions_translate_helper$PATHOLOGY
      )
    )
  }
)


release_train_patients_eng$DIFFERENTIAL_DIAGNOSIS <- diff_diag_eng

ddxplus_training_2 <- release_train_patients_eng

# 3. Add patient Id to data
# ddxplus_training_2 <- ddxplus_training_2 %>% 
#   mutate(patientId = 1:nrow(.), .before = AGE)

# readr::write_rds(x = ddxplus_training_2,
#                  file = here("data","processed","ddxplus_training_2.Rds"))

rm(diff_diag_eng,
   release_train_patients,
   conditions_translate_helper, 
   release_train_patients_eng,
   ddxplus_data_location)










