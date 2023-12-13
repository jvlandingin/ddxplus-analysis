

# Description -------------------------------------------------------------

# The goal of this script is for feature engineering. We prepare the features
# to be used for model training. The features are the evidences. 

# Furthermore, the response variable is also prepared for training. 
# Specifically, the differential diagnosis column which contains multiple
# conditions. Thus, we divide those conditions to multiple rows.




# 1. Feature Engineering evidences -----------------------------------------------------


# 1. Binary evidences

binary_evidences_filter <- evidences_tibble %>% 
  filter(data_type == "B")


release_train_patients_sample <- release_train_patients %>% 
  slice_head(n = 100)

sample_size <- nrow(release_train_patients)/2


# Divide the dataset to 2
train_patients_sample1 <- release_train_patients %>% 
  slice(1:512801)
train_patients_sample2 <- release_train_patients %>% 
  slice(512802:1025602)


plan(multisession, workers = 8)

get_evidence_unbind <- function(data, evidence_names){
  future_map(
    binary_evidences_filter$name,
    .f = function(name_of_evidence) {
      train_patients_sample2 %>%
        # limit columns to decrease runtime
        select(EVIDENCES) %>% 
        mutate(
          # From EVIDENCES column detect the presence of [variable] evidence
          "{name_of_evidence}" := str_detect(string = EVIDENCES,
                                             pattern = {
                                               name_of_evidence
                                             }),
          "{name_of_evidence}" := if_else(get({
            name_of_evidence
          }), 1, 0)
        ) %>%
        select({
          name_of_evidence
        })
    }
  )
}
# The output of `binary_evidences_unbind` is a list of dataframes

binary_evidences_unbind1 <- 
  get_evidence_unbind(data = train_patients_sample1,
                      evidence_names = binary_evidences_filter$name)
# Bind all evidence columns 
binary_evidences1 <- binary_evidences_unbind1 %>%
  bind_cols()

binary_evidences_unbind2 <- 
  get_evidence_unbind(data = train_patients_sample2,
                      evidence_names = binary_evidences_filter$name)
# Bind all evidence columns 
binary_evidences2 <- binary_evidences_unbind2 %>%
  bind_cols()


binary_evidences <- bind_rows(binary_evidences1, binary_evidences2)

binary_evidences

# readr::write_rds(x = binary_evidences,
#                  file = here("data","processed","binary_evidences.Rds"))

binary_evidences <- readr::read_rds(file = here("data","processed","binary_evidences.Rds"))


# 2. Multiple categories evidences

release_train_patients_sample <- release_train_patients %>% 
  slice_head(n = 100)


nonbinary_evidences_filter <- 
  evidences_tibble %>% 
  filter(data_type != "B")

nonbinary_evidence_names <- nonbinary_evidences_filter$name

test_string <- release_train_patients_sample$EVIDENCES[1]

str_extract(string = test_string,
            pattern = "(?<=douleurxx_precis_@_).+?(?=\\')")

train_patients_evidences <- 
  release_train_patients %>% 
  select(EVIDENCES) 


train_patients_evidences_sample1 <- train_patients_evidences %>% 
  slice(1:512801)
train_patients_evidences_sample2 <- train_patients_evidences %>% 
  slice(512802:1025602)


plan(multisession, workers = 10)

non_binary_get_evidence_unbind <- function(data, evidence_names){
  future_map(
    evidence_names,
    .f = function(name_of_evidence) {
      data %>%
        mutate(
          # From EVIDENCES column detect the presence of [variable] evidence
          "{name_of_evidence}" := str_extract(string = EVIDENCES,
                                              pattern = 
                                                paste0("(?<=",{name_of_evidence},"_@_).+?(?=\\')")
          )
          , .keep = "none")
    }, .progress = TRUE
  )
}

nonbinary_evidences_unbind1 <- 
  non_binary_get_evidence_unbind(data = train_patients_evidences_sample1,
                                 evidence_names = nonbinary_evidence_names)
nonbinary_evidences1 <- nonbinary_evidences_unbind1 %>%
  bind_cols()

nonbinary_evidences_unbind2 <- 
  non_binary_get_evidence_unbind(data = train_patients_evidences_sample2,
                                 evidence_names = nonbinary_evidence_names)
nonbinary_evidences2 <- nonbinary_evidences_unbind2 %>%
  bind_cols()

nonbinary_evidences <- 
  bind_rows(nonbinary_evidences1, nonbinary_evidences2)


# Convert numeric features to numeric
evidences_tibble %>% 
  filter(data_type != "B") %>% 
  pull(possible_values)
# 4,5,6,9,10,11

evidences_tibble %>% 
  filter(data_type != "B") %>% 
  pull(question_en)

numeric_features <- evidences_tibble %>% 
  filter(data_type != "B") %>% 
  slice(4,5,6,9,10,11) %>% 
  pull(name)

nonbinary_evidences <- nonbinary_evidences %>% 
  mutate(across(all_of(numeric_features), 
                .fns = as.numeric))


# readr::write_rds(x = nonbinary_evidences,
#                  file = here("data","processed","nonbinary_evidences.Rds"))

nonbinary_evidences <- readr::read_rds(file = here("data","processed","nonbinary_evidences.Rds"))


## 1.1. Combine and Save results ----------------------------------------------

release_train_patients_eng <-
  readr::read_rds(file = here("data","processed","release_train_patients_eng.Rds"))
binary_evidences <- 
  readr::read_rds(file = here("data","processed","binary_evidences.Rds"))
nonbinary_evidences <- 
  readr::read_rds(file = here("data","processed","nonbinary_evidences.Rds"))

training_data_raw <-
  release_train_patients_eng %>% 
  mutate(patientId = 1:nrow(.), .before = AGE) %>% 
  bind_cols(binary_evidences) %>% 
  bind_cols(nonbinary_evidences)

training_data <- 
  training_data_raw %>% 
  select(-EVIDENCES, -PATHOLOGY)

# Proportional Stratified Sampling
training_data_stratified <- 
  training_data_raw %>% 
  group_by(PATHOLOGY) %>% 
  slice_sample(prop = 0.05) %>% 
  ungroup() %>% 
  select(-EVIDENCES, -PATHOLOGY)

readr::write_rds(x = training_data, 
                 file = here("data","processed","training.Rds"))
readr::write_rds(x = training_data_stratified, 
                 file = here("data","processed","training_data_stratified.Rds"))

rm(release_train_patients_eng, binary_evidences, nonbinary_evidences, training_data_raw)


# 2. Differential diagnosis treatment ----------------------------------------

training_data_sample <- 
  training_data %>% 
  slice_sample(n = 100)

test <- training_data_sample %>% 
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
      pattern = "(?<=\\,\\s)[\\d\\.]+?(?=\\])"
    ))
  ) %>% 
  # --------------------------------------
  # only keep the top 5 differential diagnoses per patient
  group_by(DIFFERENTIAL_DIAGNOSIS) %>% 
  mutate(
    differential_score = as.numeric(differential_score),
    differential_rank = rank(-differential_score, ties.method = "min")
    ) %>% 
  filter(differential_rank <= 5) %>% 
  ungroup()
test

sample_training <- training_data_sample %>% 
  left_join(test) %>% 
  relocate(diagnosis,
           differential_score, 
           differential_rank,
           .after = DIFFERENTIAL_DIAGNOSIS) %>% 
  arrange(patientId) %>% 
  print(n = 100)

readr::write_csv(sample_training, 
                 file = "sample_training.csv")









