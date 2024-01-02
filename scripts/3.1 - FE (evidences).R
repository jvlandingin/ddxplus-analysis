

# Description -------------------------------------------------------------

# The goal of this script is for feature engineering. We prepare the features
# to be used for model training. The features are the evidences. 


# Load Datasets -----------------------------------------------------------

ddxplus_training_2 <- 
  readr::read_rds(file = here("data","processed","ddxplus_training_2.Rds"))

evidences_tibble <-
  readr::read_rds(file = here(
    "data",
    "processed",
    "evidences_tibble.Rds"
  ))

conditions_tibble <- 
  readr::read_rds(file = here(
    "data",
    "processed",
    "conditions_tibble.Rds"
  ))

# Feature Engineering evidences -----------------------------------------------------


# 1. Binary evidences

binary_evidences_filter <- evidences_tibble %>% 
  filter(data_type == "B")



get_evidence_unbind <- function(data, evidence_names){
  map(
    binary_evidences_filter$name,
    .f = function(name_of_evidence) {
      new_dat <- data %>% 
        select(EVIDENCES)
      data %>%
        # limit columns to decrease runtime
        select(patientId, EVIDENCES) %>% 
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
        select(
          {name_of_evidence}
          )
    }, .progress = TRUE
  )
}
# The output of `binary_evidences_unbind` is a list of dataframes



# divide the dataframe to batches
# how many rows if divided by p?
p <- 50
nrow(ddxplus_training_2)/p

batches <- divide_data_to_batches(ddxplus_training_2, 
                       rows_per_batch = 10000)
rm(p)

plan(multisession, workers = 12)

binary_evidences_batches <- 
  map(batches[1:2],
             .f = function(batch){
               evidence_cols <- get_evidence_unbind(
                 data = batch,
                 evidence_names = binary_evidences_filter$name
               )
               bind_cols(patientId = batch[["patientId"]], 
                         evidence_cols)
             },
             .progress = TRUE)


binary_evidences <- bind_rows(binary_evidences_batches)
binary_evidences

# readr::write_rds(x = binary_evidences,
#                  file = here("data","processed","binary_evidences.Rds"))

binary_evidences <- readr::read_rds(file = here("data","processed","binary_evidences.Rds"))

rm(binary_evidences_batches, binary_evidences_filter, evidences_tibble)

# 2. Multiple categories evidences

nonbinary_evidences_filter <- 
  evidences_tibble %>% 
  filter(data_type != "B")

nonbinary_evidence_names <- nonbinary_evidences_filter$name


plan(multisession, workers = 12)

non_binary_get_evidence_unbind <- function(data, evidence_names){
  furrr::future_map(
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

# test
evidence_cols <- non_binary_get_evidence_unbind(
  data = batches[[1]],
  evidence_names = nonbinary_evidence_names
)

nonbinary_evidences_batches <- 
  furrr::future_map(batches[1],
                    .f = function(batch){
                      evidence_cols <- non_binary_get_evidence_unbind(
                        data = batch,
                        evidence_names = nonbinary_evidence_names
                      )
                      bind_cols(patientId = batch[["patientId"]], 
                                evidence_cols)
                    },
                    .progress = TRUE)

nonbinary_evidences <- 
  bind_rows(nonbinary_evidences_batches)


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


## 1.1. Combine and Save results ----------------------------------------------

# ddxplus_training_2 <-
#   readr::read_rds(file = here("data","processed","ddxplus_training_2.Rds"))
# binary_evidences <- 
#   readr::read_rds(file = here("data","processed","binary_evidences.Rds"))
# nonbinary_evidences <-
#   readr::read_rds(file = here("data","processed","nonbinary_evidences.Rds"))

ddxplus_training_3.1_raw <-
  ddxplus_training_2 %>% 
  left_join(binary_evidences,
            by = join_by(patientId)) %>% 
  left_join(nonbinary_evidences)

ddxplus_training_3.1 <- 
  ddxplus_training_3.1_raw %>% 
  select(-EVIDENCES, -PATHOLOGY)


readr::write_rds(x = ddxplus_training_3.1, 
                 file = here("data","processed","ddxplus_training_3.1.Rds"))
readr::write_rds(x = ddxplus_training_3.1_stratified, 
                 file = here("data","processed","ddxplus_training_3.1_stratified.Rds"))

rm(binary_evidences, nonbinary_evidences, ddxplus_training_3.1_raw)
rm(evidence_cols)
rm(ddxplus_training_2)
rm(nonbinary_evidences)
rm(nonbinary_evidences_filter, 
   nonbinary_evidences_batches,
   batches,
   conditions_tibble,
   nonbinary_evidence_names)
