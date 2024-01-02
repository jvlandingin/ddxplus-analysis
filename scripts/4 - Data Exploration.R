
# training_data %>% 
#   count(PATHOLOGY, sort = T) %>% 
#   slice_head(n = 10) %>% 
#   ggplot(aes(x = fct_reorder(PATHOLOGY,n), n)) +
#   geom_col() +
#   coord_flip()
# 
# glimpse(training_data)
# names(training_data)
# 
# 
# training_data_sample <- training_data %>% 
#   slice_sample(n = 100)
# skimmed_data_sample <- skimr::skim(training_data_sample)
# skimmed_data_sample

# summary(training_data)

ddxplus_training_3.3 <- readr::read_rds(
  file = here("data", 
              "processed",
              "ddxplus_training_3.3.Rds")
)

# skimmed_data <- skimr::skim(ddxplus_training_3.3)
# skimmed_data

# readr::write_rds(x = skimmed_data, 
#                  file = here("data","processed","data_skim.Rds"))
# skimmed_data <- readr::read_rds(file =
#                                   here("data",
#                                        "processed",
#                                        "data_skim.Rds"))

rm(skimmed_data)

ddxplus_training_3.3 %>% 
  count(diagnosis, sort = TRUE) %>% 
  print(n = 100)



# 1. Study missing values ----------------------------------------------------


ddxplus_training_3.3


skimmed_data
# Numeric NAs
evidences_tibble %>% 
  filter(name %in% 
           c("douleurxx_soudain",
             "douleurxx_intens",
             "douleurxx_precis",
             "lesions_peau_intens",
             "lesions_peau_elevee",
             "lesions_peau_prurit")) %>% 
  select(question_en)

# String NAs
evidences_tibble %>% 
  filter(name %in% 
           c("douleurxx_endroitducorps",
             "douleurxx_irrad",
             "douleurxx_carac",
             "lesions_peau_endroitducorps",
             "lesions_peau_couleur",
             "lesions_peau_plusqu1cm",
             "lesions_peau_desquame",
             "oedeme_endroitducorps")) %>% 
  select(question_en)



# It seems that it is safe to convert all factors NAs to "NA" and 
# numeric NAs to 0.


ddxplus_training_4 <- ddxplus_training_3.3 %>% 
  # convert factor NAs to "NA"
  mutate(across(where(is.factor), 
                .fns = ~if_else(is.na(.), "NA", .))) %>% 
  # convert numeric NAs to 0
  mutate(across(where(is.numeric), 
                .fns = ~if_else(is.na(.), 0, .)))

map_chr(ddxplus_training_4, class)
unique(ddxplus_training_4$douleurxx_endroitducorps)




skim_ddxplus4 <- skimr::skim(ddxplus_training_4)
skim_ddxplus4

readr::write_rds(x = skim_ddxplus4, 
                 file = here("data","processed","skim_ddxplus4.Rds"))

skim_ddxplus4 <- readr::read_rds(file = 
                                  here("data",
                                       "processed",
                                       "skim_ddxplus4.Rds"))


readr::write_rds(
  ddxplus_training_4,
  file = here("data", 
              "processed",
              "ddxplus_training_4.Rds")
)

ddxplus_training_4 <- 
  readr::read_rds(
  file = here("data", 
              "processed",
              "ddxplus_training_4.Rds")
)


# 2. Why is it important to take into account differential diagnosis ---------

## Case #1: Other diagnoses can be just as probable as the 
# highest probability diagnosis

# how close are probabilities per group
ddxplus_training_4

explore <- ddxplus_training_4 %>% 
  filter(differential_case_weight < 1) %>% 
  select(patientId, diagnosis, differential_case_weight)

set.seed(2013)
sample_patients <- 
  sample(unique(explore$patientId), size = 100000)

explore2 <- explore %>% 
  filter(patientId %in% sample_patients)


library(rethinking)

hpdi_diffProb <-
  rethinking::HPDI(samples = explore2$differential_case_weight)

explore2 %>% 
  summarize(across(differential_case_weight,
                   .fns = list(Mean = mean,
                               Median = median,
                               SD = sd),
                   .names = "{.fn}")) %>% 
  mutate(`95% lower HPDI` = hpdi_diffProb[1],
         `95% upper HPDI` = hpdi_diffProb[2])

explore2 %>% 
  ggplot(aes(differential_case_weight)) +
  geom_histogram(color = "black", ) +
  geom_vline(xintercept = hpdi_diffProb[1]) + 
  geom_vline(xintercept = hpdi_diffProb[2]) + 
  xlab("Probability Proportion")


# The x-axis shows the distribution of the proportion of probability 
# of other diagnoses in the differential diagnosis on the most probable
# diagnosis

# For example

# ddxplus_training_3.1 <- 
#   readr::read_rds(
#     file = here("data", 
#                 "processed",
#                 "ddxplus_training_3.1.Rds")
#   )

ddxplus_training_3.1$DIFFERENTIAL_DIAGNOSIS[1]
# All the values here add up to 1, which represents the probabilities 
# of each diagnosis.
# Thus, proportions were computed to compare the other diagnosis to the top diagnosis.
# For example, for Pneumonia, the probability is 0.1758.
# The top diagnosis, Bronchitis is 0.1917. 
# Thus, the probability proportion is 
0.1758/0.1917
# The closer it is to one, the closer its probability to the top diagnosis.
# Thus, based on the figure, this makes it difficult to simply choose the diagnosis 
# with the highest probability because the other possible diagnosis
# can be close to 1.


## Case #2:The probability of the other diagnoses might not be very close to 
## the highest probability diagnosis, however, these conditions might have
## high severity which makes it important that doctors are aware of these.
## and rule out these conditions first.

evidences_tibble
conditions_tibble

explore_severity <- ddxplus_training_4 %>% 
  select(patientId, diagnosis, differential_case_weight) %>% 
  left_join(conditions_tibble,
            by = join_by(diagnosis == cond_name_eng))

summary(explore_severity$severity)

# distribution of severity for top 1 diagnosis
explore_severity %>% 
  filter(differential_case_weight == 1) %>% 
  count(severity) %>% 
  ggplot(aes(severity, n)) +
  geom_col()


# distribution of severity for non-top 1 diagnosis
explore_severity %>% 
  filter(differential_case_weight != 1) %>% 
  count(severity) %>% 
  ggplot(aes(severity, n)) +
  geom_col()

# Distribution of severity in conditions among patients' differential diagnosis
explore_severity %>% 
  mutate(top_diag = 
           if_else(differential_case_weight == 1, 1, 0),
         top_diag = as.factor(top_diag)) %>% 
  count(top_diag, severity) %>%
  mutate(prop = n/sum(n), .by = top_diag) %>% 
  ggplot(aes(severity, n, fill = top_diag)) +
  geom_col()

# severity df with rank_probability
start <- Sys.time()
explore_severity2 <- explore_severity %>% 
  select(patientId, diagnosis, differential_case_weight, severity) %>% 
  group_by(patientId) %>% 
  mutate(probability_rank = rank(desc(differential_case_weight), 
                                 ties.method = "min")) %>% 
  ungroup()
end <- Sys.time()
end - start

# Load the data.table package
library(data.table)
explore_severity_dt <- as.data.table(explore_severity)
# Group by ID and calculate the mean of the 'Value' column within each group
explore_severity_dt2 <- 
  explore_severity_dt[, probability_rank := rank(desc(differential_case_weight)), by = patientId]
explore_severity2 <- tibble(explore_severity_dt2)
  


explore_severity2 %>% 
group_by(probability_rank) %>% 
summarize(across(.cols = severity,
                 .fns = list(mean = mean,
                             median = median,
                             sd = sd,
                             `95% lower HPDI` = ~rethinking::HPDI(.)[1],
                             `95% upper HPDI` = ~rethinking::HPDI(.)[2])))

explore_severity2 %>% 
  ggplot(aes(differential_case_weight, fill = factor(severity))) +
  geom_density(alpha = 0.5)

explore_severity2 %>% 
  group_by(patientId) %>% 
  filter(any(rank_probability >= 6))

ddxplus_training_2 <- 
  readr::read_rds(
    file = here("data", 
                "processed",
                "ddxplus_training_2.Rds")
  )


ddxplus_training_2 <-
  ddxplus_training_2 %>% 
  select(patientId, PATHOLOGY) %>% 
  left_join(conditions_tibble %>% 
              select(cond_name_eng, severity),
            by = join_by(PATHOLOGY == cond_name_eng))

ddxplus_training_2 %>% 
  ggplot(aes(severity)) + 
  geom_bar()
# If we simply create a model based on the actual pathology, then
# it is possible that we predict the high severity conditions less
# because of how uncommon it is. 



# 3. Verify class types of columns --------------------------------------

# Extract evidences
evidences_tibble <- readr::read_rds(file = here("data",
                                                "processed",
                                                "evidences_tibble.Rds"))
categorical_evidences <-
  evidences_tibble$name[evidences_tibble$data_type == "C"]
multichoice_evidences <-
  evidences_tibble$name[evidences_tibble$data_type == "M"]

evidences_tibble %>% 
  filter(data_type %in% c("C")) %>% 
  select(question_en, possible_values) %>% 
  pull(possible_values)

skim_ddxplus4

map(categorical_evidences,
    .f = function(x){
      class(ddxplus_training_4[[x]])
    })

map(multichoice_evidences,
    .f = function(x){
      class(ddxplus_training_4[[x]])
    })


# Check which columns have too many levels --------------------------------

res <- map_chr(ddxplus_training_4, .f = function(df){
  out <- class(df)
  out <- out[[1]]
  out
})
names(res)
col_classes_df <- tibble(col_name = names(res),
       class = res)

character_evidence <- col_classes_df %>% 
  filter(class == "character") %>% 
  inner_join(evidences_tibble %>% 
              select(name, question_en),
            by = join_by(col_name == name))

evidences_tibble %>% 
  filter(name == "douleurxx_carac") %>% 
  pull(possible_values)

# later we will apply step_other to these columns

variable_to_check <- "oedeme_endroitducorps"
english_translation <- 
  character_evidence$question_en[
    which(
      character_evidence$col_name == variable_to_check
      )
    ]

check_other <- ddxplus_training_4 %>% 
  count(!!sym(variable_to_check), sort = TRUE) %>% 
  mutate(prop = n/sum(n)) 

prop_retain <- 0.001
check_other%>% 
  mutate({{variable_to_check}} := 
           1:length(!!sym(variable_to_check))) %>% 
  ggplot(aes(!!sym(variable_to_check), prop)) +
  geom_col() + 
  geom_hline(yintercept = prop_retain)
# how many are left
left <- sum(check_other$prop > prop_retain)

print(paste0("Kept ",left," over ",nrow(check_other), " levels in '", english_translation, "'"))

count(ddxplus_training_4, INITIAL_EVIDENCE, sort = TRUE) # to step other 0.01; 22/96 left
count(ddxplus_training_4, douleurxx_endroitducorps, sort = TRUE) # to step other 0.01; 20/86 left
count(ddxplus_training_4, douleurxx_irrad, sort = TRUE) # to step other 0.01; 8/29 left
count(ddxplus_training_4, douleurxx_carac, sort = TRUE) # to step other 0.05; 5/16 left
count(ddxplus_training_4, lesions_peau_endroitducorps, sort = TRUE) # to step other 0.01; 6/42 left
count(ddxplus_training_4, oedeme_endroitducorps, sort = TRUE) # to step other 0.001; 11/19 left

















