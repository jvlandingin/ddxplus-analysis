
ddxplus_training_3.2 <- readr::read_rds(
  file = here("data", 
              "processed",
              "ddxplus_training_3.2.Rds")
)



ddxplus_training_3.3 <- ddxplus_training_3.2 %>% 
  select(-DIFFERENTIAL_DIAGNOSIS, 
         -differential_score,
         -differential_rank, )

ddxplus_training_3.3 <- ddxplus_training_3.3 %>% 
  mutate(across(.cols = where(is.character),
                .fns = as.factor))

readr::write_rds(
  ddxplus_training_3.3,
  file = here("data", 
              "processed",
              "ddxplus_training_3.3.Rds")
)

rm(ddxplus_training_3.2)

