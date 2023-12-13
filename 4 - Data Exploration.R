
training_data %>% 
  count(PATHOLOGY, sort = T) %>% 
  slice_head(n = 10) %>% 
  ggplot(aes(x = fct_reorder(PATHOLOGY,n), n)) +
  geom_col() +
  coord_flip()

glimpse(training_data)
