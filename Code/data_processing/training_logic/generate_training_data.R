# generate_training_data_ids.R

library(dplyr)

generate_training_data_and_IDs <- function(combined_data_clean) {
  id_columns <- combined_data_clean %>%
    select(season, team, opponent)
  
  training_data <- combined_data_clean %>%
    select(-season, -team, -opponent)
  
  numeric_columns <- training_data %>%
    select(where(is.numeric)) %>%
    select(-won_series) %>%
    colnames()
  
  training_data_standardized <- training_data %>%
    mutate(across(all_of(numeric_columns), ~ as.numeric(scale(.))))
  
  return(list(
    training_data = training_data_standardized,
    id_columns = id_columns
  ))
}
