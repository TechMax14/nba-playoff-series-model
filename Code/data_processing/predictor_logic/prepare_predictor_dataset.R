prepare_dataset_for_model <- function(prediction_dataset) {
  # Save identity columns
  id_columns <- prediction_dataset %>%
    select(team, opponent)

  # Drop non-numeric / unwanted fields (like Conference, team/opponent)
  features_only <- prediction_dataset %>%
    select(-team, -opponent, -Conference)

  # Identify numeric columns
  numeric_columns <- features_only %>%
    select(where(is.numeric)) %>%
    colnames()

  # Standardize (mean = 0, sd = 1)
  standardized_features <- features_only %>%
    mutate(across(all_of(numeric_columns), ~ as.numeric(scale(.))))

  return(list(
    standardized_data = standardized_features,
    id_columns = id_columns
  ))
}