run_postseason_predictions <- function(final_model, standardized_data, predictor_id_columns, selected_features) {
  # Select only columns the model expects
  X_features <- standardized_data %>%
    select(all_of(selected_features))
  
  # Predict probabilities
  predicted_probs <- predict(final_model, newdata = X_features, type = "response")
  
  # Combine with team identifiers
  prediction_results <- predictor_id_columns %>%
    mutate(predicted_win_prob = predicted_probs)
  
  return(prediction_results)
}
