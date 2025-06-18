train_final_model <- function(data, selected_features, response_var = "won_series", train_prop = 0.8, seed = 123) {
  library(caret)
  
  # Create formula
  formula <- as.formula(paste(response_var, "~", paste(selected_features, collapse = " + ")))
  
  # Train/test split
  set.seed(seed)
  train_index <- createDataPartition(data[[response_var]], p = train_prop, list = FALSE)
  train_set <- data[train_index, ]
  test_set  <- data[-train_index, ]
  
  # Fit model
  model <- glm(formula, data = train_set, family = binomial)
  
  # Output
  list(
    model = model,
    train_set = train_set,
    test_set = test_set,
    formula = formula
  )
}
