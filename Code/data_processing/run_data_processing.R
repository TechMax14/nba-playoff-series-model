# process_metrics.R

# ---- Load Raw Data ----
message("📥 Loading team game data...")
team_data <- read.csv(team_game_data_path, header = TRUE, sep = ",")

# ---- Generate Metrics ----
message("🔧 Generating team and head-to-head metrics...")
reg_szn_team_data <- generate_team_metrics(team_data, gametype_filter = 2)
post_szn_team_data <- generate_team_metrics(team_data, gametype_filter = 4)
reg_szn_h2h_data <- generate_h2h_metrics(team_data, gametype_filter = 2)
post_szn_h2h_data <- generate_postseason_h2h_rolling(team_data)
series_outcomes <- generate_playoff_series_outcomes(team_data)

# print(colnames(reg_szn_team_data))
# print(colnames(post_szn_team_data))
# print(colnames(reg_szn_h2h_data))
# print(colnames(post_szn_h2h_data))

# ---- Merge and Clean Combined Dataset ----
message("🧹 Merging and cleaning data...")
combined_cleaned_data <- merge_and_clean_data(
  series_outcomes,
  reg_szn_team_data,
  post_szn_team_data,
  reg_szn_h2h_data,
  post_szn_h2h_data
)

#print(colnames(combined_cleaned_data))


# ---- Generate Training Dataset ----
message("📚 Generating training data...")
training_output <- generate_training_data_and_IDs(combined_cleaned_data)
training_data <- training_output$training_data
id_columns <- training_output$id_columns

# ================================ #
#   2023 POSTSEASON PREDICTOR SET #
# ================================ #

message("📈 Generating postseason prediction metrics...")
post_season_predictor_metrics <- generate_postseason_features(team_data)

post_season_predictor_features <- post_season_predictor_metrics$final_features

matchups_2023 <- post_season_predictor_metrics$matchups_2023
matchups_2023 <- as_tibble(matchups_2023)

message("🔗 Building raw prediction dataset...")
matchup_features <- build_raw_prediction_dataset(reg_szn_team_data, post_season_predictor_features, reg_szn_h2h_data, post_szn_h2h_data, matchups_2023, season_year = 2023)

message("🧪 Preparing dataset for model...")
postseason_matchup_data <- prepare_dataset_for_model(matchup_features)
standardized_postseason_matchup_data <- postseason_matchup_data$standardized_data
predictor_id_columns <- postseason_matchup_data$id_columns

# print(colnames(post_szn_h2h_data))
# print(colnames(training_data))
# print(colnames(standardized_data))
#print(head(prepared))

# ============================== #
#       TRAINING MODEL STAGE     #
# ============================== #

message("🧠 Training model...")
model_output <- train_final_model(training_data, selected_features)
final_model <- model_output$model

# Optional: view model summary
summary(final_model)


# ============================== #
#     RUN MODEL ON 2023 DATA     #
# ============================== #

message("🏀 Running model on 2023 data...")
prediction_results <- run_postseason_predictions(
  final_model = final_model,
  standardized_data = standardized_postseason_matchup_data,
  predictor_id_columns = predictor_id_columns,
  selected_features = selected_features
)

# 🖨️ Final Output
cat("\n--- 2023 Playoff Predictions ---\n")
print(prediction_results %>% arrange(desc(predicted_win_prob)), n = 20)

# Export predicted data
message("🏀 Exporting model prediction results...")
write_csv(prediction_results, "Results/predictions.csv")


message("🏀 Visualizing model prediction results...")
plot_postseason_predictions(
  predictions_path = here::here("Results", "predictions.csv"),
  actuals_path = here::here("Data", "actual_results_2023.csv"),
  output_plot_path = here::here("Visualizations", "2023_playoff_predictions.png"),
  output_calibration_path = here::here("Visualizations", "2023_confidence_accuracy.png")
)
