# Code/visuals/plot_predictions.R

plot_postseason_predictions <- function(predictions_path, actuals_path,
                                        output_plot_path, output_calibration_path) {
  
  # --- Load data ---
  predictions <- read_csv(predictions_path, show_col_types = FALSE)
  actuals <- read_csv(actuals_path, show_col_types = FALSE)
  
  # --- Join predictions and actuals ---
  joined <- predictions %>%
    inner_join(actuals, by = c("team", "opponent")) %>%
    mutate(
      matchup = paste(team, "vs", opponent),
      correct_prediction = ifelse(
        (predicted_win_prob >= 0.5 & winner == 1) |
          (predicted_win_prob < 0.5 & winner == 0),
        TRUE, FALSE
      )
    )
  
  # --- Accuracy Summary ---
  accuracy <- mean(joined$correct_prediction)
  cat("\n✅ Model Accuracy:", round(accuracy * 100, 2), "%\n")
  
  # --- Matchup Plot ---
  matchup_plot <- ggplot(joined, aes(
    x = reorder(matchup, predicted_win_prob),
    y = predicted_win_prob,
    fill = correct_prediction
  )) +
    geom_col(width = 0.8) +
    geom_text(aes(label = scales::percent(predicted_win_prob, accuracy = 1)),
              hjust = -0.1, size = 3.8, color = "black") +
    coord_flip() +
    scale_y_continuous(labels = percent_format(accuracy = 1), limits = c(0, 1.1)) +
    scale_fill_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#B22222")) +
    labs(
      title = "2023 NBA First Round Matchup Predictions",
      subtitle = paste0("Series-level Accuracy: ", round(accuracy * 100, 1), "%"),
      x = "Matchup",
      y = "Predicted Win Probability",
      fill = "Prediction Correct?"
    ) +
    theme_minimal(base_size = 13) +
    theme(panel.grid.major.y = element_blank())
  
  ggsave(output_plot_path, matchup_plot, width = 10, height = 6, bg = "white")
  cat("📊 Matchup plot saved to:", output_plot_path, "\n")
  
  # --- Calibration Plot ---
  confidence_bins <- joined %>%
    mutate(conf_bin = cut(predicted_win_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
    group_by(conf_bin) %>%
    summarise(n = n(), accuracy = mean(correct_prediction), .groups = "drop")
  
  calibration_plot <- ggplot(confidence_bins, aes(x = conf_bin, y = accuracy)) +
    geom_col(fill = "#4682B4", width = 0.8) +
    geom_text(aes(label = paste0(round(accuracy * 100), "%")), vjust = -0.5, size = 3.5) +
    scale_y_continuous(limits = c(0, 1), labels = percent_format(accuracy = 1)) +
    labs(
      title = "Prediction Accuracy by Confidence Level",
      subtitle = "Bands are grouped predicted win probability ranges",
      x = "Predicted Win Probability Band",
      y = "Actual Accuracy"
    ) +
    theme_minimal(base_size = 13)
  
  ggsave(output_calibration_path, calibration_plot, width = 8, height = 5, bg = "white")
  cat("📈 Calibration plot saved to:", output_calibration_path, "\n")
}







# # --- Load libraries ---
# library(readr)
# library(dplyr)
# library(ggplot2)
# library(here)
# library(scales)
# 
# # --- Define paths using 'here()' ---
# predictions_path <- here("Results", "predictions.csv")
# actuals_path <- here("Data", "actual_results_2023.csv")
# output_plot_path <- here("Visualizations", "2023_playoff_predictions.png")
# output_calibration_path <- here("Visualizations", "2023_confidence_accuracy.png")
# 
# # --- Load data ---
# predictions <- read_csv(predictions_path)
# actuals <- read_csv(actuals_path)
# 
# # --- Join predictions and actuals ---
# joined <- predictions %>%
#   inner_join(actuals, by = c("team", "opponent")) %>%
#   mutate(
#     matchup = paste(team, "vs", opponent),
#     correct_prediction = ifelse(
#       (predicted_win_prob >= 0.5 & winner == 1) |
#         (predicted_win_prob < 0.5 & winner == 0),
#       TRUE, FALSE
#     )
#   )
# 
# # --- Print overall accuracy ---
# accuracy <- mean(joined$correct_prediction)
# cat("\n✅ Model Accuracy:", round(accuracy * 100, 2), "%\n")
# 
# # === 🔹 PLOT 1: Matchup Bar Plot ===
# matchup_plot <- ggplot(joined, aes(
#   x = reorder(matchup, predicted_win_prob),
#   y = predicted_win_prob,
#   fill = correct_prediction
# )) +
#   geom_col(width = 0.8) +
#   geom_text(aes(label = scales::percent(predicted_win_prob, accuracy = 1)),
#             hjust = -0.1, size = 3.8, color = "black") +
#   coord_flip() +
#   scale_y_continuous(labels = percent_format(accuracy = 1)) +
#   scale_fill_manual(values = c("TRUE" = "#2E8B57", "FALSE" = "#B22222")) +
#   labs(
#     title = "2023 NBA First Round Matchup Predictions",
#     subtitle = paste0("Series-level Accuracy: ", round(accuracy * 100, 1), "%"),
#     x = "Matchup",
#     y = "Predicted Win Probability",
#     fill = "Prediction Correct?"
#   ) +
#   theme_minimal(base_size = 13) +
#   theme(panel.grid.major.y = element_blank())
# 
# # --- Save plot ---
# ggsave(output_plot_path, matchup_plot, width = 10, height = 6)
# cat("📊 Matchup plot saved to:", output_plot_path, "\n")
# 
# # === 🔹 PLOT 2: Confidence vs Accuracy (Calibration) ===
# confidence_bins <- joined %>%
#   mutate(conf_bin = cut(predicted_win_prob, breaks = seq(0, 1, by = 0.1), include.lowest = TRUE)) %>%
#   group_by(conf_bin) %>%
#   summarise(
#     n = n(),
#     accuracy = mean(correct_prediction),
#     .groups = "drop"
#   )
# 
# calibration_plot <- ggplot(confidence_bins, aes(x = conf_bin, y = accuracy)) +
#   geom_col(fill = "#4682B4", width = 0.8) +
#   geom_text(aes(label = paste0(round(accuracy * 100), "%")), vjust = -0.5, size = 3.5) +
#   scale_y_continuous(limits = c(0, 1), labels = percent_format(accuracy = 1)) +
#   labs(
#     title = "Prediction Accuracy by Confidence Level",
#     subtitle = "How well the model performs across win probability bands",
#     x = "Predicted Win Probability Band",
#     y = "Actual Accuracy"
#   ) +
#   theme_minimal(base_size = 13)
# 
# ggsave(output_calibration_path, calibration_plot, width = 8, height = 5)
# cat("📈 Calibration plot saved to:", output_calibration_path, "\n")
