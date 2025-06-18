# generate_h2h_metrics.R

library(dplyr)

# Import H2H metric functions
source(here::here("Code", "data_processing", "metrics", "h2h_metrics.R"))

generate_h2h_metrics <- function(data, gametype_filter) {
  data_filtered <- filter(data, gametype == gametype_filter)
  
  h2h_combined <- calculate_h2h_wins(data_filtered) %>%
    left_join(calculate_h2h_win_pct(data_filtered), by = c("season", "off_team", "def_team")) %>%
    left_join(calculate_h2h_avg_mov(data_filtered), by = c("season", "off_team", "def_team")) %>%
    left_join(calculate_h2h_net_rating(data_filtered), by = c("season", "off_team", "def_team")) %>%
    left_join(calculate_h2h_rebounding(data_filtered), by = c("season", "off_team", "def_team"))
  
  return(h2h_combined)
}

generate_postseason_h2h_rolling <- function(data) {
  data <- calculate_opp_points_and_def_possessions(data)
  data_postseason <- data %>% filter(gametype == 4)
  seasons <- sort(unique(data_postseason$season))
  
  if (!2023 %in% seasons) seasons <- c(seasons, 2023)
  
  all_h2h_rolling <- purrr::map_dfr(
    seasons,
    ~ get_playoff_h2h_up_to_season(data_postseason, cutoff_season = .x)
  ) %>%
    arrange(season, off_team, def_team)
  
  return(all_h2h_rolling)
}