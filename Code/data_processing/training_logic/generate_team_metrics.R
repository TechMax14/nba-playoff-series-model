# generate_team_metrics.R

library(dplyr)

# Import metric calculation functions
source(here::here("Code", "data_processing", "metrics", "base_metrics.R"))
source(here::here("Code", "data_processing", "metrics", "advanced_metrics.R"))
source(here::here("Code", "data_processing", "metrics", "playoff_metrics.R"))
source(here::here("Code", "config", "constants.R"))

generate_team_metrics <- function(data, gametype_filter) {
  is_postseason <- gametype_filter == 4
  data_filtered <- filter(data, gametype == gametype_filter)
  data_filtered <- calculate_opp_points_and_def_possessions(data_filtered)
  
  # Core team metrics
  win_pct     <- calculate_win_percentage(data_filtered)
  home_adv    <- calculate_home_advantage(data_filtered)
  point_diff  <- calculate_point_differential(data_filtered)
  net_rtg     <- calculate_ratings(data_filtered)
  adv_metrics <- calculate_advanced_metrics(data_filtered)
  momentum    <- calculate_momentum(data_filtered)
  sos         <- calculate_sos(data_filtered, win_pct)
  
  metrics <- win_pct %>%
    left_join(home_adv, by = c("season", "off_team")) %>%
    left_join(point_diff, by = c("season", "off_team")) %>%
    left_join(net_rtg, by = c("season", "off_team")) %>%
    left_join(adv_metrics, by = c("season", "off_team")) %>%
    left_join(momentum, by = c("season", "off_team")) %>%
    left_join(sos, by = c("season", "off_team")) %>%
    left_join(conference_map, by = "off_team") %>%
    rank_teams_by_conference()
  
  # Add postseason-specific historical features
  if (is_postseason) {
    clutch <- calculate_clutch_performance(data_filtered)
    metrics <- metrics %>%
      left_join(clutch, by = c("season", "off_team")) %>%
      add_playoff_experience_and_clean() %>%
      left_join(calculate_playoff_appearances_last_3(.), by = c("season", "off_team")) %>%
      left_join(
        calculate_last_playoff_appearance(.) %>%
          select(season, off_team, last_playoff_season, seasons_since_last_playoff),
        by = c("season", "off_team")
      )
    
    # Lag postseason columns
    lag_cols <- setdiff(
      colnames(metrics),
      c("season", "off_team", "Conference", "last_playoff_season", "seasons_since_last_playoff")
    )
    
    metrics <- metrics %>%
      arrange(off_team, season) %>%
      group_by(off_team) %>%
      mutate(across(all_of(lag_cols), lag)) %>%
      ungroup() %>%
      rename_with(~ paste0("last_PS_", .), .cols = all_of(lag_cols))
  }
  
  return(metrics)
}



# # Load all metric functions using robust project-root-relative paths
# source(here::here("Code", "metrics", "base_metrics.R"))
# source(here::here("Code", "metrics", "advanced_metrics.R"))
# source(here::here("Code", "metrics", "h2h_metrics.R"))
# source(here::here("Code", "metrics", "playoff_metrics.R"))
# source(here::here("Code", "metrics", "constants.R"))
# 
# #----------------------------------------------------------
# # TEAM METRICS
# #----------------------------------------------------------
# 
# generate_team_metrics <- function(data, gametype_filter) {
#   is_postseason <- gametype_filter == 4
#   data_filtered <- filter(data, gametype == gametype_filter)
#   data_filtered <- calculate_opp_points_and_def_possessions(data_filtered)
#   
#   # Core team metrics
#   win_pct     <- calculate_win_percentage(data_filtered)
#   home_adv    <- calculate_home_advantage(data_filtered)
#   point_diff  <- calculate_point_differential(data_filtered)
#   net_rtg     <- calculate_ratings(data_filtered)
#   adv_metrics <- calculate_advanced_metrics(data_filtered)
#   momentum    <- calculate_momentum(data_filtered)
#   sos         <- calculate_sos(data_filtered, win_pct)
#   
#   metrics <- win_pct %>%
#     left_join(home_adv, by = c("season", "off_team")) %>%
#     left_join(point_diff, by = c("season", "off_team")) %>%
#     left_join(net_rtg, by = c("season", "off_team")) %>%
#     left_join(adv_metrics, by = c("season", "off_team")) %>%
#     left_join(momentum, by = c("season", "off_team")) %>%
#     left_join(sos, by = c("season", "off_team")) %>%
#     left_join(conference_map, by = "off_team") %>%
#     rank_teams_by_conference()
#   
#   # Add postseason-specific historical features
#   if (is_postseason) {
#     clutch <- calculate_clutch_performance(data_filtered)
#     metrics <- metrics %>%
#       left_join(clutch, by = c("season", "off_team")) %>%
#       add_playoff_experience_and_clean() %>%
#       left_join(calculate_playoff_appearances_last_3(.), by = c("season", "off_team")) %>%
#       left_join(
#         calculate_last_playoff_appearance(.) %>%
#           select(season, off_team, last_playoff_season, seasons_since_last_playoff),
#         by = c("season", "off_team")
#       )
#     
#     # ✅ Dynamically identify columns to lag
#     lag_cols <- setdiff(
#       colnames(metrics),
#       c("season", "off_team", "Conference", "last_playoff_season", "seasons_since_last_playoff")
#     )
#     
#     metrics <- metrics %>%
#       arrange(off_team, season) %>%
#       group_by(off_team) %>%
#       mutate(across(all_of(lag_cols), lag)) %>%
#       ungroup() %>%
#       rename_with(~ paste0("last_PS_", .), .cols = all_of(lag_cols))
#   }
#   
#   return(metrics)
# }
# 
# #----------------------------------------------------------
# # HEAD-TO-HEAD METRICS
# #----------------------------------------------------------
# 
# generate_h2h_metrics <- function(data, gametype_filter) {
#   data_filtered <- filter(data, gametype == gametype_filter)
#   
#   h2h_combined <- calculate_h2h_wins(data_filtered) %>%
#     left_join(calculate_h2h_win_pct(data_filtered), by = c("season", "off_team", "def_team")) %>%
#     left_join(calculate_h2h_avg_mov(data_filtered), by = c("season", "off_team", "def_team")) %>%
#     left_join(calculate_h2h_net_rating(data_filtered), by = c("season", "off_team", "def_team")) %>%
#     left_join(calculate_h2h_rebounding(data_filtered), by = c("season", "off_team", "def_team"))
#   
#   return(h2h_combined)
# }
# 
# generate_postseason_h2h_rolling <- function(data) {
#   data <- calculate_opp_points_and_def_possessions(data)
#   data_postseason <- data %>% filter(gametype == 4)
#   seasons <- sort(unique(data_postseason$season))
#   
#   if (!2023 %in% seasons) seasons <- c(seasons, 2023)
#   
#   all_h2h_rolling <- purrr::map_dfr(
#     seasons,
#     ~ get_playoff_h2h_up_to_season(data_postseason, cutoff_season = .x)
#   ) %>%
#     arrange(season, off_team, def_team)
#   
#   return(all_h2h_rolling)
# }
# 
# #----------------------------------------------------------
# # SERIES OUTCOME CALCULATION
# #----------------------------------------------------------
# 
# generate_playoff_series_outcomes <- function(team_data) {
#   team_data <- team_data %>%
#     filter(gametype == 4)
#   
#   games_unique <- team_data %>%
#     group_by(season, nbagameid) %>%    # Ensuring each game is unique
#     slice(1) %>%
#     ungroup() %>%
#     mutate(
#       winner = ifelse(off_win == 1, off_team, def_team),
#       loser  = ifelse(off_win == 1, def_team, off_team)
#     )
#   
#   series_results <- games_unique %>%
#     group_by(season, winner, loser) %>%
#     summarise(wins = n(), .groups = 'drop')
#   
#   series_summary <- full_join(
#     series_results %>%
#       rename(team = winner, opponent = loser),
#     series_results %>%
#       rename(team = loser, opponent = winner, losses = wins),
#     by = c("season", "team", "opponent")
#   ) %>%
#     mutate(
#       wins = ifelse(is.na(wins), 0, wins),
#       losses = ifelse(is.na(losses), 0, losses),
#       total_games = wins + losses,
#       won_series = ifelse(wins > losses, 1, 0)
#     ) %>%
#     filter(total_games > 0) %>%
#     rename(series_wins = wins, series_losses = losses, total_games_in_series = total_games)
#   
#   return(series_summary)
# }
# 
# #----------------------------------------------------------
# # FINAL MERGE
# #----------------------------------------------------------
# 
# merge_and_clean_data <- function(series_outcomes,
#                                  reg_szn_team_data,
#                                  post_szn_team_data,
#                                  reg_szn_h2h_data,
#                                  post_szn_h2h_data) {
#   reg_szn_h2h_data <- reg_szn_h2h_data %>%
#     rename(team = off_team, opponent = def_team)
#   post_szn_h2h_data <- post_szn_h2h_data %>%
#     rename(team = off_team, opponent = def_team)
#   reg_szn_team_data <- reg_szn_team_data %>%
#     rename(team = off_team)
#   post_szn_team_data <- post_szn_team_data %>%
#     rename(team = off_team)
#   
#   combined_data <- series_outcomes %>%
#     left_join(reg_szn_h2h_data %>%
#                 rename_with(~ paste0("RS_", .), -c(season, team, opponent)),
#               by = c("season", "team", "opponent")) %>%
#     left_join(post_szn_h2h_data %>%
#                 rename_with(~ paste0("PS_", .), -c(season, team, opponent)),
#               by = c("season", "team", "opponent")) %>%
#     left_join(reg_szn_team_data %>%
#                 rename_with(~ paste0("RS_", .), -c(season, team, Conference)),
#               by = c("season", "team")) %>%
#     left_join(post_szn_team_data,
#               by = c("season", "team"))
#   
#   # Filter: must have regular season data
#   combined_data <- combined_data %>%
#     filter(if_all(starts_with("RS_"), ~ !is.na(.)))
#   
#   # Impute postseason values with 0 where missing
#   combined_data <- combined_data %>%
#     mutate(across(
#       contains("PS_") | contains("last_"),
#       ~ replace_na(., 0)
#     ))
#   
#   combined_data <- combined_data %>%
#     select(-c(series_wins, series_losses, total_games_in_series)) %>%
#     arrange(season, team)
#   
#   return(combined_data)
# }
# 
# #----------------------------------------------------------
# # FINAL TRAINING DATASET PREP
# #----------------------------------------------------------
# 
# generate_training_data_and_IDs <- function(combined_data_clean) {
#   id_columns <- combined_data_clean %>%
#     select(season, team, opponent)
#   
#   training_data <- combined_data_clean %>%
#     select(-season, -team, -opponent)
#   
#   numeric_columns <- training_data %>%
#     select(where(is.numeric)) %>%
#     select(-won_series) %>%
#     colnames()
#   
#   training_data_standardized <- training_data %>%
#     mutate(across(all_of(numeric_columns), ~ as.numeric(scale(.))))
#   
#   return(list(
#     training_data = training_data_standardized,
#     id_columns = id_columns
#   ))
# }
