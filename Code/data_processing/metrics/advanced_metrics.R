#=========================================================
# Advanced Team Metrics Functions
#=========================================================

#---------------------------------------------------------
# 1. General Advanced Metrics
#    Includes turnover %, 3P%, FT rate, assist ratio, opp steal rate
#---------------------------------------------------------
calculate_advanced_metrics <- function(data) {
  data %>%
    group_by(season, off_team) %>%
    summarise(
      total_turnovers = sum(turnovers, na.rm = TRUE),
      total_possessions = sum(possessions, na.rm = TRUE),
      total_fg3made = sum(fg3made, na.rm = TRUE),
      total_fg3attempted = sum(fg3attempted, na.rm = TRUE),
      total_ftattempted = sum(ftattempted, na.rm = TRUE),
      total_fgattempted = sum(fgattempted, na.rm = TRUE),
      total_assists = sum(assists, na.rm = TRUE),
      total_steals = sum(stealsagainst, na.rm = TRUE),
      
      # Advanced metric calculations
      turnover_pct = if_else(total_possessions > 0, (total_turnovers / total_possessions) * 100, NA_real_),
      three_point_pct = if_else(total_fg3attempted > 0, total_fg3made / total_fg3attempted, NA_real_),
      free_throw_rate = if_else(total_fgattempted > 0, total_ftattempted / total_fgattempted, NA_real_),
      assist_ratio = if_else(total_possessions > 0, (total_assists / total_possessions) * 100, NA_real_),
      opp_steal_rate = if_else(total_possessions > 0, (total_steals / total_possessions) * 100, NA_real_),
      
      .groups = "drop"
    ) %>%
    select(
      season, off_team, total_possessions,
      turnover_pct, three_point_pct, free_throw_rate,
      assist_ratio, opp_steal_rate
    )
}

#---------------------------------------------------------
# 2. Clutch Performance
#    Based on games decided by 5 points or fewer
#---------------------------------------------------------
calculate_clutch_performance <- function(data) {
  data %>%
    # Identify close games
    mutate(close_game = abs(points - opp_points) <= 5) %>%
    
    # Filter to only close games
    filter(close_game) %>%
    
    # Summarise team performance in clutch situations
    group_by(season, off_team) %>%
    summarise(
      clutch_wins = sum(off_win == 1, na.rm = TRUE),
      clutch_games = n(),
      clutch_win_pct = if_else(clutch_games > 0, clutch_wins / clutch_games, NA_real_),
      total_clutch_points = sum(points, na.rm = TRUE),
      total_clutch_opp_points = sum(opp_points, na.rm = TRUE),
      clutch_point_diff = total_clutch_points - total_clutch_opp_points,
      .groups = "drop"
    )
}
