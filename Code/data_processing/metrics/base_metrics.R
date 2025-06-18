#=========================================================
# Basic Team Metrics Functions
#=========================================================

#---------------------------------------------------------
# 1. Win Percentage
#---------------------------------------------------------
calculate_win_percentage <- function(data) { 
  data %>%
    group_by(season, off_team) %>%
    summarise(
      wins = sum(off_win, na.rm = TRUE),
      total_games = n(),
      win_pct = wins / total_games,
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 2. Home Court Advantage
#---------------------------------------------------------
calculate_home_advantage <- function(data) {
  data %>%
    group_by(season, off_team, off_home) %>%
    summarise(
      wins = sum(off_win == 1, na.rm = TRUE),
      total_games = n(),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = off_home,
      values_from = c(wins, total_games),
      names_prefix = "home_"
    ) %>%
    mutate(
      home_wins = coalesce(wins_home_1, 0),
      home_games = coalesce(total_games_home_1, 1),  # Prevent division by zero
      away_wins = coalesce(wins_home_0, 0),
      away_games = coalesce(total_games_home_0, 1),
      home_win_pct = home_wins / home_games,
      away_win_pct = away_wins / away_games,
      home_advantage = home_win_pct - away_win_pct
    ) %>%
    select(
      season, off_team,
      home_wins, home_games, home_win_pct,
      away_wins, away_games, away_win_pct,
      home_advantage
    )
}

#---------------------------------------------------------
# 3. Add Opponent Points and Defensive Possessions
#---------------------------------------------------------
calculate_opp_points_and_def_possessions <- function(data) {
  opp_data <- data %>%
    select(season, nbagameid, off_team, points, possessions) %>%
    rename(
      def_team = off_team,
      opp_points = points,
      def_possessions = possessions
    )
  
  data %>%
    right_join(opp_data, by = c("season", "nbagameid", "def_team"))
}

#---------------------------------------------------------
# 4. Point Differential (Total + Average)
#---------------------------------------------------------
calculate_point_differential <- function(data) {
  data %>%
    group_by(season, off_team) %>%
    summarise(
      total_points_scored = sum(points, na.rm = TRUE),
      total_points_allowed = sum(opp_points, na.rm = TRUE),
      point_differential = total_points_scored - total_points_allowed,
      avg_point_differential = point_differential / n(),
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 5. Offensive, Defensive, and Net Ratings
#---------------------------------------------------------
calculate_ratings <- function(data) {
  data %>%
    group_by(season, off_team) %>%
    summarise(
      total_points_scored = sum(points, na.rm = TRUE),
      total_possessions = sum(possessions, na.rm = TRUE),
      total_points_allowed = sum(opp_points, na.rm = TRUE),
      total_def_possessions = sum(def_possessions, na.rm = TRUE),
      offensive_rating = (total_points_scored / total_possessions) * 100,
      defensive_rating = (total_points_allowed / total_def_possessions) * 100,
      net_rating = offensive_rating - defensive_rating,
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 6. Team Momentum (Last 10 Games)
#---------------------------------------------------------
calculate_momentum <- function(data) {
  data %>%
    arrange(season, off_team, gamedate) %>%
    group_by(season, off_team) %>%
    mutate(
      last_10_win_pct = zoo::rollapply(off_win, width = 10, FUN = mean, align = "right", fill = NA),
      last_10_avg_point_diff = zoo::rollapply(points - opp_points, width = 10, FUN = mean, align = "right", fill = NA)
    ) %>%
    summarise(
      last_10_win_pct = if (all(is.na(last_10_win_pct))) NA else last(na.omit(last_10_win_pct)),
      last_10_avg_point_diff = if (all(is.na(last_10_avg_point_diff))) NA else last(na.omit(last_10_avg_point_diff)),
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 7. Strength of Schedule (Average Opponent Win %)
#---------------------------------------------------------
calculate_sos <- function(data, win_pct) {
  data %>%
    select(season, nbagameid, off_team, def_team) %>%
    left_join(win_pct, by = c("season", "def_team" = "off_team")) %>%
    group_by(season, off_team) %>%
    summarise(
      strength_of_schedule = mean(win_pct, na.rm = TRUE),
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 8. Conference Ranking (by Win %)
#---------------------------------------------------------
rank_teams_by_conference <- function(data) {
  data %>%
    group_by(season, Conference) %>%
    mutate(
      seed = rank(-win_pct, ties.method = "min")
    ) %>%
    ungroup()
}
