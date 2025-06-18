#=========================================================
# Head-to-Head Regular Season Metrics
#=========================================================

#---------------------------------------------------------
# 1. H2H Win Count
#---------------------------------------------------------
calculate_h2h_wins <- function(data) {
  data %>%
    group_by(season, off_team, def_team) %>%
    summarise(
      h2h_wins = sum(off_win, na.rm = TRUE),
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 2. H2H Win Percentage
#---------------------------------------------------------
calculate_h2h_win_pct <- function(data) {
  data %>%
    group_by(season, off_team, def_team) %>%
    summarise(
      h2h_total_games = n(),
      h2h_win_pct = sum(off_win, na.rm = TRUE) / h2h_total_games,
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 3. H2H Average Margin of Victory
#---------------------------------------------------------
calculate_h2h_avg_mov <- function(data) {
  # Create version of the data to extract opponent points
  data_defensive <- data %>%
    select(season, nbagameid, off_team, def_team, points) %>%
    rename(
      opponent_points = points,
      opp_team = off_team,
      def_team = def_team  # this line is redundant, retained for clarity
    )
  
  # Join to compute game-by-game margin of victory
  h2h_data <- data %>%
    left_join(
      data_defensive,
      by = c("season", "nbagameid", "off_team" = "def_team", "def_team" = "opp_team")
    ) %>%
    mutate(
      margin_of_victory = points - opponent_points
    )
  
  h2h_data %>%
    group_by(season, off_team, def_team) %>%
    summarise(
      h2h_avg_mov = mean(margin_of_victory, na.rm = TRUE),
      .groups = "drop"
    )
}

#---------------------------------------------------------
# 4. H2H Net Rating
#    Requires possessions and opponent points
#---------------------------------------------------------
calculate_h2h_net_rating <- function(data) {
  # Ensure defensive possessions and opponent points are present
  data <- calculate_opp_points_and_def_possessions(data)
  
  data %>%
    group_by(season, off_team, def_team) %>%
    summarise(
      total_points_scored       = sum(points, na.rm = TRUE),
      total_off_possessions     = sum(possessions, na.rm = TRUE),
      total_points_allowed      = sum(opp_points, na.rm = TRUE),
      total_def_possessions     = sum(def_possessions, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      h2h_net_rating =
        (total_points_scored / total_off_possessions) * 100 -
        (total_points_allowed / total_def_possessions) * 100
    ) %>%
    select(season, off_team, def_team, h2h_net_rating)
}

#---------------------------------------------------------
# 5. H2H Rebounding Metrics
#    Includes offensive and defensive rebound percentages
#---------------------------------------------------------
calculate_h2h_rebounding <- function(data) {
  data %>%
    group_by(season, off_team, def_team) %>%
    summarise(
      total_off_rebounds         = sum(reboffensive, na.rm = TRUE),
      total_rebound_chances      = sum(reboundchance, na.rm = TRUE),
      off_reb_pct                = total_off_rebounds / total_rebound_chances,
      
      total_def_rebounds         = sum(rebdefensive, na.rm = TRUE),
      total_def_rebound_chances  = sum(reboundchance, na.rm = TRUE),
      def_reb_pct                = total_def_rebounds / total_def_rebound_chances,
      
      .groups = "drop"
    )
}
