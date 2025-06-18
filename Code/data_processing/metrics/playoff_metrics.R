#=========================================================
# Playoff Team Metrics Functions
#=========================================================

#---------------------------------------------------------
# 1. Playoff Experience (Last 3 Seasons)
#---------------------------------------------------------
add_playoff_experience_and_clean <- function(data) {
  data %>%
    arrange(off_team, season) %>%
    group_by(off_team) %>%
    mutate(
      # Lag one season to avoid current-season leakage
      lagged_wins = lag(wins),
      lagged_total_games = lag(total_games),
      
      # Rolling sums/means over previous 3 seasons (excluding current)
      wins_last_3yrs       = slider::slide_dbl(lagged_wins, sum, .before = 2, .complete = TRUE),
      avg_wins_last_3yrs   = slider::slide_dbl(lagged_wins, mean, .before = 2, .complete = TRUE),
      games_last_3yrs      = slider::slide_dbl(lagged_total_games, sum, .before = 2, .complete = TRUE),
      win_pct_last_3yrs    = wins_last_3yrs / games_last_3yrs
    ) %>%
    ungroup() %>%
    select(
      -matches("total_points_scored|total_points_allowed|total_possessions"),
      -lagged_wins, -lagged_total_games
    )
}

#---------------------------------------------------------
# 2. Playoff Appearances in Last 3 Years
#    Checks if team played in the 3 previous seasons
#---------------------------------------------------------
calculate_playoff_appearances_last_3 <- function(data) {
  data %>%
    group_by(off_team) %>%
    arrange(season) %>%
    mutate(
      # Define previous 3 seasons
      prev_szn1 = season - 1,
      prev_szn2 = season - 2,
      prev_szn3 = season - 3,
      
      # Get list of all past seasons for current team
      prev_seasons = purrr::map(row_number(), ~ season[1:(.x - 1)]),
      
      # Check if each prev_szn was a playoff season for this team
      match_prev_szn1 = purrr::map2_int(prev_szn1, prev_seasons, ~ as.integer(.x %in% .y)),
      match_prev_szn2 = purrr::map2_int(prev_szn2, prev_seasons, ~ as.integer(.x %in% .y)),
      match_prev_szn3 = purrr::map2_int(prev_szn3, prev_seasons, ~ as.integer(.x %in% .y)),
      
      # Sum number of playoff appearances in last 3 seasons
      playoff_appearance_last_3yrs = match_prev_szn1 + match_prev_szn2 + match_prev_szn3
    ) %>%
    ungroup() %>%
    select(off_team, season, playoff_appearance_last_3yrs)
}

#---------------------------------------------------------
# 3. Last Playoff Appearance
#    Tracks most recent prior season with playoff games
#---------------------------------------------------------
calculate_last_playoff_appearance <- function(data) {
  data %>%
    arrange(off_team, season) %>%
    group_by(off_team) %>%
    mutate(
      # Identify valid playoff seasons (non-zero games)
      playoff_seasons = ifelse(!is.na(total_games) & total_games > 0, season, NA),
      
      # Carry most recent non-NA playoff season forward
      last_playoff_season = lag(zoo::na.locf(playoff_seasons, na.rm = FALSE)),
      
      # Calculate gap since last playoff
      seasons_since_last_playoff = ifelse(is.na(last_playoff_season), NA, season - last_playoff_season)
    ) %>%
    ungroup() %>%
    select(-playoff_seasons)
}

#---------------------------------------------------------
# 4. Head-to-Head Playoff Metrics
#    Calculates all-time and recent (last 5 seasons) metrics
#---------------------------------------------------------
get_playoff_h2h_up_to_season <- function(data, cutoff_season) {
  # Filter for playoff games prior to current season
  data_filtered <- data %>% filter(season < cutoff_season)
  teams <- sort(unique(data$off_team))
  
  # Generate all team-vs-team combinations (no self-matchups)
  matchups <- expand.grid(Off_Team = teams, Def_Team = teams) %>%
    filter(Off_Team != Def_Team)
  
  # Summary function for any matchup set
  summarize_matchup <- function(df) {
    total_games <- nrow(df)
    win_pct <- if (total_games > 0) mean(df$off_win) else NA_real_
    net_rating <- if (total_games > 0) mean(df$points - df$opp_points) else NA_real_
    home_win_pct <- if (sum(df$off_home == 1) > 0) mean(df$off_win[df$off_home == 1]) else NA_real_
    away_win_pct <- if (sum(df$off_home == 0) > 0) mean(df$off_win[df$off_home == 0]) else NA_real_
    c(total_games, win_pct, net_rating, home_win_pct, away_win_pct)
  }
  
  # Compute metrics for each matchup
  purrr::pmap_dfr(matchups, function(Off_Team, Def_Team) {
    historical <- data_filtered %>% filter(off_team == Off_Team, def_team == Def_Team)
    recent     <- historical %>% filter(season >= (cutoff_season - 5))
    
    tibble(
      season = cutoff_season,
      off_team = Off_Team,
      def_team = Def_Team,
      
      # All-time h2h playoff metrics
      h2h_hist_GP               = summarize_matchup(historical)[1],
      h2h_hist_win_pct          = round(summarize_matchup(historical)[2], 3),
      h2h_hist_net_rating       = round(summarize_matchup(historical)[3], 2),
      h2h_hist_home_win_pct     = round(summarize_matchup(historical)[4], 3),
      h2h_hist_away_win_pct     = round(summarize_matchup(historical)[5], 3),
      
      # Playoff h2h metrics over last 5 years
      h2h_GP_last5yrs            = summarize_matchup(recent)[1],
      h2h_win_pct_last5yrs       = round(summarize_matchup(recent)[2], 3),
      h2h_net_rating_last5yrs    = round(summarize_matchup(recent)[3], 2),
      h2h_home_win_pct_last5yrs  = round(summarize_matchup(recent)[4], 3),
      h2h_away_win_pct_last5yrs  = round(summarize_matchup(recent)[5], 3)
    )
  })
}
