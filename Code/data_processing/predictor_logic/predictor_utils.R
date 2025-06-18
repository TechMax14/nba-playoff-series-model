# predictors_util.R


# 1. Function to load and join conference data
get_conference_data <- function(team_data) {
  return(left_join(team_data, conference_map, by = "off_team"))
}

# 3. Calculate Win Percentage and Seed for Regular Season
calculate_win_pct_and_seed <- function(team_data) {
  win_pct <- team_data %>%
    filter(gametype == 2) %>%
    group_by(season, off_team) %>%
    summarise(win_pct = mean(off_win, na.rm = TRUE), .groups = "drop") %>%
    left_join(conference_map, by = "off_team") %>%
    group_by(season, Conference) %>%
    mutate(seed = rank(-win_pct, ties.method = "min")) %>%
    ungroup()
  return(win_pct)
}

# 4. Function to calculate Strength of Schedule (SOS)
calculate_sos <- function(postseason_data, win_pct) {
  sos <- postseason_data %>%
    left_join(win_pct, by = c("season", "def_team" = "off_team")) %>%
    group_by(season, off_team) %>%
    summarise(strength_of_schedule = mean(win_pct, na.rm = TRUE), .groups = "drop")
  return(sos)
}

# Function to calculate Opponent Points and Defensive Possessions
create_opp_data <- function(postseason_data) {
  opp_data <- postseason_data %>%
    select(season, nbagameid, off_team, points, possessions) %>%
    rename(def_team = off_team, opp_points = points, def_possessions = possessions)
  
  # Right join postseason_data with opp_data
  postseason_data <- postseason_data %>%
    right_join(opp_data, by = c("season", "nbagameid", "def_team"))
  
  return(postseason_data)
}

# 5. Function to calculate Postseason Statistics
calculate_playoff_stats <- function(postseason_data, sos) {
  # Calculate clutch games (close games)
  postseason_data <- postseason_data %>%
    mutate(close_game = abs(points - opp_points) <= 5)  # Clutch game if points difference <= 5
  
  last_playoff_stats <- postseason_data %>%
    group_by(season, off_team) %>%
    summarise(
      last_PS_wins = sum(off_win),
      last_PS_total_games = n(),
      last_PS_win_pct = last_PS_wins / last_PS_total_games,
      last_PS_home_wins = sum(off_win[off_home == 1]),
      last_PS_home_games = sum(off_home == 1),
      last_PS_home_win_pct = ifelse(last_PS_home_games > 0, last_PS_home_wins / last_PS_home_games, NA),
      last_PS_away_wins = sum(off_win[off_home == 0]),
      last_PS_away_games = sum(off_home == 0),
      last_PS_away_win_pct = ifelse(last_PS_away_games > 0, last_PS_away_wins / last_PS_away_games, NA),
      last_PS_home_advantage = last_PS_home_win_pct - last_PS_away_win_pct,
      last_PS_point_differential = sum(points, na.rm = TRUE) - sum(opp_points, na.rm = TRUE),
      last_PS_avg_point_differential = last_PS_point_differential / last_PS_total_games,
      last_PS_total_def_possessions = sum(def_possessions),
      last_PS_offensive_rating = 100 * sum(points) / sum(possessions),
      last_PS_defensive_rating = 100 * sum(opp_points) / sum(def_possessions),
      last_PS_net_rating = last_PS_offensive_rating - last_PS_defensive_rating,
      last_PS_turnover_pct = 100 * sum(turnovers) / sum(possessions),
      last_PS_three_point_pct = ifelse(sum(fg3attempted) > 0, sum(fg3made) / sum(fg3attempted), NA),
      last_PS_free_throw_rate = ifelse(sum(fgattempted) > 0, sum(ftattempted) / sum(fgattempted), NA),
      last_PS_assist_ratio = 100 * sum(assists) / sum(possessions),
      last_PS_clutch_wins = sum(off_win[close_game == TRUE], na.rm = TRUE),
      last_PS_clutch_games = sum(close_game == TRUE),
      last_PS_clutch_win_pct = ifelse(last_PS_clutch_games > 0, last_PS_clutch_wins / last_PS_clutch_games, NA),
      last_PS_total_clutch_points = sum(points[close_game == TRUE], na.rm = TRUE),
      last_PS_total_clutch_opp_points = sum(opp_points[close_game == TRUE], na.rm = TRUE),
      last_PS_clutch_point_diff = last_PS_total_clutch_points - last_PS_total_clutch_opp_points,
      last_PS_strength_of_schedule = mean(strength_of_schedule, na.rm = TRUE),
      last_PS_seed = ifelse(all(is.na(seed)), NA, min(seed, na.rm = TRUE)),
      .groups = "drop"
    )
  return(last_playoff_stats)
}

# 6. Function to calculate playoff appearances in the last 3 years
calculate_appearance_counts <- function(last_playoff_stats) {
  appearance_counts <- last_playoff_stats %>%
    group_by(off_team) %>%
    arrange(season) %>%
    mutate(
      last_PS_wins_last_3yrs = slide_dbl(last_PS_wins, ~sum(.x), .before = 2, .complete = TRUE),
      last_PS_avg_wins_last_3yrs = slide_dbl(last_PS_wins, ~mean(.x), .before = 2, .complete = TRUE),
      last_PS_games_last_3yrs = slide_dbl(last_PS_total_games, ~sum(.x), .before = 2, .complete = TRUE),
      last_PS_win_pct_last_3yrs = ifelse(last_PS_games_last_3yrs > 0, last_PS_wins_last_3yrs / last_PS_games_last_3yrs, NA)
    ) %>%
    mutate(
      last_PS_playoff_appearance_last_3yrs = slide_index_int(season, season, ~ sum(.x %in% (max(.x) - 1):(max(.x) - 3)), .before = 0, .complete = TRUE)
    ) %>%
    ungroup()
  return(appearance_counts)
}

# 7. Final merge function
merge_final_features <- function(last_playoff, appearance_counts, conference_map) {
  final_features <- last_playoff %>%
    left_join(appearance_counts, by = c("off_team", "last_playoff_season" = "season")) %>%
    left_join(conference_map, by = "off_team") %>%
    mutate(seasons_since_last_playoff = 2023 - last_playoff_season) %>%
    relocate(last_playoff_season, .before = seasons_since_last_playoff) %>%
    select(-ends_with(".y")) %>%
    rename_with(~ gsub("\\.x$", "", .), ends_with(".x")) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  return(final_features)
}

# 8. Function to prepare dataset for model
prepare_dataset_for_model <- function(prediction_dataset) {
  # Save identity columns
  id_columns <- prediction_dataset %>%
    select(team, opponent)
  
  # Drop non-numeric / unwanted fields (like Conference, team/opponent)
  features_only <- prediction_dataset %>%
    select(-team, -opponent, -Conference)
  
  # Identify numeric columns
  numeric_columns <- features_only %>%
    select(where(is.numeric)) %>%
    colnames()
  
  # Standardize (mean = 0, sd = 1)
  standardized_features <- features_only %>%
    mutate(across(all_of(numeric_columns), ~ as.numeric(scale(.))))
  
  return(list(
    standardized_data = standardized_features,
    id_columns = id_columns
  ))
}

# 9. Function to build raw prediction dataset
build_raw_prediction_dataset <- function(
    regszn_team_data,
    last_postszn_team_data,
    regszn_h2h_data,
    postszn_h2h_data,
    matchups,
    season_year
) {
  # FILTER for relevant season and rename key identifiers
  regszn_team_data_filtered <- regszn_team_data %>%
    filter(season == season_year) %>%
    rename(team = off_team)
  
  last_postszn_team_data_filtered <- last_postszn_team_data %>%
    rename(team = off_team)
  
  regszn_h2h_filtered <- regszn_h2h_data %>%
    filter(season == season_year) %>%
    rename(team = off_team, opponent = def_team)
  
  postszn_h2h_filtered <- postszn_h2h_data %>%
    filter(season == season_year) %>%
    rename(team = off_team, opponent = def_team) %>%
    rename_with(
      ~ paste0("PS_", .),
      .cols = -c(season, team, opponent)
    )
  
  # BUILD RAW PREDICTION DATASET
  prediction_dataset <- matchups %>%
    left_join(
      regszn_h2h_filtered %>%
        select(team, opponent,
               h2h_wins, h2h_total_games, h2h_win_pct,
               h2h_avg_mov, h2h_net_rating) %>%
        rename(
          RS_h2h_wins = h2h_wins,
          RS_h2h_GP = h2h_total_games,
          RS_h2h_win_pct = h2h_win_pct,
          RS_h2h_avg_mov = h2h_avg_mov,
          RS_h2h_net_rating = h2h_net_rating
        ),
      by = c("team", "opponent")
    ) %>%
    left_join(
      postszn_h2h_filtered %>%
        select(team, opponent,
               PS_h2h_hist_GP, PS_h2h_hist_win_pct, PS_h2h_hist_net_rating,
               PS_h2h_hist_home_win_pct, PS_h2h_hist_away_win_pct,
               PS_h2h_GP_last5yrs, PS_h2h_win_pct_last5yrs,
               PS_h2h_net_rating_last5yrs,
               PS_h2h_home_win_pct_last5yrs, PS_h2h_away_win_pct_last5yrs),
      by = c("team", "opponent")
    ) %>%
    left_join(
      regszn_team_data_filtered %>%
        select(team, Conference,
               win_pct, home_advantage, net_rating, offensive_rating, defensive_rating,
               point_differential, last_10_win_pct, strength_of_schedule, seed) %>%
        rename(
          RS_win_pct = win_pct,
          RS_home_advantage = home_advantage,
          RS_net_rating = net_rating,
          RS_off_rating = offensive_rating,
          RS_def_rating = defensive_rating,
          RS_point_diff = point_differential,
          RS_last_10_win_pct = last_10_win_pct,
          RS_SOS = strength_of_schedule,
          RS_seed = seed
        ),
      by = "team"
    ) %>%
    left_join(
      last_postszn_team_data_filtered %>%
        select(team,
               last_PS_win_pct, last_PS_home_win_pct, last_PS_away_win_pct,
               last_PS_home_advantage, last_PS_point_differential, last_PS_avg_point_differential,
               last_PS_offensive_rating, last_PS_defensive_rating, last_PS_net_rating,
               last_PS_turnover_pct, last_PS_three_point_pct, last_PS_free_throw_rate,
               last_PS_assist_ratio, last_PS_strength_of_schedule,
               last_PS_clutch_win_pct, last_PS_clutch_point_diff,
               last_PS_seed, last_PS_win_pct_last_3yrs,
               last_PS_playoff_appearance_last_3yrs, seasons_since_last_playoff) %>%
        rename(
          last_PS_win_pct = last_PS_win_pct,
          last_PS_home_advantage = last_PS_home_advantage,
          last_PS_point_diff = last_PS_point_differential,
          last_PS_avg_point_diff = last_PS_avg_point_differential,
          last_PS_off_rating = last_PS_offensive_rating,
          last_PS_def_rating = last_PS_defensive_rating,
          last_PS_net_rating = last_PS_net_rating,
          last_PS_TO_pct = last_PS_turnover_pct,
          last_PS_3p_pct = last_PS_three_point_pct,
          last_PS_FT_rate = last_PS_free_throw_rate,
          last_PS_SOS = last_PS_strength_of_schedule
        ),
      by = "team"
    ) %>%
    arrange(team) %>%
    mutate(across(where(is.numeric), ~ replace_na(., 0)))
  
  return(prediction_dataset)
}
