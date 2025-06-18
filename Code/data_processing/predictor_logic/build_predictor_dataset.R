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