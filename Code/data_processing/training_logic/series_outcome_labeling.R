# generate_playoff_series.R

library(dplyr)

generate_playoff_series_outcomes <- function(team_data) {
  team_data <- team_data %>%
    filter(gametype == 4)
  
  games_unique <- team_data %>%
    group_by(season, nbagameid) %>%
    slice(1) %>%
    ungroup() %>%
    mutate(
      winner = ifelse(off_win == 1, off_team, def_team),
      loser  = ifelse(off_win == 1, def_team, off_team)
    )
  
  series_results <- games_unique %>%
    group_by(season, winner, loser) %>%
    summarise(wins = n(), .groups = 'drop')
  
  series_summary <- full_join(
    series_results %>%
      rename(team = winner, opponent = loser),
    series_results %>%
      rename(team = loser, opponent = winner, losses = wins),
    by = c("season", "team", "opponent")
  ) %>%
    mutate(
      wins = ifelse(is.na(wins), 0, wins),
      losses = ifelse(is.na(losses), 0, losses),
      total_games = wins + losses,
      won_series = ifelse(wins > losses, 1, 0)
    ) %>%
    filter(total_games > 0) %>%
    rename(series_wins = wins, series_losses = losses, total_games_in_series = total_games)
  
  return(series_summary)
}
