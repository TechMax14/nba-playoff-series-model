# merge_and_clean.R

library(dplyr)

merge_and_clean_data <- function(series_outcomes, reg_szn_team_data, post_szn_team_data, reg_szn_h2h_data, post_szn_h2h_data) {
  reg_szn_h2h_data <- reg_szn_h2h_data %>%
    rename(team = off_team, opponent = def_team)
  post_szn_h2h_data <- post_szn_h2h_data %>%
    rename(team = off_team, opponent = def_team)
  reg_szn_team_data <- reg_szn_team_data %>%
    rename(team = off_team)
  post_szn_team_data <- post_szn_team_data %>%
    rename(team = off_team)
  
  combined_data <- series_outcomes %>%
    left_join(reg_szn_h2h_data %>%
                rename_with(~ paste0("RS_", .), -c(season, team, opponent)),
              by = c("season", "team", "opponent")) %>%
    left_join(post_szn_h2h_data %>%
                rename_with(~ paste0("PS_", .), -c(season, team, opponent)),
              by = c("season", "team", "opponent")) %>%
    left_join(reg_szn_team_data %>%
                rename_with(~ paste0("RS_", .), -c(season, team, Conference)),
              by = c("season", "team")) %>%
    left_join(post_szn_team_data,
              by = c("season", "team"))
  
  combined_data <- combined_data %>%
    filter(if_all(starts_with("RS_"), ~ !is.na(.))) %>%
    mutate(across(contains("PS_") | contains("last_"), ~ replace_na(., 0))) %>%
    select(-c(series_wins, series_losses, total_games_in_series)) %>%
    arrange(season, team)
  
  return(combined_data)
}
