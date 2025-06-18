# utilities/constants.R

#=========================================================
# Constants and Static Reference Tables
#=========================================================

#---------------------------------------------------------
# 1. Conference Mapping
#    Maps each NBA team abbreviation to its conference
#    ("E" = Eastern, "W" = Western)
#---------------------------------------------------------
conference_map <- tibble(
  off_team = c(
    "ATL", "BKN", "BOS", "CHA", "CHI", "CLE", "DET", "IND", "MIA", "MIL",
    "NYK", "ORL", "PHI", "TOR", "WAS",  # Eastern Conference
    "DAL", "DEN", "GSW", "HOU", "LAC", "LAL", "MEM", "MIN", "NOP", "OKC",
    "PHX", "POR", "SAC", "SAS", "UTA"   # Western Conference
  ),
  Conference = c(rep("E", 15), rep("W", 15))
)

#---------------------------------------------------------
# 2. Playoff Matchups
#    Defines the 2023 NBA playoff matchups
#---------------------------------------------------------
matchups_2023 <- tribble(
  ~team, ~opponent,
  "BOS", "MIA",
  "MIA", "BOS",
  "CLE", "ORL",
  "ORL", "CLE",
  "MIL", "IND",
  "IND", "MIL",
  "NYK", "PHI",
  "PHI", "NYK",
  "OKC", "NOP",
  "NOP", "OKC",
  "LAC", "DAL",
  "DAL", "LAC",
  "MIN", "PHX",
  "PHX", "MIN",
  "DEN", "LAL",
  "LAL", "DEN"
)

#---------------------------------------------------------
# 3. Feature Selection
#    Selected features based off CV and VIF
#---------------------------------------------------------
selected_features <- c(
  "RS_h2h_win_pct",
  "RS_h2h_net_rating",
  "PS_h2h_hist_win_pct",
  "PS_h2h_hist_net_rating",
  "PS_h2h_win_pct_last5yrs",
  "PS_h2h_net_rating_last5yrs",
  "RS_win_pct",
  "RS_home_advantage",
  "RS_net_rating",
  "RS_last_10_win_pct",
  "RS_seed"
)
