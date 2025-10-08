#map_team_abbr.R
# library(dplyr)
# library(rlang)

library(dplyr)
library(rlang)

map_team_abbr <- function(data, team_col, new_col, direction = c("to_pff", "to_fastr")) {
  direction <- match.arg(direction)
  
  abbr_map <- tibble::tibble(
    nflfastr_abbr = c(
      "ARI", "ATL", "BAL", "BUF", "CAR", "CHI", "CIN", "CLE", "DAL", "DEN",
      "DET", "GB",  "HOU", "IND", "JAX", "KC",  "LA",  "LAC", "LV",  "MIA",
      "MIN", "NE",  "NO",  "NYG", "NYJ", "OAK", "PHI", "PIT", "SD",  "SEA",
      "SF",  "STL", "TB",  "TEN", "WAS"
    ),
    pff_team = c(
      "ARZ", "ATL", "BLT", "BUF", "CAR", "CHI", "CIN", "CLV", "DAL", "DEN",
      "DET", "GB",  "HST", "IND", "JAX", "KC",  "LA",  "LAC", "LV",  "MIA",
      "MIN", "NE",  "NO",  "NYG", "NYJ", "OAK", "PHI", "PIT", "SD",  "SEA",
      "SF",  "SL",  "TB",  "TEN", "WAS"
    )
  )
  
  team_col_quo <- enquo(team_col)
  
  if (direction == "to_pff") {
    data %>%
      left_join(abbr_map, by = setNames("nflfastr_abbr", quo_name(team_col_quo))) %>%
      mutate(!!sym(new_col) := coalesce(pff_team, !!team_col_quo)) %>%
      select(-pff_team)
  } else {
    data %>%
      left_join(abbr_map, by = setNames("pff_team", quo_name(team_col_quo))) %>%
      mutate(!!sym(new_col) := coalesce(nflfastr_abbr, !!team_col_quo)) %>%
      select(-nflfastr_abbr)
  }
}

# pff_plays |> 
#   map_team_abbr(team_col = "offense_team", new_col = "fastr_off", direction = "to_fastr")

