#get_nfl_player_contract_by_season.R

get_nfl_player_contract_by_season <- function(nfl_contracts_input){
  
  nfl_player_contract_by_season <- nfl_contracts_input |> 
    arrange(otc_id,
            year_signed,
            years,
            value,
            guaranteed) |> 
    group_by(otc_id) |> 
    mutate(contract_id = paste(otc_id,year_signed,years,team, sep = '-'),
           next_contract_year_signed = lead(year_signed),
           next_contract_id = lead(contract_id)) |> 
    ungroup() |> 
    unnest(cols,
           names_sep = "_",
           keep_empty = TRUE) |> 
    filter(tolower(cols_year) != "total" | is.na(cols_year)) |> 
    mutate(cols_year = as.numeric(cols_year), 
           cols_year = coalesce(cols_year, year_signed),
           rookie_deal = ifelse(year_signed == draft_year, 1, 0)) |> 
    filter(cols_year >= year_signed,
           (cols_year < next_contract_year_signed) | is.na(next_contract_year_signed)
    ) |> 
    arrange(otc_id,
            contract_id,
            cols_year) |> 
    group_by(otc_id,
             cols_year) |> 
    filter(year_signed == max(year_signed)) |> 
    ungroup()
  
  nfl_player_contract_by_season <- nfl_player_contract_by_season |> 
    rename(contract_season = cols_year,
           otc_team = cols_team) |> 
    rename_all(~str_replace(., "cols_", "season_")) |> 
    mutate(across(starts_with("season_"), ~ replace_na(., 0) ) )
  
  return(nfl_player_contract_by_season)
}
