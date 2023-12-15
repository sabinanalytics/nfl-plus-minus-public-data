#model_all_year_player_effects.R

# nfl_participation_temp <- nflreadr::load_participation(2016)
## earliest participation is 2016 
## til now


#### Thoughts
## start with 2016, everyone gets a prior based on draft position (for first year)
## AR1 with last season's value the prior for this one
## need to have a variable of first and last season to know which season to go through
## second iteration: can use age to estimate improvement from year to year (automatic age-curve included) -- by position

library(MASS)
library(Matrix)
library(parallel)
library(rstan)
library(rstanarm)
library(tidyverse)
library(nflfastR)
library(SparseM)
library(fuzzyjoin)
library(randomForest)
library(tidybayes)
library(bayestestR)
library(aws.s3)
library(splines)

options(tibble.width = Inf)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())### run on all cores
run_season <- 2016:2023
min_season <- min(run_season)
max_season <- max(run_season)


source("get_nfl_player_contract_by_season.R")

nfl_participation <- nflreadr::load_participation(run_season)
nfl_rosters <- nflreadr::load_rosters_weekly(run_season)
nfl_contracts <- nflreadr::load_contracts()
nfl_schedule <- nflreadr::load_schedules()
nfl_player_stats <- load_player_stats(run_season)
nfl_pbp <- load_pbp(run_season)


player_yearly_salary <- get_nfl_player_contract_by_season(nfl_contracts)


#offense players long version
offense_participation <- nfl_participation %>% 
  filter(n_offense > 0) %>% 
  dplyr::select(-players_on_play,
                -defense_players) %>% 
  rename(player_id = offense_players) %>% 
  separate_rows(player_id, sep = ";") %>% 
  mutate(off_def_ind = 1)
#defense players long version
defense_participation <- nfl_participation %>% 
  filter(n_defense > 0) %>% 
  dplyr::select(-players_on_play,
                -offense_players) %>% 
  rename(player_id = defense_players) %>% 
  separate_rows(player_id, sep = ";") %>% 
  mutate(off_def_ind = -1)

all_participation <- bind_rows(offense_participation, defense_participation)

#remove excess participation objects
rm(offense_participation)
rm(defense_participation)

#simplify personnel to be (11, 12, 21, etc.)
all_participation <- all_participation %>% 
  mutate(off_personnel_simple = str_remove_all(offense_personnel, "[[:space:]]|[[:alpha:]]+|,") %>% substr(., 1,2),
         def_personnel_simple = str_remove_all(defense_personnel, "[[:space:]]|[[:alpha:]]+|,"),
  ) %>% 
  dplyr::select(-offense_personnel,
                -defense_personnel) %>% 
  rename(gsis_id = player_id)

## pull out relevant pbp variables per play
nfl_pbp_simple <- nfl_pbp %>% 
  filter(!is.na(posteam)) %>% 
  mutate(posteam_site_ind = case_when(tolower(location) == "neutral" ~ 0,
                                      posteam == home_team ~ 1,
                                      posteam == away_team ~ -1,
                                      TRUE ~ NA_real_
  )
  ) %>% 
  dplyr::select(season,
                play_id,
                game_id,
                posteam,
                defteam,
                game_date,
                posteam_site_ind,
                ep,
                epa,
                wp,
                wpa,
                yards_gained,
                yards_gained,
                half_seconds_remaining,
                game_half,
                down,
                ydstogo,
                yardline_100,
                posteam_score,
                defteam_score,
                qb_kneel,
                qb_spike,
                qb_dropback,
                rush_attempt,
                pass_attempt,
                punt_attempt,
                extra_point_attempt,
                field_goal_attempt,
                kickoff_attempt,
                special_teams_play)

#remove original pbp to save space
rm(nfl_pbp)
gc(verbose = TRUE)

## add game variables (like roof, surface, weather, etc.)
game_effects <- nfl_schedule |> 
  dplyr::select(game_id, 
                roof,
                surface,
                temp,
                wind) |> 
  mutate(outdoors_ind = ifelse(tolower(roof) == "outdoors", 1, 0),
         grass_ind = ifelse(str_detect(tolower(surface), "grass"), 1, 0),
         missing_temp_ind = ifelse(is.na(temp), 1, 0),
         missing_wind_ind = ifelse(is.na(wind), 1, 0)
         )

## Player List for Each Year

player_names <- nfl_rosters %>% 
  group_by(gsis_id) |>
  summarize(start_season = min(season),
            end_season = max(season),
            tot_games = n(),
            full_name = full_name[n()]) |> 
  ungroup() |> 
  mutate(tot_seasons = end_season - start_season + 1)

##if missing an ngs position, what to fill it in as based on "position" and "depth_chart_position"
infer_ngs_pos_tbl <- nfl_rosters |> 
  count(ngs_position, 
        depth_chart_position, 
        position) |> 
  filter(!is.na(ngs_position))

ngs_pos_rf <- randomForest(factor(ngs_position) ~ depth_chart_position + position, 
                           data = nfl_rosters |> 
                             filter(!is.na(ngs_position))
                           )

##fill in missing ngs positions with ones from model
nfl_rosters$pred_ngs_position = predict(ngs_pos_rf, newdata = nfl_rosters) |> as.character()
nfl_rosters <- nfl_rosters |> 
  mutate(missing_ngs_position = ifelse(is.na(ngs_position), 1, 0),
         ngs_position = coalesce(ngs_position, pred_ngs_position)
         )

all_player_positions <- nfl_rosters %>% 
  group_by(position,
           depth_chart_position,
           ngs_position,
           gsis_id) %>% 
  summarize(tot_weeks = n(),
            max_week = max(week),
            ) %>% 
  arrange(gsis_id,
          desc(tot_weeks), 
          desc(max_week)) %>% 
  ungroup()

player_positions <- all_player_positions |> 
  group_by(gsis_id, ngs_position) |> 
  summarize(tot_weeks = sum(tot_weeks), 
            max_week = max(max_week)) |> 
  arrange(desc(tot_weeks)) |> 
  mutate(tot_weeks = sum(tot_weeks),
         max_week = max(max_week)
         ) |> 
  slice(1) |> 
  ungroup()

#so far 1 player only at OLB - he's a safety so move him there
player_positions <- player_positions |> 
  mutate(ngs_position = ifelse(ngs_position == "OLB", "SAFETY", ngs_position))

player_tbl <- nfl_rosters %>% 
  mutate(gsis_id = replace_na(gsis_id, "-1")) |> 
  arrange(season,
          week,
          team) %>% 
  group_by(### this section allows the "team" to show multiple teams if switched team's at some point
           draft_number,
           gsis_id,
           college) %>% 
  summarize(team = paste(unique(team), collapse = ","),
            entry_year = mean(entry_year, na.rm = TRUE) |> round(),
            min_years_exp = min(years_exp, na.rm = TRUE),
            max_years_exp = max(years_exp, na.rm = TRUE),
            height = mean(height, na.rm = TRUE),
            weight = mean(weight, na.rm = TRUE),
            birth_date = mean(birth_date, na.rm =)
            ) %>% 
  ungroup() %>% 
  left_join(player_positions, by = c("gsis_id")) %>% 
  left_join(player_names, by = c("gsis_id")) %>% 
  mutate(undrafted = ifelse(is.na(draft_number), 1, 0),
         player_index = 1:n())




player_tbl <- player_tbl |> 
  mutate(start_season_ind = start_season - min_season + 1,
         end_season_ind = end_season - min_season + 1
         )

##get contract details for each player by year by fuzzy matching on otc data 
# this loops through each draft year to narrow the list of candidates
all_player_entry_seasons <- player_tbl |> 
  count(entry_year)

player_contract_season_tbl <- NULL
for(s in 1:nrow(all_player_entry_seasons)){
  temp_entry_year <- all_player_entry_seasons$entry_year[s]
  
  
  ## first get the best match from otc
  temp_matched <- player_tbl |> 
    filter(entry_year == temp_entry_year) |> 
    stringdist_left_join(player_yearly_salary |> 
                           filter(draft_year == temp_entry_year) |> 
                           dplyr::select(player,
                                         otc_position = position,
                                         otc_team,
                                         otc_id,
                                         otc_draft_overall = draft_overall,
                                         contract_season,
                                         contract_signed = year_signed,
                                         contract_years  = years,
                                         contract_tot_value = value,
                                         contract_tot_guaranteed = guaranteed,
                                         contract_apy_cap_pct = apy_cap_pct,
                                         contract_season_cap_percent = season_cap_percent,
                                         draft_year,
                                         rookie_deal
                           ),
                         by = c("full_name" = "player"),
                         ignore_case = TRUE,
                         distance_col = "match_string_dist",
                         max_dist = 0.25,#match_string_dist <= 0.4,
                         method = 'jw') |> 
    filter(draft_number == otc_draft_overall | 
             (is.na(draft_number) & is.na(otc_draft_overall) )
    ) 
  
  # #new way
  matched_data <- temp_matched |>
    group_by(otc_id) |>
    filter(match_string_dist == min(match_string_dist) ) |>
    ungroup()
  
  # #old way
  matched_data <- matched_data |>
    group_by(gsis_id) |>
    filter(match_string_dist == min(match_string_dist) ) |>
    ungroup()
  
  ## combine data
  player_contract_season_tbl <- player_contract_season_tbl |> 
    bind_rows( 
      matched_data
    )
  
  
}
## could manually go through but about 8 out of 138 not complete matches may be wrong... pretty good.
# player_contract_season_tbl |> 
#   filter(match_string_dist != 0) |> 
#   distinct(entry_year, 
#            full_name, 
#            draft_number,
#            otc_draft_overall,
#            player, 
#            match_string_dist
#            ) |> 
#   arrange(desc(match_string_dist))

# a couple of otc duplicates
player_contract_season_tbl <- player_contract_season_tbl |> 
  group_by(gsis_id, contract_season) |> 
  slice(1) |>
  ungroup()



# break out into run and pass plays ---------------------------------------

play_type <- 'pass'

if(play_type == 'pass'){
  nfl_pbp_model_data <- nfl_pbp_simple %>% 
    filter(qb_dropback == 1,
           qb_kneel == 0,
           qb_spike == 0,
           extra_point_attempt == 0,
           field_goal_attempt == 0,
           kickoff_attempt == 0,
           special_teams_play == 0) %>% 
    dplyr::select(season:defteam_score)
  
  participation_model <- nfl_pbp_model_data %>% 
    inner_join(all_participation %>% 
                dplyr::select(-old_game_id,
                              -possession_team),
              by = c("game_id" = "nflverse_game_id", "play_id")
    ) %>% 
    left_join(player_tbl, 
              by = c("gsis_id"))
  
  
  ## filter out missing epa data
  participation_model <- participation_model %>% 
    filter(!is.na(epa), !is.na(yards_gained))
  
  ## add season_index to "participation_model"
  participation_model <- participation_model %>% 
    ungroup %>% 
    arrange(season, game_id, play_id, off_def_ind, gsis_id) %>%
    group_by(game_id, play_id) %>% 
    mutate(play_index = cur_group_id(),
           ) %>%
    ungroup() %>%
    mutate(season_index = dense_rank(season) ) |> 
    arrange(game_id, play_id)
  
  
  n_plays <- max(participation_model$play_index)
  n_seasons = length(unique(participation_model$season))
  
  #expand player_index to player_season_index by lengthening player_index by number of total seasons
  participation_model <- participation_model %>% 
    mutate(player_season_index = n_seasons*(player_index - 1) + season_index )
  
  
  play_data_model <- participation_model %>% 
    arrange(play_index) |> 
    distinct(season,
             play_id,
             game_id,
             posteam,
             defteam,
             play_index, 
             epa,
             ep,
             wp,
             wpa,
             posteam_site_ind,
             defenders_in_box,
             number_of_pass_rushers,
             off_personnel_simple,
             def_personnel_simple
             ) |> 
    left_join(game_effects, 
              by = c("game_id")) |> 
    mutate(across(c("temp", "wind"), ~coalesce(.x, -999) ) )
  
  # EPA and Success Vector
  play_epa <- play_data_model %>% pull(epa)
  play_success <- ifelse(play_epa > 0, 1, 0)
  
  ### home/away/neutral each play
  play_home_ind <- play_data_model %>% pull(posteam_site_ind)
  
  ### make "model_position" the "ngs_position" but replace missing (only 1 so far) with "FB" (generic)
  player_tbl_play_type <- player_tbl %>% 
    mutate(model_position = coalesce(ngs_position, "FB"),
           start_season_ind = coalesce(start_season_ind, 1),
           end_season_ind = coalesce(end_season_ind, max(end_season_ind, na.rm = TRUE) ),
           tot_seasons = coalesce(tot_seasons, max(tot_seasons, na.rm = TRUE) ),
    )
  
  
  #player plays 
  player_tbl_play_type <- player_tbl_play_type |> 
    left_join(participation_model |> 
                count(gsis_id,
                      ngs_position) |> 
                rename(plays = n) |> 
                mutate(model_position = coalesce(ngs_position, "NA")) |> 
                dplyr::select(-ngs_position),
              by = c("gsis_id", "model_position")
    ) |> 
    mutate(plays = replace_na(plays, 0)) |> 
    mutate(model_position = factor(model_position) %>% 
             fct_relevel(c("QB", "RB", "FB", "SLOT_WR", "WR", "TE", "T", "G", "C", 
                           "EDGE", "INTERIOR_LINE", "MLB", "SLOT_CB", "CB", "SAFETY")
             )
    )
  
  #remove player with no plays
  # player_tbl_play_type <- player_tbl_play_type |> 
  #   filter(plays > 0)
  
  
  player_pos <- player_tbl_play_type %>% 
    pull(model_position)
  
  # player position matrix (right now only have 1 position per player, so not relevant)
  # player_pos_mat <- player_tbl_play_type |> 
  #   distinct(player_season_index,
  #            gsis_id,
  #            model_position,
  #            plays) |> 
  #   pivot_wider(values_from = "plays",
  #               names_from = "model_position")
  
  #player plays
  player_plays <- player_tbl_play_type |>
    arrange(player_index) |> 
    pull(plays)
  
  #total position plays
  position_plays <- player_tbl_play_type |> 
    group_by(model_position) |> 
    summarize(plays = sum(plays)) |> 
    ungroup() |> 
    mutate(position_play_pct = plays / sum(plays)) |> 
    arrange(model_position)
  
  # add contract value to player table
  player_tbl_play_type <- player_tbl_play_type |>
    left_join(player_contract_season_tbl |>
                filter(contract_season >= min_season,
                       contract_season <= max_season) |> 
                ungroup() |> ## need to take average contract over length of this model maybe
                summarize(across(c(contract_apy_cap_pct,
                                 contract_season_cap_percent,
                                 rookie_deal),
                                 ~ mean(.x, na.rm = TRUE)
                                 ),
                          .by = gsis_id),
              by = c("gsis_id")
    )
  ## now deal with missing values on draft_number, contract_apy_cap_pct, and rookie_deal
  player_tbl_play_type <- player_tbl_play_type |> 
    mutate(draft_number = replace_na(draft_number, max(draft_number, na.rm = TRUE) + 30 )
           ) |> 
    mutate(rookie_deal = case_when(!is.na(rookie_deal) ~ rookie_deal,
                                   #pct of seasons within 3 of entry in window
                                   TRUE ~ sum(entry_year >= start_season &
                                                entry_year < (start_season + 3)
                                              ) / 
                                          (end_season - start_season + 1)
                                   ),
           rookie_deal = coalesce(rookie_deal, 0)
    ) |>
    group_by(model_position,
             rookie_deal,
             undrafted) |> #replace missing apy_cap_pct with the 10th percentile based on position, drafted, and on rookie_deal or not
    mutate(contract_apy_cap_pct = coalesce(contract_apy_cap_pct, quantile(contract_apy_cap_pct, 0.1, na.rm = TRUE)) ) |>
    ungroup() |> # for any contracts that didn't get covered by the replacement
    mutate(contract_apy_cap_pct = coalesce(contract_apy_cap_pct, quantile(0.1, contract_apy_cap_pct, na.rm = TRUE)) )
  
  #if a player was already in the league before the first season here, then flag
  player_tbl_play_type <- player_tbl_play_type |> 
    mutate(already_in_league = ifelse(entry_year < start_season, 1, 0),
           already_in_league = replace_na(already_in_league, 1)
           )
  
  # Weighted summary metrics of salary by position
  ## now calculate implied percentages of positional value by contracts & prior for player value?
  pos_shrinkage_prior <- player_tbl_play_type |> 
    group_by(model_position) |> 
    summarize(avg_apy_cap_pct = weighted.mean(contract_apy_cap_pct, w = plays, na.rm = TRUE),
              plays = sum(plays, na.rm = TRUE)
    ) |> 
    mutate(pos_shrink_prior_mean = avg_apy_cap_pct * (1/weighted.mean(avg_apy_cap_pct, w = plays, na.rm = TRUE)) ) |> 
    ungroup() |> 
    arrange(model_position)
  
  # Ignore this pct of cap prior since we will just use draft position for now
  # ## variables for player priors (right now just based on contract value and whether on rookie deal)
  # #draft_number^(-0.2) * rookie_deal + (1-rookie_deal)*apy_cap_pct_vs_avg  (the last variable is only within position)
  # player_tbl_play_type <- player_tbl_play_type |> 
  #   group_by(rookie_deal,
  #            model_position) |> 
  #   mutate(apy_cap_pct_vs_avg = contract_apy_cap_pct - weighted.mean(contract_apy_cap_pct, w = plays) ) |> 
  #   ungroup()
  
  mean_epa <- mean(play_epa)
  sd_epa <- sd(play_epa)
  
  z_epa <- (play_epa - mean_epa) / sd_epa
  
  mean_play_success <- mean(play_success)
  sd_play_success <- sd(play_success)
  
  
  # y <- (play_epa - mean_epa)/sd_epa
  # y <- (play_success - mean_play_success) / sd_play_success
  y_success <- play_success
  
  n_players = max(player_tbl_play_type$player_index)
  n_player_seasons = n_players*n_seasons
  
  X_model <- sparseMatrix(i = participation_model$play_index, 
                          j = participation_model$player_season_index, 
                          x = participation_model$off_def_ind, 
                          giveCsparse = TRUE, 
                          dims = c(length(y_success), n_player_seasons))
  
  
  sparse_parts <- extract_sparse_parts(X_model)
  
  #   ## designate offensive or defensive player in player_tbl_play_type
  # player_tbl_play_type <- player_tbl_play_type %>% 
  #   mutate(Off = if_else(position_group %in% c("C", "OG", "OT", "QB", "RB", "SWR", "TE", "WR"), 1, 0), 
  #          Def = if_else(Off == 1, 0, 1))
  # 
  ## sparse matrix for team offense, defensive effects
  # unique_player_teams <- team_games$temp_espn_team_id %>% unique()
  # team_ind_match <- match(participation_model$espn_team_id, unique_player_teams)
  # 
  # team_off_ind_mat <- sparseMatrix(i = participation_model$play_index[participation_model$offense == 1], 
  #                                  j = team_ind_match[participation_model$offense == 1], 
  #                                  x = 1, 
  #                                  dims = c(length(y), length(unique_player_teams))
  # )
  # team_def_ind_mat <- sparseMatrix(i = participation_model$play_index[participation_model$offense == -1], 
  #                                  j = team_ind_match[participation_model$offense == -1], 
  #                                  x = -1, 
  #                                  dims = c(length(y), length(unique_player_teams))
  # )
  
  ## just player effects for now for direct comparison
  # x <- cbind( play_home_ind, X_model)
  # # x <- cbind( play_home_ind, play_pass,X_model, team_off_ind_mat, team_def_ind_mat) 
  # diag_ridge <- sparseMatrix(i = 1:ncol(x), j = 1:ncol(x), x = 1, giveCsparse = TRUE) 
  
  ## player draft basis expansion
  player_draft_basis <- bs(player_tbl_play_type$draft_number, df = 4)
  player_draft_basis_col <- ncol(player_draft_basis)

  
  ### filter out data from this week and create test/training datasets
  
  ### stan data object
  
  stan_dat <- list(
                   positions = as.integer(player_pos),
                   home = as.integer(play_home_ind), 
                   
                   y_success = y_success, ### takes the centered at 0 epa for hopefully easier estimation
                   z_epa = z_epa, ## the z-score of the epa per play
                   n_plays = n_plays,
                   n_players = n_players,
                   n_player_seasons = n_player_seasons,#number of players * tot # of seasons looked at
                   n_seasons = n_seasons,
                   n_position = nlevels(player_pos),
                   n_player_plays = length(sparse_parts$w),
                   
                   
                   ##variables surrounding the game/plays itself
                   grass_ind = play_data_model$grass_ind,
                   outdoors_ind = play_data_model$outdoors_ind,
                   non_missing_temp_ind = 1 - play_data_model$missing_temp_ind,
                   non_missing_wind_ind = 1 - play_data_model$missing_wind_ind,
                   temp = play_data_model$temp,
                   wind = play_data_model$wind,
                   
                   
                   player_plays = player_plays,#number of total plays per player
                   position_play_pct = position_plays$position_play_pct, #pct of total plays played by each position
                   
                   ## denote the start season and end season for each player and total seasons
                   player_start_season_index = player_tbl_play_type$start_season_ind,
                   player_end_season_index = player_tbl_play_type$end_season_ind,
                   player_tot_seasons = player_tbl_play_type$tot_seasons,
                   
                   # player_prior_mean = rep(qnorm(0.1), max(player_tbl_play_type$player_index)),
                   # player_prior_sd_multiple = rep(1, max(player_tbl_play_type$player_index))
                   ## variables for player priors (right now just based on contract value and whether on rookie deal)
                   #draft_number^(-0.2) * rookie_deal + (1-rookie_deal)*apy_cap_pct_vs_avg  (the last variable is only within position)
                   player_rookie_deal = player_tbl_play_type$rookie_deal,
                   player_draft_number = player_tbl_play_type$draft_number,
                   player_undrafted = player_tbl_play_type$undrafted,
                   #denote if player was already in league before first year of model
                   player_already_in_league = player_tbl_play_type$already_in_league,
                   # player_apy_cap_pct_vs_avg = player_tbl_play_type$apy_cap_pct_vs_avg,
                   player_draft_basis = player_draft_basis,
                   player_draft_basis_col = player_draft_basis_col,
                   
                   
                   # posMat = player_pos_pct_mat  %>% as.matrix,
                   
                   #weight player plays to have mean 0 player effects
                   # playerPlayWeights = player_tbl_play_type$plays,
                   
                   pos_shrinkage_prior_mean = pos_shrinkage_prior$pos_shrink_prior_mean,
                   pos_shrinkage_prior_sd = rep(1, nrow(pos_shrinkage_prior)),
                   sd_sum_weighted_phi = 0.0001,#tolerance of the weighted average of phi parameters
                   
                   wX = sparse_parts$w, ## values of sparse matrix
                   vX = sparse_parts$v, # column indicators of sparse matrix
                   uX = sparse_parts$u #row indicators of sparse matrix
  )
  
}

stan_start_time <- Sys.time()
nfl_player_stan_fit <- stan(file = "../stan/all_year_player_model.stan", data = stan_dat,
                            control = list(adapt_delta = 0.81,
                                           stepsize_jitter = 0.925,
                                           max_treedepth = 14#,
                                           # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
                            ),
                            pars = c("eta", 
                                     "eta_phi", 
                                     "b_long", 
                                     "mu_success", 
                                     "mu_epa",
                                     "player_prior_mean_model",
                                     "player_prior_sd_model"),
                            include = FALSE,
                            init = function(){list(#b = rep(stan_dat$pm_replacement, stan_dat$n_players), 
                                                   u = c(0.02),
                                                   # s2model = c(0.98898),
                                                   phi = c( 2.78414, #QB
                                                            0.9, #RB
                                                            0.56643, #FB
                                                            1.06, 1.06, #SWR/WR
                                                            1, #TE
                                                            1.3, 1.3, 1.5, #OT/OG/C
                                                            1.3, 0.9, #EDGE/INTERIOR
                                                            0.8, #MLB
                                                            1.5, 1.5,  #SCB/CB
                                                            1.2 #SAFETY
                                                   ), 
                                                   sd_player = c(0.213),
                                                   sd_model = 0.06,
                                                   sum_weighted_player_effect = 0,
                                                   sum_weighted_phi = 1
                                                   # positionVarCoef = rep(1, train$stanDat$nPlayers)
                            )}, seed = 6,#s2player = c(0.0015)
                            iter = 600, warmup = 200, chains = 11, thin = 1,
                            save_warmup = FALSE)
gc(verbose = TRUE)
stan_end_time <- Sys.time()
cat("Recruiting Stan Model took: ",
    time_length(stan_end_time - stan_start_time, unit = "minutes"),
    " minutes \n")

#temporarly save stan model
nfl_player_stan_fit |> write_rds("nfl_player_stan_fit.rds")
### Notes:
#1. positional value priors should be stronger? why isn't QB No. 1?
#2. aaron donald?
#3. positions are weird: Kelce a WR and Puka a TE? should I switch sources?

##effective sample sizes
neff_tbl <- effective_sample(nfl_player_stan_fit) |> 
  as_tibble() |> 
  rename(ess = ESS,
         param = Parameter)

## player effects
player_draws <- nfl_player_stan_fit %>%
   spread_draws(b[player_index, season_index],
                sep = "[,]")

player_param_summary <- player_draws |> 
  group_by(player_index,
           season_index) |> 
  summarize(post_mean = mean(b),
            post_sd = sd(b),
            prob_pos = mean(b > 0),
            prob_neg = 1 - prob_pos
            ) |>
  ungroup() |> 
  mutate(season = season_index - 1 + min_season)

## combine table of players with season estimates (this ignores positional value -- added with positional phi parameter)
player_summary_play_type <- player_tbl_play_type |> 
  left_join(player_param_summary,
            by = c("player_index")) |> 
  filter(season >= start_season,
         season <= end_season)

### KIRK COUSINS, DERRICK HENRY IN 2023 TWICE? ## Where is Taylor Heinicke?

## Game parameters (home field, intercept, and other game-level effects)
success_rate_game_params <- nfl_player_stan_fit %>%
  spread_draws(game_params_success[param_ind],
               sep = "[,]") |> 
  group_by(param_ind) |> 
  summarize(post_mean = mean(game_params_success),
            post_sd = sd(game_params_success),
            prob_pos = mean(game_params_success > 0),
            prob_neg = 1 - prob_pos
            ) |> 
  cbind(tibble(param = c("intercept",
                         "home",
                         "grass",
                         "outdoors",
                         "temp",
                         "wind")
               )
        )
epa_game_params <- nfl_player_stan_fit %>%
  spread_draws(game_params_epa[param_ind],
               sep = "[,]") |> 
  group_by(param_ind) |> 
  summarize(post_mean = mean(game_params_epa),
            post_sd = sd(game_params_epa),
            prob_pos = mean(game_params_epa > 0),
            prob_neg = 1 - prob_pos
  ) |> 
  cbind(tibble(param = c("home",
                         "grass",
                         "outdoors",
                         "temp",
                         "wind")
               )
        )


## player prior means
player_prior_draws <- nfl_player_stan_fit %>%
  spread_draws(player_prior_mean_model[b_long_index],
               sep = "[,]")
## player prior parameters (including the draft) (beta_player, beta_player_draft)
player_prior_coef_draws <- nfl_player_stan_fit %>%
  spread_draws(beta_player[coef_num],
               sep = "[,]")
player_prior_draft_basis_coef_draws <- nfl_player_stan_fit %>%
  spread_draws(player_draft_basis[basis_col_num],
               sep = "[,]")

player_prior_coef_summary <- player_prior_draft_basis_coef_draws |> 
  group_by(basis_col_num) |> 
  summarize(post_mean = mean(player_draft_basis),
            post_sd = sd(player_draft_basis),
            prob_pos = mean(player_draft_basis > 0),
            prob_neg = 1 - prob_pos
  ) |>
  mutate(param = "player_draft_basis") |> 
  rename(coef_num = basis_col_num) |> 
  bind_rows(
    player_prior_coef_draws |> 
      group_by(coef_num) |> 
      summarize(post_mean = mean(beta_player),
                post_sd = sd(beta_player),
                prob_pos = mean(beta_player > 0),
                prob_neg = 1 - prob_pos
      ) |> 
      mutate(param = "beta_player")
  )
## variances (sd_model, sd_player_yoy, sd_player_first, sd_player_rookie)


## positional value (phi)

## soft constraints (sum_weighted_phi, sum_weighed_player_effect)
game_params_success
game_params_epa


##position effects
phi_post <- rstan::extract(nfl_player_stan_fit, "phi")$phi
phi_post_tbl <- phi_post %>% 
  as.data.frame %>% as_tibble
colnames(phi_post_tbl) <- c("QB", "RB", "FB", "SLOT_WR", "WR", "TE", "T", "G", "C", 
                            "EDGE", "INTERIOR_LINE", "MLB", "SLOT_CB", "CB", "SAFETY")
phi_post_tbl <- phi_post_tbl %>% 
  pivot_longer(cols = everything(), 
               names_to = "position", 
               values_to = "poseterior_draw")

phi_post_tbl_med <- phi_post_tbl %>% 
  group_by(position) %>% 
  summarize(effect = median(poseterior_draw), 
            var = var(poseterior_draw)) %>% 
  rename(sub_param = position) %>% 
  mutate(param = 'position') %>% 
  arrange(effect)


## save to aws
bucket_exists(bucket = "s3://sagemaker-studio-m35e50pwfmm",
              region = "us-east-1")

# 
# plyr_effect |> 
#   group_by(team) |> 
#   summarize(avg_team = weighted.mean(player_effect, w = plays), 
#             players = n(), 
#             plays = sum(plays)) |> 
#   filter(!is.na(avg_team)) |> 
#   arrange(desc(avg_team)) |> 
#   filter(players > 10)