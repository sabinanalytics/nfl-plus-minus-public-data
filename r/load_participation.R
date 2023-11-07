library(MASS)
library(Matrix)
library(parallel)
library(rstan)
library(rstanarm)
library(tidyverse)
library(nflfastR)
library(SparseM)
options(tibble.width = Inf)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())### run on all cores
season <- 2023

nfl_participation <- nflreadr::load_participation(season)
nfl_rosters <- nflreadr::load_rosters_weekly(season)
nfl_contracts <- nflreadr::load_contracts()
nfl_schedule <- nflreadr::load_schedules()
nfl_player_stats <- load_player_stats(season)
nfl_pbp <- load_pbp(season)


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


## Player List for Each Year

player_names <- nfl_rosters %>% 
  count(season,
        gsis_id,
        full_name) %>% 
  group_by(season, gsis_id) %>% 
  filter(n == max(n)) %>% 
  ungroup() %>% 
  dplyr::select(-n)

player_positions <- nfl_rosters %>% 
  group_by(season,
        position,
        depth_chart_position,
        ngs_position,
        gsis_id) %>% 
  summarize(n = n(),
            max_week = max(week)) %>% 
  group_by(season, gsis_id) %>% 
  filter(!is.na(ngs_position)) %>% 
  arrange(desc(n), 
          desc(max_week)) %>% 
  slice(n()) %>% 
  ungroup() %>% 
  dplyr::select(-n,
                -max_week)
  

player_tbl <- nfl_rosters %>% 
  arrange(season,
          week,
          team) %>% 
  distinct(season,
           team,
           draft_number,
           birth_date,
           height,
           weight,
           gsis_id,
           years_exp,
           college) %>% 
  group_by(season, ### this section allows the "team" to show multiple teams if switched mid-year
           draft_number,
           birth_date,
           height,
           weight,
           gsis_id,
           years_exp,
           college) %>% 
  summarize(team = paste(team, collapse = ",")) %>% 
  ungroup() %>% 
  left_join(player_positions, by = c("season", "gsis_id")) %>% 
  left_join(player_names, by = c("season", "gsis_id")) %>% 
  mutate(undrafted = ifelse(is.na(draft_number), 1, 0),
         player_index = 1:n())


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
    left_join(all_participation %>% 
                dplyr::select(-old_game_id,
                              -possession_team),
              by = c("game_id" = "nflverse_game_id", "play_id")
    ) %>% 
    left_join(player_tbl, 
              by = c("season", "gsis_id"))
  
  participation_model <- participation_model %>% 
    ungroup %>% 
    arrange(game_id, play_id, off_def_ind, gsis_id) %>%
    group_by(game_id, play_id) %>% 
    mutate(play_index = cur_group_id()) %>%
    ungroup() %>% 
    arrange(game_id, play_id)
  
  n_plays <- max(participation_model$play_index)
  
  
  # EPA and Success Vector
  play_epa <- participation_model %>% 
    arrange(play_index) %>%
    distinct(play_index, epa) %>%
    pull(epa)
  play_success <- ifelse(play_epa > 0, 1, 0)
  
  ### home/away/neutral each play
  play_home_ind <- participation_model %>% 
    arrange(play_index) %>%
    distinct(play_index, posteam_site_ind) %>% 
    pull(posteam_site_ind)

  ### look up the factor levels and which are which
  player_pos <- player_tbl %>% 
    mutate(model_position = coalesce(ngs_position, "NA") ) %>% 
    mutate(model_position = factor(model_position) %>% 
             fct_relevel(c("QB", "RB", "FB", "SLOT_WR", "WR", "TE", "T", "G", "C", 
                           "EDGE", "INTERIOR_LINE", "OLB", "MLB", "SLOT_CB", "CB", "SAFETY",
                           "NA")
                         )
    ) %>% 
    pull(model_position)
  
  
  
  
  mean_epa <- mean(play_epa)
  sd_epa <- sd(play_epa)
  
  mean_play_success <- mean(play_success)
  sd_play_success <- sd(play_success)
  
  
  # y <- (play_epa - mean_epa)/sd_epa
  # y <- (play_success - mean_play_success) / sd_play_success
  y <- play_success
  
  season_players <- nrow(player_tbl)
  
  X_model <- sparseMatrix(i = participation_model$play_index, 
                          j = participation_model$player_index, 
                          x = participation_model$off_def_ind, 
                          giveCsparse = TRUE, 
                          dims = c(length(y), season_players))
  
  
  sparse_parts <- extract_sparse_parts(X_model)
  
  #   ## designate offensive or defensive player in player_tbl
  # player_tbl <- player_tbl %>% 
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
  
  
  
  ### filter out data from this week and create test/training datasets
  
  ### stan data object
  
  stan_dat <- list(player_index = as.integer(player_tbl$player_index),

                  positions = as.integer(player_pos),
                  home = as.integer(play_home_ind), 
                  
                  y = y, ### takes the centered at 0 epa for hopefully easier estimation
                  n_plays = n_plays,
                  pm_replacement = qnorm(0.1), #10th percentile player
                  n_players = max(player_tbl$player_index),

                  n_position = nlevels(player_pos),
                  n_player_plays = length(sparse_parts$w),
                        
                  player_prior_mean = rep(qnorm(0.1), max(player_tbl$player_index)),
                  # player_prior_sd_multiple = rep(1, max(player_tbl$player_index))
                  # bpmMean = player_tbl$bpm_mean,
                  # bpmVar = player_tbl$bpm_var,
                  # bpmSD = sqrt(player_tbl$bpm_var),
                  
                  # posMat = player_pos_pct_mat  %>% as.matrix,
                  
                  #weight player plays to have mean 0 player effects
                  # playerPlayWeights = player_tbl$plays,
                  
                  # posWeights = position_weights$weight,
                  # sdSumWeights = 0.0001,
                  
                  wX = sparse_parts$w, ## values of sparse matrix
                  vX = sparse_parts$v, # column indicators of sparse matrix
                  uX = sparse_parts$u #row indicators of sparse matrix
  )
  
}
### TO DO: Add CAP_APY_PCT as a prior (but do something else for rookies -- i.e. their own non-informative priors)


stan_start_time <- Sys.time()
nfl_player_stan_fit <- stan(file = "../stan/nfl_success_adj_pm.stan", data = stan_dat,
                           control = list(adapt_delta = 0.801,
                                          #stepsize_jitter = 0.925,
                                          max_treedepth = 14#,
                                          # adapt_init_buffer = 70, adapt_term_buffer = 45, adapt_window = 25
                           ),
                           pars = c("eta", "eta_phi"),include = FALSE,
                           init = function(){list(b = rep(stan_dat$pm_replacement, stan_dat$n_players), 
                                                  u = c(0.02),
                                                  # s2model = c(0.98898),
                                                  phi = c( 2.78414, #QB
                                                           0.9, #RB
                                                           0.56643, #FB
                                                           1.06, 1.06, #SWR/WR
                                                           1, #TE
                                                           1.3, 1.3, 1.5, #OT/OG/C
                                                           1.3, 0.9, #EDGE/INTERIOR
                                                           0.9, 0.8, #OLB/MLB
                                                           1.5, 1.5,  #SCB/CB
                                                           1.2, #SAFETY
                                                           0.5 #NA (Missing position data)
                                                           ), 
                                                  s2player = c(0.0035)
                                                  # positionVarCoef = rep(1, train$stanDat$nPlayers)
                           )}, seed = 6,#s2player = c(0.0015)
                           iter = 800, warmup =350, chains = 8, thin = 1,
                           save_warmup = FALSE)
gc(verbose = TRUE)
stan_end_time <- Sys.time()
cat("Recruiting Stan Model took: ",
    time_length(stan_end_time - stan_start_time, unit = "minutes"),
    " minutes \n")



#player effects
plyr_summary <- summary(nfl_player_stan_fit, pars = "b")$summary
plyr_effect <- player_tbl
plyr_effect$player_effect <- plyr_summary[,"mean"]
plyr_effect$player_effect_sd <- plyr_summary[,"sd"]


### home field and pass play effects
home_effect <- rstan::extract(nfl_player_stan_fit, "u")$u %>% as_tibble() %>% 
  summarize(est = median(value), var = var(value))


sd_player_est <- rstan::extract(nfl_player_stan_fit, "sd_player")$sd_player %>% as_tibble() %>% 
  summarize(est = median(value), var = var(value))


##position effects
phi_post <- rstan::extract(nfl_player_stan_fit, "phi")$phi
phi_post_tbl <- phi_post %>% 
  as.data.frame %>% as_tibble
colnames(phi_post_tbl) <- c("QB", "RB", "FB", "SLOT_WR", "WR", "TE", "T", "G", "C", 
                            "EDGE", "INTERIOR_LINE", "OLB", "MLB", "SLOT_CB", "CB", "SAFETY",
                            "NA")
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

### force positions to sum to 1 (weighted by plays)
##  average players to sum to 0 (weighted by plays)
