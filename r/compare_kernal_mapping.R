#compare_kernal_mapping.R
library(tidyverse)
library(nflfastR)
library(nimble)
library(aws.s3)
bucket_name <- "s3://sagemaker-studio-m35e50pwfmm/"
options(tibble.width = Inf)
aws.s3::bucket_list_df(paste0(bucket_name, "/football_data/"))

data_filenames <- get_bucket_df(bucket_name) %>% 
  as_tibble() %>% 
  rename(filename = Key) %>% 
  filter(str_detect(filename, "nfl_data/play_distributions/")) %>% 
  mutate(filetype = str_extract(filename, "quantiles|summary"),
         season = str_extract(filename, "[[:digit:]]{4,}")) %>% 
  dplyr::select(filename,
                filetype,
                Bucket, 
                season) %>% 
  pivot_wider(values_from = "filename", 
              names_from = "filetype")

play_summary <- play_quantiles <- NULL
### read in play data from distribution models
for(i in 1:nrow(data_filenames)){
  play_summary <- s3read_using(FUN = read_rds,
                                bucket = bucket_name,
                                object = data_filenames$summary[i]) %>% 
    bind_rows(play_summary)
  play_quantiles <-  s3read_using(FUN = read_rds,
                                   bucket = bucket_name,
                                   object = data_filenames$quantiles[i]) %>% 
    bind_rows(play_summary)
}

max_season <- max(data_filenames$season)
min_season <- min(data_filenames$season)
all_seasons <- min_season:max_season
nfl_schedule <- nflreadr::load_schedules(all_seasons)
team_logos <- nflreadr::load_teams()
nfl_pbp <- load_pbp(all_seasons)


# QB Data -----------------------------------------------------------------
qb_seasons <- nflfastR::load_player_stats(season = all_seasons) %>% 
  mutate(epa = replace_na(passing_epa, 0) + replace_na(rushing_epa, 0),
         dropbacks = attempts + sacks + carries) %>% 
  filter(position == "QB") %>% 
  group_by(season,
           player_name, 
           team = recent_team,
           headshot_url
  ) %>% 
  summarize(air_yds_play = sum(passing_air_yards) / sum(dropbacks),
            epa_play = sum(epa) / sum(dropbacks),
            dropbacks = sum(dropbacks)
  ) %>% 
  filter(dropbacks >= 100,
         !is.na(player_name)) %>% 
  ungroup() %>% 
  left_join(team_logos %>% 
              dplyr::select(team_abbr,
                            team_logo_espn),
            by = c("team" = "team_abbr"))

# qb_seasons %>% 
#   arrange(desc(epa_play)) %>% 
#   print(n = 10)

# Play-by-Play Data Wrangling ---------------------------------------------

nfl_pbp <- nfl_pbp %>% 
  mutate(end_yard_line = replace(end_yard_line, end_yard_line == "GB -126", "GB 18"))

nfl_pbp <- nfl_pbp %>% 
  group_by(game_id, 
           game_half) %>% 
  mutate(temp_next_yardline_100 = lead(yardline_100)) %>% 
  ungroup() %>% 
  mutate(end_side_of_field = str_extract(end_yard_line, "[[:alpha:]]+"),
         end_yrdline = str_extract(end_yard_line, "[[:digit:]]{1,3}") %>% as.numeric(),
         turnover = fumble_lost == 1 | interception == 1,
         next_yardline_100 = case_when(turnover & end_yrdline == 50 ~ 50,
                                       turnover & (posteam == end_side_of_field) ~ 
                                         end_yrdline,
                                       turnover & (is.na(end_yard_line)) ~ temp_next_yardline_100,
                                       turnover & (defteam == end_side_of_field) ~ 100 - end_yrdline,
                                       TRUE ~ yardline_100 - yards_gained
         ),
         ## account for interception or fumble return touchdowns
         next_yardline_100 = ifelse( turnover & return_touchdown == 1, 0, next_yardline_100),
         ## account for touchbacks
         next_yardline_100 = ifelse(next_yardline_100 == 100 & !is.na(temp_next_yardline_100), temp_next_yardline_100, next_yardline_100),
         
  )

### show relationship between next play yardline_100 and current for fumbles/interceptions
# nfl_pbp %>% 
#   filter(fumble_lost == 1 | interception == 1, kickoff_attempt == 0) %>% 
#   filter(!is.na(yards_gained)) %>% 
#   ggplot(aes(x = yardline_100, y = next_yardline_100) ) + geom_point() + geom_smooth() + theme_bw()


nfl_pbp_simple <- nfl_pbp %>% 
  mutate(half = ceiling(qtr / 2),
         loc = case_when(tolower(location) == "neutral" ~ "n",
                         posteam == home_team ~ 'h',
                         posteam == away_team ~ 'a',
                         TRUE ~ NA_character_),
         turnover = if_else(fumble_lost == 1 | interception == 1, 1, 0),
         current_tot_score = posteam_score + defteam_score
  ) %>% 
  filter(!play_type %in% c("field_goal", "no_play", "qb_kneel", "qb_spike", "punt")) %>% 
  filter(!is.na(play_type)) %>% 
  dplyr::select(season,
                game_id,
                week,
                play_id,
                series,
                series_success,
                posteam,
                defteam,
                home_team,
                # away_team,
                play_type,
                special_teams_play,
                turnover,
                touchdown,
                passer,
                passer_id,
                rusher,
                rusher_id,
                qb_kneel,
                qb_spike,
                penalty,
                turnover,
                fumble_lost,
                interception,
                pass_attempt,
                rush_attempt,
                roof,
                surface,
                temp,
                wind,
                yards_gained,
                current_tot_score,
                current_score_diff = score_differential,
                epa,
                ep,
                wpa,
                wp,
                vegas_wp,
                loc,
                # location,
                qb_dropback,
                half_seconds_remaining,
                half,
                qtr,
                yardline_100,
                next_yardline_100,
                down,
                ydstogo,
                play_clock,
                posteam_timeouts_remaining,
                defteam_timeouts_remaining
  ) %>% 
  filter(!is.na(down), 
         special_teams_play == 0) %>% 
  left_join(play_summary %>% 
              dplyr::select(season,
                            game_id,
                            play_id,
                            play_quantile),
            by = c("season", "game_id", "play_id"))

nfl_pbp_simple <- nfl_pbp_simple %>% filter(!is.na(play_quantile)) #only a handful of pass/run plays here


# QB Stability & Kernal Covariance Matrix ---------------------------------


#look at QB's at first
nfl_qb_pbp <- nfl_pbp_simple %>% 
  filter(qb_dropback == 1) %>% 
  mutate(qb_id = coalesce(passer_id, rusher_id),
         qb = coalesce(passer, rusher),
         adj_nya = yards_gained + 20*touchdown - 45*turnover
         ) %>% 
  dplyr::select(season,
                game_id,
                play_id,
                qb,
                qb_id,
                epa,
                yards_gained,
                adj_nya,
                wpa,
                play_quantile
                ) %>% 
  filter(!is.na(epa))
epa_avg <- mean(nfl_qb_pbp$epa)
epa_sd <- sd(nfl_qb_pbp$epa)
pos_epa_avg <-  epa_avg - floor(min(nfl_qb_pbp$epa))
gamma_shape <- pos_epa_avg / epa_sd
gamma_scale <- epa_sd^2 / pos_epa_avg

# INTER Season QB Correlation ---------------------------------------------
nfl_qb_yearly_metrics <- nfl_qb_pbp %>% 
  mutate(knorm = qnorm(play_quantile),
         kt5 = qt(play_quantile, df = 5),
         klaplace = qdexp(play_quantile),
         kgamma = qgamma(play_quantile, gamma_shape, gamma_scale)
  ) %>% 
  group_by(season,
           qb,
           qb_id) %>% 
  mutate(n_dropbacks = n()) %>% 
  summarize(across(epa:last_col(), mean)) %>% 
  filter(n_dropbacks >= 50) %>% 
  ungroup()

nfl_qb_yearly_metric_summary <- nfl_qb_yearly_metrics %>% 
  group_by(qb, qb_id) %>% 
  mutate(prev_epa = lag(epa),
         prev_yards_gained = lag(yards_gained),
         prev_adj_nya = lag(adj_nya),
         prev_wpa = lag(wpa),
         prev_play_quantile = lag(play_quantile),
         prev_knorm = lag(knorm),
         prev_kt5 = lag(kt5),
         prev_klaplace = lag(klaplace),
         prev_kgamma = lag(kgamma),
         prev_n_dropbacks = lag(n_dropbacks)
  ) %>% 
  ungroup() %>% 
  filter(complete.cases(.))

n_boot <- 1000
n_within_boot <- 300

inter_season_bootstrap_qb_correlation <- NULL
set.seed(5)
for(b in 1:n_boot){

  
  #correlation matrix between all of these
  nfl_qb_yearly_metric_summary_cor <- nfl_qb_yearly_metric_summary %>% 
    sample_n(n_within_boot, replace = TRUE) %>% 
    dplyr::select(-season,
                  -qb,
                  -qb_id) %>% 
    cor()
  
  inter_season_bootstrap_qb_correlation <- nfl_qb_yearly_metric_summary_cor[!str_detect(colnames(nfl_qb_yearly_metric_summary_cor), "prev_"),
                                                                            str_detect(colnames(nfl_qb_yearly_metric_summary_cor), "prev_")] %>% 
    as_tibble() %>% 
    mutate(comp_var_2 = colnames(nfl_qb_yearly_metric_summary_cor)[!str_detect(colnames(nfl_qb_yearly_metric_summary_cor), "prev_")] ) %>% 
    pivot_longer(cols = starts_with("prev_"),
                 values_to = "correlation", 
                 names_to = "comp_var_1") %>% 
    mutate(across(starts_with("comp_var"), ~str_remove(.x, "prev_")),
           boot = b) %>% 
    bind_rows(inter_season_bootstrap_qb_correlation)

  cat(b, " \r")
  
}


#pct of time highest correlation with itself
inter_season_bootstrap_qb_correlation_summary <- inter_season_bootstrap_qb_correlation %>% 
  filter(comp_var_1 == comp_var_2,
         !comp_var_1 %in% c("n_dropbacks", "tot_games")#ignore counts of dropbacks and games
  ) %>% 
  group_by(boot) %>% 
  mutate(correlation_rank = min_rank(desc(correlation))
  ) %>% 
  ungroup() %>% 
  rename(stat = comp_var_1) %>% 
  group_by(stat) %>% 
  summarize(avg_rank = mean(correlation_rank),
            pct_no_1 = mean(correlation_rank == 1), 
            avg_correlation = mean(correlation),
            sd_correlation = sd(correlation)
  )

inter_season_bootstrap_qb_correlation_summary %>% 
  arrange(desc(avg_correlation))

# Within Season Correlation Bootstrap -------------------------------------


#look at team averages by game
qb_game_avg <- nfl_pbp_simple %>% 
  filter(qb_dropback == 1) %>% 
  mutate(qb_id = coalesce(passer_id, rusher_id),
         qb = coalesce(passer, rusher),
         adj_nya = yards_gained + 20*touchdown - 45*turnover
  ) %>% 
  mutate(knorm = qnorm(play_quantile),
         kt5 = qt(play_quantile, df = 5),
         klaplace = qdexp(play_quantile),
         kgamma = qgamma(play_quantile, 1, 1)
  ) %>% 
  group_by(season,
           week,
           game_id,
           qb_id,
           qb) %>% 
  mutate(n_dropbacks = n()) %>% 
  summarize(across(c("n_dropbacks",
                     "epa", 
                     "yards_gained",
                     "adj_nya",
                     "wpa",
                     "play_quantile",
                     "knorm",
                     "kt5",
                     "klaplace",
                     "kgamma"),
                   ~mean(.x, na.rm = TRUE))
  ) %>% 
  ungroup()

n_boot <- 1000
n_half_weeks <- 9
n_weeks <- 18

intra_season_bootstrap_qb_correlation <- NULL
set.seed(5)
for(b in 1:n_boot){
  
  sample_weeks <- sample(n_weeks, n_half_weeks)
  
  temp_qb_game_avg <- qb_game_avg %>% 
    mutate(season_half = ifelse(week %in% sample_weeks, 1, 2))
  
  qb_half_season_avg <- temp_qb_game_avg %>% 
    group_by(season,
             season_half,
             qb_id,
             qb) %>% 
    mutate(tot_games = n(),
           tot_dropbacks = sum(n_dropbacks)) %>% 
    summarize_at(vars(epa:kgamma, tot_dropbacks, tot_games), ~weighted.mean(.x, w = n_dropbacks, na.rm = TRUE) )  %>% 
    ungroup()
  
  qb_half_season_avg_wide <- qb_half_season_avg %>% 
    pivot_wider(values_from = epa:tot_games,
                names_from = "season_half") %>% 
    filter(tot_dropbacks_1 >= 50, tot_dropbacks_2 >= 50) #must have at least 50 dropbacks in both halves of season
  
  qb_half_ssn_cor <- qb_half_season_avg_wide %>% 
    dplyr::select(ends_with("_1"), ends_with("_2")) %>% 
    cor()

  ## look just at 1st half to second half correlations
  intra_season_bootstrap_qb_correlation <- qb_half_ssn_cor[str_detect(colnames(qb_half_ssn_cor), "_1"),
                  str_detect(colnames(qb_half_ssn_cor), "_2")
  ] %>% 
    as_tibble() %>% 
    mutate(comp_var_1 = colnames(qb_half_ssn_cor)[str_detect(colnames(qb_half_ssn_cor), "_1")] ) %>% 
    pivot_longer(cols = ends_with("_2"),
                 values_to = "correlation", 
                 names_to = "comp_var_2") %>% 
    mutate(across(starts_with("comp_var"), ~str_remove(.x, "_1|_2")),
           boot = b) %>% 
    bind_rows(intra_season_bootstrap_qb_correlation)
  
    cat(b, " \r")
}

## T-distribution (df =5), LaPlace both stronger correlated with 2nd half EPA/play than 1st half EPA/play!!!!


#pct of time highest correlation with itself
intra_season_bootstrap_qb_correlation_summary <- intra_season_bootstrap_qb_correlation %>% 
  filter(comp_var_1 == comp_var_2,
         !comp_var_1 %in% c("tot_dropbacks", "tot_games")#ignore counts of dropbacks and games
         ) %>% 
  group_by(boot) %>% 
  mutate(correlation_rank = min_rank(desc(correlation))
         ) %>% 
  ungroup() %>% 
  rename(stat = comp_var_1) %>% 
  group_by(stat) %>% 
  summarize(avg_rank = mean(correlation_rank),
            pct_no_1 = mean(correlation_rank == 1), 
            avg_correlation = mean(correlation),
            sd_correlation = sd(correlation)
  )

intra_season_bootstrap_qb_correlation_summary %>% 
  arrange(desc(avg_correlation))

#on average EPA is 6th in intra-season correlation

#pct of time highest correlation with epa/play




# Team Stability ----------------------------------------------------------
quant_avg <- mean(nfl_pbp_simple$play_quantile, na.rm = TRUE)
quant_sd <- sd(nfl_pbp_simple$play_quantile, na.rm = TRUE)
min_quant <- floor(min(nfl_pbp_simple$play_quantile, na.rm = TRUE))
pos_quant_avg <-  quant_avg - min_quant
gamma_shape <- pos_quant_avg / quant_sd
gamma_scale <- quant_sd^2 / pos_quant_avg

#look at team averages by game
nfl_game_avg <- nfl_pbp_simple %>% 
  mutate(knorm = qnorm(play_quantile),
         kt5 = qt(play_quantile, df = 5),
         klaplace = qdexp(play_quantile),
         kgamma = qgamma(play_quantile, 1, 1)
  ) %>% 
  group_by(season,
           week,
           game_id,
           posteam,
           defteam,
           pass_attempt,
           rush_attempt) %>% 
  mutate(n_plays = n()) %>% 
  summarize(across(c("n_plays",
                     "epa", 
                     "play_quantile",
                     "knorm",
                     "kt5",
                     "klaplace",
                     "kgamma"),
                   mean)
            ) %>% 
  ungroup()

### Team Half-season bootstrap ######

n_boot <- 1000
n_half_weeks <- 9
n_weeks <- 18

intra_season_bootstrap_team_def_correlation <- intra_season_bootstrap_team_off_correlation <- NULL
set.seed(5)
for(b in 1:n_boot){
  
  sample_weeks <- sample(n_weeks, n_half_weeks)
  
  temp_nfl_game_avg <- nfl_game_avg %>% 
    mutate(season_half = ifelse(week %in% sample_weeks, 1, 2),
           play_type = ifelse(pass_attempt == 1, "pass", "rush")
    )
  
  ## look at first half to second half relationship of each metric
  
  
  off_team_half_ssn_avg <- temp_nfl_game_avg %>% 
    group_by(season,
             season_half,
             team = posteam,
             play_type) %>% 
    summarize(across(n_plays:kgamma, ~weighted.mean(.x, w = n_plays, na.rm = TRUE) ), .groups = "keep" ) %>% 
    mutate(off_def = "off") %>% 
    ungroup()
  
  def_team_half_ssn_avg <- temp_nfl_game_avg %>% 
    group_by(season,
             season_half,
             team = defteam,
             play_type) %>% 
    summarize(across(n_plays:kgamma, ~weighted.mean(.x, w = n_plays, na.rm = TRUE) ), .groups = "keep" ) %>% 
    mutate(off_def = "def") %>% 
    ungroup()
  
  nfl_team_half_ssn_avg <- bind_rows(off_team_half_ssn_avg, def_team_half_ssn_avg) %>% 
    pivot_wider(values_from = n_plays:kgamma,
                names_from = "season_half")
  
  nfl_off_half_ssn_cor <- nfl_team_half_ssn_avg %>% 
    filter(off_def == "off") %>% 
    dplyr::select(n_plays_1:last_col()) %>% 
    cor()
  
  nfl_def_half_ssn_cor <- nfl_team_half_ssn_avg %>% 
    filter(off_def == "def") %>% 
    dplyr::select(n_plays_1:last_col()) %>% 
    cor()
  
  ## 1st half to second half team per play correlation
  #offense
  intra_season_bootstrap_team_off_correlation <- nfl_off_half_ssn_cor[str_detect(colnames(nfl_off_half_ssn_cor), "_1"),
                                                                      str_detect(colnames(nfl_off_half_ssn_cor), "_2")] %>% 
    as_tibble() %>% 
    mutate(comp_var_1 = colnames(nfl_off_half_ssn_cor)[str_detect(colnames(nfl_off_half_ssn_cor), "_1")] ) %>% 
    pivot_longer(cols = ends_with("_2"),
                 values_to = "correlation", 
                 names_to = "comp_var_2") %>% 
    mutate(across(starts_with("comp_var"), ~str_remove(.x, "_1|_2")),
           boot = b) %>% 
    bind_rows(intra_season_bootstrap_team_off_correlation)

  #defense
  intra_season_bootstrap_team_def_correlation <- nfl_def_half_ssn_cor[str_detect(colnames(nfl_def_half_ssn_cor), "_1"),
                                                                      str_detect(colnames(nfl_def_half_ssn_cor), "_2")] %>% 
    as_tibble() %>% 
    mutate(comp_var_1 = colnames(nfl_def_half_ssn_cor)[str_detect(colnames(nfl_def_half_ssn_cor), "_1")] ) %>% 
    pivot_longer(cols = ends_with("_2"),
                 values_to = "correlation", 
                 names_to = "comp_var_2") %>% 
    mutate(across(starts_with("comp_var"), ~str_remove(.x, "_1|_2")),
           boot = b) %>% 
    bind_rows(intra_season_bootstrap_team_def_correlation)
  
  cat(b, " \r")
}

#pct of time highest correlation with itself
#Offense
intra_season_bootstrap_team_off_correlation_summary <- intra_season_bootstrap_team_off_correlation %>% 
  filter(comp_var_1 == comp_var_2,
         !comp_var_1 %in% c("n_plays")#ignore counts of dropbacks and games
  ) %>% 
  group_by(boot) %>% 
  mutate(correlation_rank = min_rank(desc(correlation))
  ) %>% 
  ungroup() %>% 
  rename(stat = comp_var_1) %>% 
  group_by(stat) %>% 
  summarize(avg_rank = mean(correlation_rank),
            pct_no_1 = mean(correlation_rank == 1), 
            avg_correlation = mean(correlation),
            sd_correlation = sd(correlation)
  )
#Defense
intra_season_bootstrap_team_def_correlation_summary <- intra_season_bootstrap_team_def_correlation %>% 
  filter(comp_var_1 == comp_var_2,
         !comp_var_1 %in% c("n_plays")#ignore counts of dropbacks and games
  ) %>% 
  group_by(boot) %>% 
  mutate(correlation_rank = min_rank(desc(correlation))
  ) %>% 
  ungroup() %>% 
  rename(stat = comp_var_1) %>% 
  group_by(stat) %>% 
  summarize(avg_rank = mean(correlation_rank),
            pct_no_1 = mean(correlation_rank == 1), 
            avg_correlation = mean(correlation),
            sd_correlation = sd(correlation)
  )

#show rankings
intra_season_bootstrap_team_off_correlation_summary %>% 
  arrange(desc(avg_correlation))
intra_season_bootstrap_team_off_correlation_summary %>% 
  arrange(desc(avg_correlation))

## EPA worst within season

# Team Inter Season -------------------------------------------------------
nfl_game_avg <- nfl_game_avg %>% 
  mutate(play_type = ifelse(pass_attempt == 1, "pass", "rush"))

off_team_ssn_avg <- nfl_game_avg %>% 
  group_by(season,
           team = posteam,
           play_type) %>% 
  summarize(across(n_plays:kgamma, ~weighted.mean(.x, w = n_plays, na.rm = TRUE) ) ) %>% 
  mutate(off_def = "off") %>% 
  ungroup()

def_team_ssn_avg <- nfl_game_avg %>% 
  group_by(season,
           team = defteam,
           play_type) %>% 
  summarize(across(n_plays:kgamma, ~weighted.mean(.x, w = n_plays, na.rm = TRUE) ) ) %>% 
  mutate(off_def = "def") %>% 
  ungroup()

nfl_team_ssn_avg <- bind_rows(off_team_ssn_avg, def_team_ssn_avg) %>% 
  group_by(team,
           play_type,
           off_def
           ) %>% 
  mutate(lag_n_plays = lag(n_plays),
         lag_epa = lag(epa),
         lag_play_quantile = lag(play_quantile),
         lag_knorm = lag(knorm),
         lag_kt5 = lag(kt5),
         lag_klaplace = lag(klaplace),
         lag_kgamma = lag(kgamma)
         ) %>% 
  filter(season > min(season)) %>% 
  dplyr::select(season, 
                team,
                play_type,
                off_def,
                everything()
                ) %>% 
  ungroup()



n_boot <- 1000
n_within_boot <- 300

inter_season_bootstrap_team_off_correlation <- inter_season_bootstrap_team_def_correlation <- NULL
set.seed(5)
for(b in 1:n_boot){
    
    
    nfl_off_ssn_cor <- nfl_team_ssn_avg %>% 
      sample_n(n_within_boot, replace = TRUE) %>% 
      filter(off_def == "off") %>% 
      dplyr::select(n_plays:last_col()) %>% 
      cor()
    
    nfl_def_ssn_cor <- nfl_team_ssn_avg %>% 
      sample_n(n_within_boot, replace = TRUE) %>% 
      filter(off_def == "def") %>% 
      dplyr::select(n_plays:last_col()) %>% 
      cor()
    
    ## one season to next team per play correlation
    #offense
    nfl_off_ssn_cor[str_detect(colnames(nfl_off_ssn_cor), "lag_"),
                    !str_detect(colnames(nfl_off_ssn_cor), "lag_")]
    #defense
    nfl_def_ssn_cor[str_detect(colnames(nfl_def_ssn_cor), "lag_"),
                    !str_detect(colnames(nfl_def_ssn_cor), "lag_")]

    
    ## All one season to next stability of metrics
    #Offense
    inter_season_bootstrap_team_off_correlation <- nfl_off_ssn_cor[str_detect(colnames(nfl_off_ssn_cor), "lag_"),
                    !str_detect(colnames(nfl_off_ssn_cor), "lag_")] %>% 
      as_tibble() %>% 
      mutate(comp_var_1 = str_remove_all(colnames(nfl_off_ssn_cor), "lag_") %>% unique() ) %>% 
      pivot_longer(cols = -comp_var_1,
                   values_to = "correlation", 
                   names_to = "comp_var_2") %>% 
      mutate(across(starts_with("comp_var"), ~str_remove(.x, "_1|_2")),
             boot = b) %>% 
      bind_rows(inter_season_bootstrap_team_off_correlation)
    
    
    
    
    #Defense
    inter_season_bootstrap_team_def_correlation <- nfl_def_ssn_cor[str_detect(colnames(nfl_def_ssn_cor), "lag_"),
                                                                   !str_detect(colnames(nfl_def_ssn_cor), "lag_")] %>% 
      as_tibble() %>% 
      mutate(comp_var_1 = str_remove_all(colnames(nfl_def_ssn_cor), "lag_") %>% unique() ) %>% 
      pivot_longer(cols = -comp_var_1,
                   values_to = "correlation", 
                   names_to = "comp_var_2") %>% 
      mutate(across(starts_with("comp_var"), ~str_remove(.x, "_1|_2")),
             boot = b) %>% 
      bind_rows(inter_season_bootstrap_team_def_correlation)
  

    cat(b, '\r')
    
}


#pct of time highest correlation with itself
#Offense
inter_season_bootstrap_team_off_correlation_summary <- inter_season_bootstrap_team_off_correlation %>% 
  filter(comp_var_1 == comp_var_2,
         !comp_var_1 %in% c("n_plays")#ignore counts of dropbacks and games
  ) %>% 
  group_by(boot) %>% 
  mutate(correlation_rank = min_rank(desc(correlation))
  ) %>% 
  ungroup() %>% 
  rename(stat = comp_var_1) %>% 
  group_by(stat) %>% 
  summarize(avg_rank = mean(correlation_rank),
            pct_no_1 = mean(correlation_rank == 1), 
            avg_correlation = mean(correlation),
            sd_correlation = sd(correlation)
  )
#Defense
inter_season_bootstrap_team_def_correlation_summary <- inter_season_bootstrap_team_def_correlation %>% 
  filter(comp_var_1 == comp_var_2,
         !comp_var_1 %in% c("n_plays")#ignore counts of dropbacks and games
  ) %>% 
  group_by(boot) %>% 
  mutate(correlation_rank = min_rank(desc(correlation))
  ) %>% 
  ungroup() %>% 
  rename(stat = comp_var_1) %>% 
  group_by(stat) %>% 
  summarize(avg_rank = mean(correlation_rank),
            pct_no_1 = mean(correlation_rank == 1), 
            avg_correlation = mean(correlation),
            sd_correlation = sd(correlation)
  )

#Results
inter_season_bootstrap_team_off_correlation_summary %>% 
  arrange(desc(avg_correlation))
inter_season_bootstrap_team_def_correlation_summary %>% 
  arrange(desc(avg_correlation))


### Save off this information
save.image("compare_epa_alternatives.RData")



### Future Ideas:
## Deal with the nonlinear jump from ~10 yards to touchdown yards
# what is the percentile performance?
# what percent of epa available did you get (or pct of negative did you get?)