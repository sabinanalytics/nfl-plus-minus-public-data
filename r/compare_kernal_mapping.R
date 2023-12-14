#compare_kernal_mapping.R
library(tidyverse)
library(nflfastR)
library(nimble)

data_filenames <- list.files("../data/") %>% 
  enframe() %>% 
  rename(filename = value) %>% 
  mutate(filetype = str_extract(filename, "quantiles|summary"),
         season = str_extract(filename, "[[:digit:]]{4,}")) %>% 
  dplyr::select(-name) %>% 
  pivot_wider(values_from = "filename", 
              names_from = "filetype")

play_summary <- play_quantiles <- NULL
### read in play data from distribution models
for(i in 1:nrow(data_filenames)){
  play_summary <- read_rds(paste0('../data/', data_filenames$summary[i])) %>% bind_rows(play_summary)
  play_quantiles <- read_rds(paste0('../data/', data_filenames$quantiles[i])) %>% bind_rows(play_quantiles)
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
  dplyr::select(season,
                game_id,
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
            by = c("season", "game_id", "play_id")) %>% 
  filter(!is.na(play_quantile)) #SHOULD GET RID OF THIS ONCE THIS IS FIXED


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
                ) 
epa_avg <- mean(nfl_qb_pbp$epa)
epa_sd <- sd(nfl_qb_pbp$epa)
pos_epa_avg <-  epa_avg - floor(min(nfl_qb_pbp$epa))
gamma_shape <- pos_epa_avg / epa_sd
gamma_scale <- epa_sd^2 / pos_epa_avg

nfl_qb_yearly_metric_summary <- nfl_qb_pbp %>% 
  mutate(knorm = qnorm(play_quantile),
         kcauchy = qt(play_quantile, df = 1),
         klaplace = qdexp(play_quantile),
         kgamma = qgamma(play_quantile, gamma_shape, gamma_scale)
         ) %>% 
  group_by(season,
           qb,
           qb_id) %>% 
  mutate(n_dropbacks = n()) %>% 
  summarize(across(epa:last_col(), mean)) %>% 
  filter(n_dropbacks >= 50) %>% 
  group_by(qb, qb_id) %>% 
  mutate(prev_epa = lag(epa),
         prev_yards_gained = lag(yards_gained),
         prev_adj_nya = lag(adj_nya),
         prev_wpa = lag(wpa),
         prev_play_quantile = lag(play_quantile),
         prev_knorm = lag(knorm),
         prev_kcauchy = lag(kcauchy),
         prev_klaplace = lag(klaplace),
         prev_kgamma = lag(kgamma),
         ) %>% 
  ungroup() %>% 
  filter(complete.cases(.))
  
#correlation matrix between all of these
nfl_qb_yearly_metric_summary %>% 
  dplyr::select(-season,
                -qb,
                -qb_id) %>% 
  cor()
