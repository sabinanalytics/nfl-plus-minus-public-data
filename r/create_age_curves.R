#create_age_curves.R
library(rstan)
library(tidyverse)
library(nflfastR)
library(tidybayes)
library(bayestestR)
library(aws.s3)
library(splines)
library(arrow)
library(data.table)
options(tibble.width = Inf)
##reading in data (select one)
bucket_name <- "s3://sagemaker-studio-m35e50pwfmm/"

## save to aws
# bucket_exists(bucket = bucket_name,
#               region = "us-east-1")
file_path <- "nfl_data/adjusted-plus-minus-model/model_files/"

# parameter names in s3
object_name_all <- c("neff_tbl",
                     "player_tbl_play_type",
                     "player_draws",
                     "player_prior_coef_draws",
                     "player_prior_draft_basis_coef_draws",
                     "phi_post_draws",
                     "game_effects_sr_draws",
                     "game_effects_pts_draws",
                     "std_dev_draws",
                     "soft_constraint_draws")

objects_to_read <- c("player_tbl_play_type",
                     "player_draws")

final_age_summary <- NULL
for(play_type_run in c("pass", "rush")){
  for(object_name in objects_to_read){
    
    object_name_read <- paste0(object_name, "_", play_type_run, ".parquet")
    temp <- aws.s3::s3read_using(x = get(object_name),
                                 FUN = arrow::read_parquet,
                                 bucket = bucket_name,
                                 object = paste0(file_path, object_name_read)
    )
    
    assign(str_remove(str_remove(object_name_read, "_pass|_rush"), ".parquet"), temp)
  }
  
  
  
  # create tibble with one row per player season. --------
  
  ## combine player_tbl_play_type_rush & player_tbl_play_type_pass
  player_play_type_season_df <- player_tbl_play_type %>% 
    dplyr::select(
      draft_number,
      gsis_id,
      entry_year:contract_apy_cap_pct
    ) %>% 
    mutate(play_type = play_type_run) %>% 
    distinct() %>% 
    arrange(gsis_id, 
            play_type) %>% 
    slice(rep(1:n(), times = tot_seasons)) %>% 
    mutate(start_season = coalesce(start_season, min(start_season, na.rm = TRUE) ),
           end_season = coalesce(end_season, max(end_season, na.rm = TRUE) )
    ) %>% 
    group_by(gsis_id,
             play_type) %>% 
    mutate(season_index = 1:n(),
           season = (start_season - 1) + season_index ) %>% 
    ungroup()
  
  #replace missing birthdates with average age for non-missing birthdays for their first season
  avg_rookie_age <- player_play_type_season_df %>% 
    filter(!is.na(birth_date)) %>% 
    mutate(rookie_age = time_length(ymd(paste0(entry_year, "-09-01")) - birth_date, unit = "days")) %>% 
    pull(rookie_age) %>% 
    mean(na.rm = TRUE) %>% 
    as.numeric()
  
  player_play_type_season_df <- player_play_type_season_df %>% 
    mutate(birth_date = coalesce(birth_date, 
                                 (ymd(paste0(entry_year, "-09-01")) - days(floor(avg_rookie_age)))
    ),
    age = time_length(ymd(paste0(season, "-09-01")) - birth_date, unit = 'years') %>% as.numeric(),
    height = coalesce(height, mean(height, na.rm = TRUE))
    )
  
  # combine pass and rushing draws for each player -----
  # for each player calculate difference from rookie season draw
  
  
  # Convert the data.frame to a data.table
  setDT(player_draws)
  
  # Perform the operations
  player_draws[, b_yoy_diff := b - shift(b), by = .(player_index, .chain, .iteration)]
  player_draws[, b_yoy_cum := cumsum(b_yoy_diff, na.rm = TRUE), by = .(player_index, .chain, .iteration)]
  player_draws[, c(".chain", ".iteration", ".draw") := NULL]
  player_draws <- player_draws[!is.na(b_yoy_diff)]
  
  #add age for that season for the values
  player_season_df <- player_play_type_season_df 
  setDT(player_season_df)
  setkeyv(player_season_df, c("player_index", "season_index"))   # Set the key for dt2
  setkeyv(player_draws, c("player_index", "season_index")) 
  
  # Perform the left join (not enough memory here)
  age_diff_draws <- player_draws[player_season_df[, .(player_index, season_index, age, ngs_position, height, weight)], nomatch = 0]
  
  ## by position, fit age curves
  age_diff_draws[, age_floor := floor(age)]
  
  # Calculate mean and standard deviation
  default_quantile_seq <- seq(0.01, 0.99, by = 0.01)
  calc_quantiles <- function(x, quantile_seq = default_quantile_seq) {
    as.list(quantile(x, probs = quantile_seq, na.rm = TRUE))
  }
  
  # Calculate mean, standard deviation, and quantiles, grouped by 'age_floor' and 'ngs_position'
  summary_stats <- age_diff_draws[, .(mean_b = mean(b, na.rm = TRUE), 
                                      sd_b = sd(b, na.rm = TRUE),
                                      quantiles_b = calc_quantiles(b),
                                      mean_b_yoy_diff = mean(b_yoy_diff, na.rm = TRUE),
                                      sd_b_yoy_diff = sd(b_yoy_diff, na.rm = TRUE),
                                      quantiles_b_yoy_diff = calc_quantiles(b_yoy_diff)), 
                                  by = .(age_floor, ngs_position)]
  summary_stats[, 
                quantile := default_quantile_seq,
                by = .(age_floor, ngs_position)]
  
  summary_stats <- as.data.frame(summary_stats) %>% 
    as_tibble() %>% 
    unnest(cols = starts_with("quantiles"))
  
  rm(draws_pass)
  
  gc(verbose = TRUE)
  
  # TO DO:
  #maybe before aggregation do the cumulative sum 
  
  # library(paws)
  # s3 <- paws::s3(config=list(<your configurations here to give access to s3>))
  cat("Finished reading in ", object_name_read, '\n')
  
  # Combine summaries
  final_age_summary <- summary_stats %>% 
    mutate(play_type = play_type_run) %>% 
    bind_rows(final_age_summary)
  
}



## plot age curve
pass_summary_stats %>% 
  ggplot(aes(x = age_floor,
             y = quantiles_b_yoy_diff,
             group = quantile,
             col = quantile)) + 
  geom_line(alpha = 0.5) + 
  theme_bw() + 
  theme(legend.position = 'bottom') +
  facet_wrap(~ngs_position)
