#analyze_model_parameters.R
library(rstan)
library(tidyverse)
library(nflfastR)
library(tidybayes)
library(bayestestR)
library(aws.s3)
library(splines)
library(arrow)
options(tibble.width = Inf)
##reading in data (select one)
bucket_name <- "s3://sagemaker-studio-m35e50pwfmm/"

## save to aws
# bucket_exists(bucket = bucket_name,
#               region = "us-east-1")
file_path <- "nfl_data/adjusted-plus-minus-model/model_files/"

#write draws of parameters to s3
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
for(object_name in object_name_all){
  object_name_write <- paste0(object_name, "_", play_type, ".parquet")
  aws.s3::s3write_using(x = get(object_name),
                        FUN = arrow::write_parquet,
                        bucket = bucket_name,
                        object = paste0(file_path, object_name_write)
  )
  # library(paws)
  # library(arrow)
  # 
  # s3 <- paws::s3(config=list(<your configurations here to give access to s3>))
  # object <- s3$get_object(Bucket = "path_to_bucket", Key = "file_name.parquet")
  # data <- object$Body
  # read_parquet(data)
  
}

## summarize parameters

#player effects
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

#positions
phi_post_tbl_med <- phi_post_draws %>% 
  group_by(position) %>% 
  summarize(effect = median(poseterior_draw), 
            var = var(poseterior_draw)) %>% 
  rename(sub_param = position) %>% 
  mutate(param = 'position') %>% 
  arrange(effect)

#player prior means
player_prior_draws

#player prior mean coefficients
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


### GAME VARIABLES FOR
## success rate
game_effects_sr_summary <- game_effects_sr_draws |> 
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

## epa/pts

game_effects_pts_summary <- game_effects_pts_draws |> 
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

# 
# plyr_effect |> 
#   group_by(team) |> 
#   summarize(avg_team = weighted.mean(player_effect, w = plays), 
#             players = n(), 
#             plays = sum(plays)) |> 
#   filter(!is.na(avg_team)) |> 
#   arrange(desc(avg_team)) |> 
#   filter(players > 10)