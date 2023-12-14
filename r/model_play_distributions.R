#model_play_distributions.R
library(xgboost)
library(parallel)
library(rstan)
library(rstanarm)
library(tidyverse)
library(nflfastR)
library(drf)
library(keras)
library(tensorflow)
library(plotly)

options(tibble.width = Inf)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())### run on all cores
total_seasons <- 13
cross_validate <- FALSE#if true, run through and cross validate, if not fit model on all seasons
# source("get_nfl_player_contract_by_season.R")
# nfl_participation <- nflreadr::load_participation(all_seasons)
# nfl_rosters <- nflreadr::load_rosters_weekly(all_seasons)
# nfl_contracts <- nflreadr::load_contracts()
nfl_schedule <- nflreadr::load_schedules()
# nfl_player_stats <- load_player_stats(all_seasons)
max_season <- max(nfl_schedule$season)
all_seasons <- (max_season - total_seasons + 1):max_season

nfl_pbp <- load_pbp(all_seasons)
# player_yearly_salary <- get_nfl_player_contract_by_season(nfl_contracts)


## a couple of approaches:
## stitched binary models P(y <= 2), P(y <= 3), etc.
## could have decent empirical estimates of all 1st, 2nd, 3rd & 10's -- 
### for other distances, squeeze values between line of scrimmage and first down and increase density (estimate)
### conditional ordinal neural network (cornn)
### Gaussian process? dirichlet process? 

## turnover yards_gained are wrong -- fix
#yrdln
#end_yard_line

## fix known bad "end_yardline" value
nfl_pbp <- nfl_pbp %>% 
  mutate(end_yard_line = replace(end_yard_line, end_yard_line == "GB -126", "GB 18"))

nfl_pbp <- nfl_pbp %>% 
  group_by(game_id, 
           game_half) %>% 
  mutate(temp_next_yardline_100 = lead(yardline_100)) %>% 
  ungroup() %>% 
  mutate(end_side_of_field = str_extract(end_yard_line, "[[:alpha:]]+"),
         end_yrdline = str_extract(end_yard_line, "[[:digit:]]{1,3}") %>% as.numeric(),

         next_yardline_100 = case_when((fumble_lost == 1 | interception == 1) & end_yrdline == 50 ~ 50,
                                       (fumble_lost == 1 | interception == 1) & (posteam == end_side_of_field) ~ 
                                       end_yrdline,
                                       (fumble_lost == 1 | interception == 1) & (is.na(end_yard_line)) ~ temp_next_yardline_100,
                                       (fumble_lost == 1 | interception == 1) & (defteam == end_side_of_field) ~ 
                                         100 - end_yrdline,
                                     TRUE ~ yardline_100 - yards_gained
                                     ),
         ## account for interception or fumble return touchdowns
         next_yardline_100 = ifelse( (fumble_lost == 1 | interception == 1) & return_touchdown == 1, 0, next_yardline_100),
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
                passer,
                passer_id,
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
                )

#define half numerically,
#combine stadium types
#ignore special teams, qb kneels and qb spikes
nfl_pbp_model_data <- nfl_pbp_simple %>% 
  filter(!is.na(play_type),
         special_teams_play == 0, 
         qb_kneel == 0,
         qb_spike == 0,
         play_type %in% c("pass", "run", "no_play"),
         !is.na(down)
         ) %>% 
  ## if indoors, temp and wind are -99 (an encoding)
  mutate(across(temp:wind, ~replace_na(.x, -99))) %>% 
  mutate(play_clock = as.numeric(play_clock)) %>% 
  #make "outdoor" a variable for stadium
  mutate(outdoors_stadium = ifelse(roof == "outdoors" | is.na(roof), 1, 0),
         surface_grass = ifelse(surface == "grass", 1, 0)
         )
# 
# nfl_pbp_model_data_x <- nfl_pbp_model_data %>%
#   dplyr::select(
#     loc,
#     qb_dropback,#no run v pass, only qb_dropback vs not
#     temp,
#     wind,
#     ep,
#     current_tot_score,
#     current_score_diff,
#     wp,
#     vegas_wp,
#     half_seconds_remaining,
#     half,
#     yardline_100,
#     down,
#     ydstogo,
#     play_clock,
#     outdoors_stadium,
#     surface_grass,
#     team_teamouts,
#     opp_timeouts
#   )
# 
# nfl_pbp_model_data_y <- nfl_pbp_model_data %>%
#   dplyr::select(
#                 # epa,
#                 yards_gained,
#                 turnover
#                 )
# 
# ## fit model predicting epa distribution first 
# ##(later we can do a combo of next yardline & whether a turnover -- but quantifying turnover hard)
# n_train <- 25000
# n_data <- nrow(nfl_pbp_model_data)
# set.seed(5)
# train_samples_index <- sample(n_data, n_train)
# test_samples_index <- (1:n_data)[-train_samples_index]
# #distribution model
# epa_dist_rf <- drf(X = nfl_pbp_model_data_x %>%
#                      slice(train_samples_index),
#                    Y = nfl_pbp_model_data_y %>% 
#                      slice(train_samples_index),
#                    seed = 5,
#                    num.trees = 100,
#                    num.features = 6
#                    )
# #variable importance
# variableImportance(epa_dist_rf)
# 
# #predict distribution
# temp <- predict(epa_dist_rf, newdata = nfl_pbp_model_data_x[test_samples_index,][1:6,])
# 
# # distribution weights
# temp$weights[2,]
# #y values
# dim(temp$y)
# temp$y[1:10,]
# 
# ## need to cutoff (i.e. only allow ydsnet, or epa to be valid values)
# 
# #need to calculate percentile outcome for each metric
# 
# 
# #plot temp$y (only shows observed values of y -- can't extrapolate in-between)
# temp$y %>% 
#   as_tibble() %>% 
#   bind_cols(weight = temp$weights[1,]) %>% 
#   ggplot(aes(x = ydsnet, 
#              y = epa,
#              col = factor(turnover))
#          ) + 
#   geom_density2d()
#   # geom_point(size = 0.25)
# 

# ML Setup ----------------------------------------------------------

## ignore all wp/ep estimates going forward
# include timeouts -- no need to to have competing models etc.
# for now, we ignore team strengths or pass/rush balance -- defeats purpose of ultimate model
#team_timeouts, opp_timeouts

### TO DO:
#1. add team team_timeouts & opp_timeouts
#2. model only non-turnover plays
#3. model chance of turnover
#4. model distribution of yardline IF turnover
#5. combine 3-stage model
#6. run 3-stage model through EP model


# Loop By Season (Leave Season Out CV) ------------------------------------

data <-  nfl_pbp_model_data %>%
  filter(!is.na(next_yardline_100),
         between(next_yardline_100, 0, 100)) %>% 
  # mutate(next_yardline_100 = yardline_100 - yards_gained) %>% #defined above
  ## encode location (loc) variable
  mutate(home = ifelse(loc == 'h',1,0),
         away = ifelse(loc == 'a',1,0),
         neutral = ifelse(loc == 'n',1,0)) %>% 
  dplyr::select(
    season,
    game_id, 
    play_id,
    posteam,
    defteam,
    home_team,
    epa,
    target = next_yardline_100,
    turnover,
    home,
    away,
    neutral,
    qb_dropback,#no run v pass, only qb_dropback vs not
    temp,
    wind,
    # ep,
    current_tot_score,
    current_score_diff,
    # wp,
    # vegas_wp,
    half_seconds_remaining,
    half,
    yardline_100,
    down,
    ydstogo,
    play_clock,
    outdoors_stadium,
    surface_grass,
    posteam_timeouts_remaining,
    defteam_timeouts_remaining
  ) %>% 
  filter(!is.na(target))
# Assuming 'data' is your dataframe, 'features' are your predictors, and 'target' is your ordinal target variable

# all xgboost model setup -------------------------------------------------



feature_names <- data %>% 
  select(-turnover,
         -target,
         -season,
         -game_id, 
         -epa,
         -play_id,
         -posteam,
         -home_team,
         -defteam) %>% 
  colnames()

# test_season <- 2023

if(cross_validate){
  run_seasons <- all_seasons
}else{
  run_seasons <- max(all_seasons) + 1
}


for(test_season in run_seasons){
set.seed(123) # for reproducibility
# index <- sample(1:nrow(data), 0.8 * nrow(data))
#index by season
index <- which(data$season != test_season)

#add season weight for training, linear 0.1 decay from current season with minimum weight of 0.1
data <- data %>% 
  mutate(training_weight = pmax( (11 - abs(season - test_season))/10, 0.1) )

#train/test data for all plays
train_data <- data[index, ]
test_data <- data[-index, ]


#train/test data for turnover plays
train_data_to_plays <- data %>% 
  slice(index) %>% 
  filter(turnover == 1)
test_data_to_plays <- data %>% 
  slice(-index) %>% 
  filter(turnover == 1)
#train/test data for no turnover plays
train_data_no_to_plays <- data %>% 
  slice(index) %>% 
  filter(turnover == 0)
test_data_no_to_plays <- data %>% 
  slice(-index) %>% 
  filter(turnover == 0)

unique_yrdline_labels <- 0:100
n_unique_yrdline_labels <- length(unique_yrdline_labels)

#whether or not it's a turnover label
train_to_labels <- train_data$turnover
test_to_labels <- test_data$turnover
#yardline labels for turnover plays
train_to_yrdline_labels <- train_data_to_plays$target
test_to_yrdline_labels <- test_data_to_plays$target
#yardline labels for NON-turnover plays
train_no_to_yrdline_labels <- train_data_no_to_plays$target
test_no_to_yrdline_labels <- test_data_no_to_plays$target

# training xgboost dmatrix
dtrain_to <- xgb.DMatrix(data = as.matrix(train_data[,feature_names]),
                         label = train_to_labels,
                         weight = train_data$training_weight)
dtrain_to_yrdline <- xgb.DMatrix(data = as.matrix(train_data_to_plays[,feature_names]),
                                 label = train_to_yrdline_labels,
                                 weight = train_data_to_plays$training_weight)
dtrain_no_to_yrdline <- xgb.DMatrix(data = as.matrix(train_data_no_to_plays[,feature_names]),
                                    label = train_no_to_yrdline_labels,
                                    weight = train_data_no_to_plays$training_weight)

# testing xgboost dmatrix
if(cross_validate){
  dtest_to <- xgb.DMatrix(data = as.matrix(test_data[,feature_names]), 
                          label = test_to_labels)
  dtest_to_yrdline <- xgb.DMatrix(data = as.matrix(test_data_to_plays[,feature_names]),
                                  label = test_to_yrdline_labels)
  dtest_no_to_yrdline <- xgb.DMatrix(data = as.matrix(test_data_no_to_plays[,feature_names]),
                                     label = test_no_to_yrdline_labels)
}


# Number of rounds for boosting, can be tuned using cross-validation
nrounds <- 100

#defining a watchlist
watchlist = list(train=dtrain_to, test=dtest_to)

# turnover model ----------------------------------------------------------

params_to_model <- list(
  num_class = 2,  # Number of classes
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  eta = 0.025,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  max_depth = 5,
  min_child_weight = 1,
  validate_parameters = TRUE
)
to_model <- xgb.train(params = params_to_model,
                            data = dtrain_to,
                            nrounds = 225,
                            watchlist = watchlist
                            )

# Saving the model (optional)
if(cross_validate){
  model_file_path <- paste0("models/to_model_test_season_", test_season, ".model")
}else{
  model_file_path <- paste0("models/to_model_all_season.model")
}
xgb.save(to_model, model_file_path)

# # Compute feature importance matrix
# importance_matrix_to_model = xgb.importance(colnames(dtrain_to), 
#                                    model = to_model) %>% 
#   as_tibble()
# 
# ## make predictions
# predictions_to <- predict(to_model,
#                        reshape = TRUE,
#                        dtest_to)
# plot(predictions_to[1,], type = 'h')
# apply(predictions_to, 2, min)
# apply(predictions_to, 2, max)
# summary(predictions_to[,2])
# test_data[which.max(predictions_to[,2]),]#end of game desperation play (makes sense)

# turnover yardline model -------------------------------------------------


params_yrdline_model <- list(
  num_class = n_unique_yrdline_labels,  # Number of classes
  booster = "gbtree",
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  eta = 0.025,
  gamma = 1,
  subsample = 0.8,
  colsample_bytree = 0.8,
  max_depth = 5,
  min_child_weight = 1,
  validate_parameters = TRUE
)
to_yrdline_model <- xgb.train(params = params_yrdline_model,
                              data = dtrain_to_yrdline,
                              nrounds = 150,
                              watchlist = watchlist
                              )

# Saving the model (optional)
if(cross_validate){
  model_file_path <- paste0("models/to_yrdline_model_test_season_", test_season, ".model")
}else{
  model_file_path <- paste0("models/to_yrdline_all_season.model")
}
xgb.save(to_yrdline_model, model_file_path)

# # Compute feature importance matrix
# importance_matrix_to_yrdline_model = xgb.importance(colnames(dtrain_to_yrdline), 
#                                             model = to_yrdline_model) %>% 
#   as_tibble()
# 
# ## make predictions
# predictions_to_yrdline <- predict(to_yrdline_model,
#                           reshape = TRUE,
#                           dtest_to_yrdline)
# plot(predictions_to_yrdline[1,], type = 'h')


# no turnover yardline model ----------------------------------------------
## naive model has multinomial log loss of: -(1/m)*sum(y*log(p_1) + (1-y)*log(p_2) + ... + (1-y)*log(p_2)) #if it is in class 1 =
# -log(1/101) = 4.615121


no_to_yrdline_model <- xgb.train(params = params_yrdline_model,
                              data = dtrain_no_to_yrdline,
                              nrounds = 50,#50 rounds or so better for this model
                              watchlist = watchlist
)

# Saving the model (optional)
if(cross_validate){
  model_file_path <- paste0("models/no_to_yrdline_model_test_season_", test_season, ".model")
}else{
  model_file_path <- paste0("models/no_to_yrdline_all_season.model")
}
xgb.save(no_to_yrdline_model, model_file_path)

# # Compute feature importance matrix
# importance_matrix_dtrain_no_to_yrdline = xgb.importance(colnames(dtrain_no_to_yrdline),
#                                                         model = no_to_yrdline_model) %>% 
#   as_tibble()
# 
# 
# ## make predictions
# predictions_no_to_yrdline <- predict(no_to_yrdline_model,
#                                      reshape = TRUE,
#                                      dtest_no_to_yrdline)
# plot(predictions_no_to_yrdline[1,], type = 'h')
# test_data_no_to_plays[1,]
# #specific play probability
# pred_tibble <- tibble(next_yrdline_100 = unique_yrdline_labels,
#        no_to_prob = predictions_no_to_yrdline[1,],
#        )
# 
# pred_dist_plot <- pred_tibble %>% 
#   ggplot(aes(x = next_yrdline_100,
#              y = no_to_prob)) + 
#   geom_point() 
# 
# ggplotly(pred_dist_plot)


# nflfastr expected points model code
# xgb.train(params = params, data = dtrain, nrounds = nrounds, 
#           watchlist = watchlist, verbose = verbose, print_every_n = print_every_n, 
#           early_stopping_rounds = early_stopping_rounds, maximize = maximize, 
#           save_period = save_period, save_name = save_name, xgb_model = xgb_model, 
#           callbacks = callbacks)
# params (as set within xgb.train):
#   booster = "gbtree", objective = "multi:softprob", eval_metric = "mlogloss", num_class = "7", eta = "0.025", gamma = "1", subsample = "0.8", colsample_bytree = "0.8", max_depth = "5", min_child_weight = "1", validate_parameters = "TRUE"



# Loop through all plays and calculate distribution summaries -----------------------------------


#nflfastr ep model:
# fastrmodels::ep_model
# fastrmodels::ep_model$feature_names
#loop through all data for the season

#if not cross-validating, no saving the out of sample data
if(!cross_validate)next

n_test_data <- nrow(test_data)

play_summary <- play_quantiles <- NULL
for(test_data_i in 1:n_test_data){
  
  #all data 
  predict_play_dist <- test_data %>% slice(test_data_i)
  #predicted data row
  predict_play_xgbdata <- xgb.DMatrix(data = as.matrix(predict_play_dist[,feature_names]) )
  
  # Combine Models to Get Next Play Distribution 
  predict_to_prob <- predict(to_model,
                             reshape = TRUE,
                             predict_play_xgbdata)[,2] 
  
  pred_tibble <- tibble(next_yrdline_100 = unique_yrdline_labels,
                        team_yrdline_prob = predict(no_to_yrdline_model,
                                                    reshape = TRUE,
                                                    predict_play_xgbdata)[1,],
                        opp_yrdline_prob = predict_to_prob*
                          predict(to_yrdline_model,
                                  reshape = TRUE,
                                  predict_play_xgbdata)[1,]
  ) %>% 
    pivot_longer(cols = ends_with("_prob"),
                 values_to = "prob",
                 names_to = "posteam_label") %>% 
    #normalize probabilities
    mutate(prob = prob / sum(prob),
           posteam_label = str_extract(posteam_label, "team|opp"))
  
  
  #adjust predicted distribution variables based on the yardline result
  pred_next_play_tibble <- pred_tibble %>% 
    bind_cols(predict_play_dist %>% 
                mutate(season = test_season,
                       roof = ifelse(outdoors_stadium == 1, 'outdoors', 'dome'))
    ) %>% 
    nflfastR::calculate_expected_points() %>% 
    dplyr::select(ep:last_col()) %>% 
    #save off starting down, dist, yrdline, score
    rename(ep_start = ep) %>% 
    mutate(down_start = down,
           yardline_100_start = yardline_100,
           distance_start = ydstogo,
           score_diff_start = current_score_diff
    ) %>% 
    #based on if turnover or not adjust, posteam
    mutate(posteam_start = posteam,
           posteam2 = ifelse(posteam_label == 'team', posteam, defteam),
           defteam2 = ifelse(posteam_label == 'team', defteam, posteam),
    ) %>% 
    dplyr::select(-posteam, -defteam) %>% 
    rename(posteam = posteam2,
           defteam = defteam2) %>% 
    #adjust down, ydstogo, posteam based on outcome of play
    mutate(first_down_yrdge = (yardline_100 - next_yrdline_100) >= ydstogo | posteam_label == "opp",#did you get the required yards or is it a turnover?
           turnover_on_downs = down == 4 & !first_down_yrdge) %>% 
    #switch possessing teams if turnover on downs
    mutate(posteam2 = ifelse(turnover_on_downs, defteam, posteam),
           defteam2 = ifelse(turnover_on_downs, posteam, defteam),
    ) %>% 
    dplyr::select(-posteam, -defteam) %>% 
    rename(posteam = posteam2,
           defteam = defteam2) %>%   
    mutate(
      down = case_when(turnover_on_downs ~ 1, #turnover on downs
                       first_down_yrdge ~ 1,#got the first down
                       next_yrdline_100 %in% c(0,100) ~ NA_real_,#either a TD or Safety so no down
                       TRUE ~ down + 1), #didn't get the first down
      ydstogo = case_when(is.na(down) ~ NA_real_,
                          down == 1 ~ 10,
                          TRUE ~ next_yrdline_100 - (yardline_100 - ydstogo) 
      )
    )
  
  #for next play, take half_seconds_remaining and subtract 5 with minimum of 1
  pred_next_play_tibble <- pred_next_play_tibble %>% 
    mutate(half_seconds_remaining = pmax(half_seconds_remaining - 5, 1))
  
  
  ##TO DO:
  #1 just keep the EP column
  #2. replace td's & safeties with 6.95 or 2 etc.
  #3. make sure the EP column is in the right team's reference for turnover on downs and other turnovers
  
  next_play_distribution <- pred_next_play_tibble %>%
    mutate(season = test_season,
           roof = ifelse(outdoors_stadium == 1, 'outdoors', 'dome')) %>% 
    rename(last_yrdline_100 = yardline_100, 
           yardline_100 = next_yrdline_100) %>% 
    nflfastR::calculate_expected_points() %>% 
    dplyr::select(ep:last_col()) %>% 
    mutate(ep = ifelse(yardline_100 == 0, 6.96, ep),#td value
           ep = ifelse(yardline_100 == 100, -2, ep),#safety value
           turnover = posteam != posteam_start,
           #if ep is for a turnover (or turnover on downs) make value of ep negative
           ep_new = ifelse(turnover, -ep, ep),
           epa = ep_new - ep_start,
           yards_gained = yardline_100_start - yardline_100
    ) %>% 
    dplyr::select(season,
                  posteam,
                  defteam,
                  yardline_100,
                  yards_gained,
                  ep = ep_new,
                  epa,
                  turnover,
                  prob,
                  down,
                  ydstogo,
                  ends_with("_start"),
                  actual_next_yardline_100 = target
    )
  
  #calculate the quantile of the actual play & sequence of quantiles for the play
  quantiles_to_save <- c(0.01, seq(0.05, 0.95, by = 0.05), 0.99)
  next_play_distribution <- next_play_distribution %>% 
    arrange(ep) %>% 
    mutate(cdf_val = cumsum(prob))
  
  ### CHECK:
  ## some missing play_quantile values (19238 out of 361098 from 2014-2023)
  # a lot of them are on touchdowns where next_yardline_100 = 0
  #Example: season = 2014, game_id = 2014_01_BUF_CHI, play_id = 210
  
  
  # Use approx to interpolate
  quantile_values <- approx(x = next_play_distribution$cdf_val,
                            y = next_play_distribution$epa,
                            xout = quantiles_to_save) %>% 
    bind_rows() %>% 
    rename(quantile = x,
           epa = y) %>% 
    mutate(season = predict_play_dist$season,
           game_id = predict_play_dist$game_id,
           play_id = predict_play_dist$play_id)
  
  #function saved to calculate quantile for this play
  impute_epa_quantil_fun <- approxfun(y = next_play_distribution$cdf_val,
                                      x = next_play_distribution$epa)
  
  ## combine output play outcome distributions & save
  predict_play_dist$play_quantile <- impute_epa_quantil_fun(predict_play_dist$epa)
  
  #if this value is missing its because it's either above 0.99 or below 0.01 then replace with those values
  if(is.na(predict_play_dist$play_quantile)){
    predict_play_dist$play_quantile <- case_when(predict_play_dist$epa >= max(next_play_distribution$epa) ~ 0.99,
                                                 predict_play_dist$epa <= min(next_play_distribution$epa) ~ 0.01,
                                                 TRUE ~ 0.5
                                                 )
  }
  play_summary <- play_summary %>% bind_rows(predict_play_dist)
  play_quantiles <- play_quantiles %>% bind_rows(quantile_values)
  
  #write out if last value of season
  if(test_data_i == n_test_data){
    play_summary %>% write_rds(paste0('../data/play_summary_', test_season, '.rds'))
    play_quantiles %>% write_rds(paste0('../data/play_quantiles_', test_season, '.rds'))
  }
  
  cat("Finished play ", test_data_i, " of ", n_test_data, " in ", test_season, " season \r")
  
}

  

  cat("Finished season: ", test_season, " \n")


}


# Plotting Play Outcome Distributions -------------------------------------


#total epa distribution for the play
next_play_distribution %>% 
  ggplot(aes(x = ep,
             y = prob,
             col = posteam,
             fill = posteam)
         ) + 
  geom_col()
#total next yardline distribution for the play
next_play_distribution %>% 
  ggplot(aes(x = yardline_100,
             y = prob,
             col = posteam,
             fill = posteam)
  ) + 
  geom_col() +
  ggtitle()


# Neural Network ----------------------------------------------------------

 

# Normalize or standardize your features (example code for standardization)
test_data[features] <- scale(test_data[features], 
                             center = colMeans(train_data[features]), 
                             scale = apply(train_data[features], 2, sd)
                             )
train_data[features] <- scale(train_data[features])


# Convert features to matrix
train_features <- as.matrix(train_data[features]) %>% as.array()
test_features <- as.matrix(test_data[features]) %>% as.array()



# Build the model
model <- keras_model_sequential() 

model %>%
  layer_dense(units = 128, activation = 'relu', input_shape = length(features), name = 'layer1') %>%
  layer_dense(units = 32, activation = 'relu', name = 'layer2') %>%
  layer_dense(units = n_unique_labels, activation = 'softmax', name = 'ordinal_output')# %>%
  # layer_dense(units = 1, activation = 'sigmoid', name = 'binary_output')


### with just the ordinal piece for now

# Compile the model with multiple outputs
model %>% 
  compile(
    loss = 'sparse_categorical_crossentropy',#'categorical_crossentropy' or sparse_categorical_crossentropy'
    optimizer = 'adam',
    metrics = 'accuracy'
    )
model %>% summary()
# Train the model with multiple targets
model %>% 
  fit(train_features, # Feature data for training
      train_labels, 
      epochs = 10, 
      verbose = 2,
      # batch_size = 128,
      validation_split = 0.2
      )

score <- model %>% evaluate(test_features, test_labels, verbose = 0)

cat('Test loss:', score["loss"], "\n")
cat('Test accuracy:', score["accuracy"], "\n")


# Compile the model with multiple outputs
model %>% compile(
  loss = list(
    "ordinal_output" = 'categorical_crossentropy',
    "binary_output" = 'binary_crossentropy'
  ),
  optimizer = 'adam',
  metrics = list(
    "ordinal_output" = 'accuracy',
    "binary_output" = 'accuracy'
  )
)

# Train the model with multiple targets
history <- model %>% fit(
  x = train_features,  # Feature data for training
  y = list(
    "ordinal_output" = train_labels,
    "binary_output" = train_turnover
  ),
  epochs = 20,
  batch_size = 128,
  validation_split = 0.2
)
# Evaluate the model
model %>% evaluate(
  x = test_features,  # Feature data for testing
  y = list(
    ordinal_output = test_labels,
    binary_output = test_turnover
  )
)



# Prepare your target variables as before...

# Build and compile your model as before...




