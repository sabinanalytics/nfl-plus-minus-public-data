#predict_qb_contract.R
library(lme4)
library(magrittr)
library(tidyverse)
library(splines)
library(nflfastR)
library(cfbfastR)
library(mice)
library(broom.mixed)
library(tidybayes)
library(tools)
library(tidymodels)
library(vip)
options(tibble.width = Inf)
#if working directly isn't R folder then move it
source("convert_team_abbreviation.R")
source("get_nfl_player_contract_by_season.R")
source("load_or_cache_data.R")


# Read in Models -----
current_time <- Sys.time()
model_start_season <- min_data_season <- 2006
this_season <- 2025
all_seasons <- model_start_season:this_season
# Reading in Data ---------------------------------------------------------
nfl_contracts <- nflreadr::load_contracts()
nfl_contracts_by_season <- get_nfl_player_contract_by_season(nfl_contracts)

nfl_rosters <- load_or_cache_data(load_fun = nflreadr::load_rosters,
                                  file_stub = "historical_rosters",
                                  seasons = all_seasons,
                                  refresh_seasons = this_season,
                                  cache_dir = local_cache_dir)

nfl_rosters <- nfl_rosters |> 
  mutate(team = case_when(
    team == "ARZ" ~ "ARI",
    team == "BLT" ~ "BAL",
    team == "CLV" ~ "CLE",
    team == "HST" ~ "HOU",
    team == "SL" ~ "LA",
    team == "OAK" ~ "LV",
    team == "SD" ~ "LAC",
    TRUE ~ team
  )
  )
nfl_rosters <- convert_team_abbreviation(nfl_rosters, team_conversion)

nfl_teams <- load_or_cache_data(
  load_fun = nflreadr::load_teams,
  file_stub = "teams",
  seasons = NULL,
  refresh_seasons = NULL
)

nfl_schedules_origin <- load_or_cache_data(
  load_fun = nflreadr::load_schedules,
  file_stub = "schedules",
  seasons = min_data_season:this_season,
  refresh_seasons = this_season
)
nfl_games <- nfl_schedules_origin |> mutate(gameday = ymd(gameday))


# Simplifying Data --------------------------------------------------------

nfl_qbs <- nfl_rosters |> filter(position == "QB")

#unique nfl qb's

nfl_qb_seasons <- nfl_qbs |> 
  # filter(!is.na(gsis_id)) |> 
  group_by(gsis_id, gsis_it_id) |> 
  fill(draft_number, .direction = "downup") |> 
  fill(draft_club, .direction = 'downup') |> 
  fill(birth_date , .direction = 'downup') |> 
  fill(height, .direction = 'downup') |> 
  fill(weight, .direction = 'downup') |> 
  fill(entry_year, .direction = 'downup') |> 
  fill(rookie_year, .direction = 'downup') |> 
  mutate(full_name = full_name[1]) |> 
  ungroup() |> 
  group_by(qb_gsis_id = gsis_id,
           qb_gsis_it_id = gsis_it_id,
           season,
           full_name,
           birth_date,
           draft_number,
           entry_year,
           rookie_year
  ) |> 
  summarize(team = paste0(team, collapse = "/"),
            height = mean(height),
            weight = mean(weight),
            .groups = 'drop'
  ) |> 
  mutate(birth_date = as.Date(birth_date)) |> 
  rename(draft_year = entry_year) |> 
  mutate(undrafted = ifelse(is.na(draft_number), 1, 0),
         draft_number = coalesce(draft_number, max(draft_number, na.rm = TRUE) ),
         season_age = time_length(ymd(paste0(season, "-09-01")) - birth_date, 'year') 
  ) |> 
  arrange(birth_date,
          season)

qb_starts <- nfl_games |> 
  count(season, away_qb_id) |> 
  rename(qb_gsis_id = away_qb_id) |> 
  bind_rows(nfl_games |> 
              count(season, home_qb_id) |> 
              rename(qb_gsis_id = home_qb_id)) |> 
  group_by(season, qb_gsis_id) |> 
  summarize(starts = sum(n),
            .groups = 'drop') |> 
  arrange(season, desc(starts))

nfl_qb_seasons <- nfl_qb_seasons |> 
  left_join(qb_starts, by = c("qb_gsis_id", "season")) |> 
  mutate(starts = replace_na(starts, 0))

nfl_qb_seasons <- nfl_qb_seasons |> 
  arrange(qb_gsis_id, qb_gsis_it_id, season) |> 
  mutate(draft_number = as.numeric(draft_number),
         undrafted = ifelse(is.na(draft_number) | is.na(draft_year), 1, 0),
         draft_number = ifelse(undrafted == 1, max(draft_number, na.rm = TRUE) + 1, draft_number),
         min_season = model_start_season
  ) |> 
  group_by(qb_gsis_id, qb_gsis_it_id) |> 
  mutate(qb_first_season = ifelse(row_number() == 1, 1, 0),
         qb_rookie_year = ifelse((rookie_year >= min_season | is.na(rookie_year)) & qb_first_season == 1, 1, 0),
         starts_last_season = lag(starts, default = 0)
  ) |> 
  ungroup()


# ## only keep qb's who have started a game or are on 2024 roster
# nfl_qb_seasons <- nfl_qb_seasons |> 
#   filter(starts > 0 | season == this_season)

nfl_qb_seasons <- nfl_qb_seasons |> 
  mutate(qb_season_ind = 1:n() )
#
qb_draft_basis <- bs(nfl_qb_seasons$draft_number, df = 3)
qb_draft_basis_col <- ncol(qb_draft_basis)



# Rushing QB Clustering ---------------------------------------------------

#use past data to get an idea who is a runner & use college data for new people
cfb_qb_usage <- NULL
for(s in 2013:(max(all_seasons) - 1)){
  cfb_qb_usage <- cfbfastR::cfbd_player_usage(year = s) |> 
    filter(position == "QB") |> 
    bind_rows(cfb_qb_usage)  
  cat("read in cfb qb usage data from ", s, "\n")
}
#fix bo nix id
cfb_qb_usage <- cfb_qb_usage |> 
  #replace bo nix auburn id with his oregon one.
  mutate(athlete_id = ifelse(athlete_id == "4567218", "4426338", athlete_id)
  ) |> 
  mutate(across(starts_with("usg_"), ~ as.numeric(.x)))
#max college qb rush season
cfb_qb_usage_max_rush <- cfb_qb_usage %>%
  arrange(name, season) %>%
  group_by(name) %>%
  mutate(
    prev_athlete_id = lag(athlete_id),
    prev_season = lag(season),
    # Always start a new group if it's the first row OR athlete_id changes across non-consecutive seasons
    is_new_group = if_else(
      is.na(prev_season) | is.na(prev_athlete_id) | athlete_id != prev_athlete_id & (season - prev_season > 1), TRUE, FALSE)
    ) |> 
  ungroup() |> 
  mutate(
    name_group_id = cumsum(is_new_group)
  ) %>%
  group_by(name, name_group_id) %>%
  mutate(max_cfb_season = max(season)) %>%
  arrange(name, name_group_id, desc(usg_rush), desc(season)) %>%
  slice(1) %>%
  ungroup()

temp <- cfbfastR::cfbd_stats_season_player(year = 2024)

cfb_player_stats <- load_or_cache_data(
  load_fun = cfbfastR::cfbd_stats_season_player,
  file_stub = "college_season_player_stats",
  seasons = 2013:(max(all_seasons) - 1),
  refresh_seasons = NULL
)

cfb_player_stats_passing <- cfb_player_stats |> 
  filter(passing_att > 10) |> 
  dplyr::select(year:rushing_long)

#join season with most passing attempts to cfb_qb_usage_max_rush
cfb_qb_max_usage <- cfb_qb_usage_max_rush |> 
  left_join(cfb_player_stats_passing |> 
              dplyr::select(year,
                            athlete_id,
                            passing_att,
                            passing_cmp = passing_completions,
                            passing_pct,
                            passing_yds,
                            passing_td,
                            passing_int,
                            passing_ypa,
                            rushing_att = rushing_car,
                            rushing_yds,
                            rushing_td,
                            rushing_ypc,
                            rushing_long),
            by = c("max_cfb_season" = "year", "athlete_id" = "athlete_id")
  ) |> 
  mutate(rush_pct = rushing_att / (rushing_att + passing_att)
  )

cfb_qb_max_usage <- cfb_qb_max_usage |> 
  filter(passing_att >= 100) |> 
  #replace NA values from passing_att to rush_pct with 0
  mutate(across(c(passing_att:rush_pct), ~ replace_na(.x, 0))) |> 
  dplyr::select(-prev_athlete_id, -prev_season, -is_new_group, -name_group_id, -max_cfb_season) |> 
  mutate(conference = ifelse(is.na(conference) | nchar(conference) ==0 , "FCS", conference))


## if this returns anything then need to fix cfb id's
nfl_qb_seasons <- nfl_qb_seasons |>
  left_join(cfb_qb_max_usage |>
              dplyr::select(cfb_247_id = athlete_id,
                            name,
                            cfb_team = team,
                            conference,
                            usg_overall:rush_pct),
            by = c("full_name" = "name")
  ) |> 
  group_by(qb_rookie_year) |> 
  mutate(qb_have_cfb_data = ifelse(!is.na(cfb_team), 1, 0),
         # cfb_usg_rush = coalesce(cfb_usg_rush, mean(cfb_usg_rush, na.rm = TRUE))
         ) |> 
  ungroup()

nfl_qb_w_cfb_data <- nfl_qb_seasons |> 
  filter(draft_year >= 2014,
         !is.na(cfb_team)
         ) |> 
  group_by(qb_gsis_id) |> 
  mutate(rookie_age = min(season_age, na.rm = TRUE)
         ) |>
  ungroup() |> 
  #if missing all ages make rookie age the average rookie age
  mutate(rookie_age = ifelse(abs(rookie_age) == Inf | is.na(rookie_age), 
                             mean(rookie_age[abs(rookie_age) != Inf], na.rm = TRUE),
                             rookie_age))

#get second contract for each qb
second_contract_apy <- nfl_contracts_by_season |> 
  filter(position == "QB") |>
  group_by(gsis_id) |> 
  filter(rookie_deal == 0) |> 
  filter(contract_id == min(contract_id)) |> 
  group_by(gsis_id, contract_id, player, college) |> 
  summarize(apy_cap_pct = mean(apy_cap_pct),
            first_contract_season = min(contract_season),
            .groups = 'drop')
  

## add second contract information
#
data_to_model <- second_contract_apy |> 
  inner_join(nfl_qb_w_cfb_data |> 
               group_by(qb_gsis_id) |> 
               filter(season == min(season)) |> 
               ungroup(),
             by = c("gsis_id" = "qb_gsis_id")
             ) |> 
  dplyr::select(-any_of(c(
    "season_age",
    "starts", 
    "min_season",
    "qb_first_season",
    "qb_rookie_year",
    "starts_last_season",
    "qb_season_ind",
    "qb_have_cfb_data"
  ))) |> 
  dplyr::select(gsis_id, 
                player, 
                college,
                conference,
                birth_date,
                draft_year, 
                draft_number,
                undrafted,
                apy_cap_pct,
                height,
                weight,
                rookie_age,
                usg_overall:rush_pct
  )



# Random Forest Model -----------------------------------------------------


# --- Data Split not needed since we want metrics on the whole dataset using CV ---
# Define the recipe
rf_recipe <- recipe(apy_cap_pct ~ ., data = data_to_model) |>
  update_role(gsis_id, player, college, birth_date, draft_number, new_role = "ID") |>
  step_rm(gsis_id, player, college, birth_date) |>
  step_log(apy_cap_pct, base = exp(1), skip = FALSE) |>  # log transform the outcome
  step_dummy(all_nominal_predictors()) |>
  step_zv(all_predictors()) |>
  step_normalize(all_numeric_predictors())

# Random forest model spec
rf_spec <- rand_forest(mtry = tune(), min_n = tune(), trees = 500) |>
  set_mode("regression") |>
  set_engine("ranger", importance = "permutation")

# Workflow
rf_workflow <- workflow() |>
  add_model(rf_spec) |>
  add_recipe(rf_recipe)

# Cross-validation folds
folds <- vfold_cv(data_to_model, v = 5)

# Tune model
rf_tune <- tune_grid(
  rf_workflow,
  resamples = folds,
  grid = 10,
  metrics = metric_set(rmse, mae, rsq)
)

# Select best parameters
best_params <- select_best(rf_tune, metric = "rmse")

# Finalize workflow
final_rf_workflow <- finalize_workflow(rf_workflow, best_params)

# === Fit resamples to get out-of-fold predictions ===
cv_fit <- fit_resamples(
  final_rf_workflow,
  resamples = folds,
  control = control_resamples(save_pred = TRUE)
)

# --- Collect out-of-fold predictions ---
cv_preds <- collect_predictions(cv_fit) |> 
  mutate(.pred = exp(.pred),
         apy_cap_pct = exp(apy_cap_pct))  # back-transform predictions

# Join with original data (if needed for player names)
cv_preds <- cv_preds |>
  left_join(data_to_model |> 
              dplyr::mutate(rowid = 1:n()) |> 
              dplyr::select(rowid, player), by = c(".row" = "rowid"))

# === Evaluation Metrics ===
cv_metrics <- cv_preds |> metrics(truth = apy_cap_pct, estimate = .pred)
cv_bias <- cv_preds |> summarise(bias = mean(.pred - apy_cap_pct))

# === Plot Predicted vs Actual ===
cv_preds |>
  ggplot(aes(y = apy_cap_pct, x = .pred)) +
  geom_point(alpha = 0.7) +
  geom_smooth(method = 'glm', color = 'red', se = FALSE) +
  geom_abline(slope = 1, intercept = 0, color = "black", 2) +
  labs(title = "Out-of-Fold Predicted vs Actual apy_cap_pct", y = "Actual", x = "Predicted") + 
  theme_bw() + 
  theme(aspect.ratio = 1) +
  xlim(0, 0.25) + ylim(0, 0.25)

# === Biggest Over/Under Predictions ===
cv_preds |> 
  mutate(error = .pred - apy_cap_pct, abs_error = abs(error)) |>
  arrange(desc(abs_error)) |> 
  select(player, apy_cap_pct, .pred, error) |> 
  head(10)

# === Variable Importance Plot ===
final_rf_fit <- fit(final_rf_workflow, data = data_to_model)

final_rf_fit |> 
  extract_fit_parsnip() |>
  vip(num_features = 20)

# === Output metrics ===
cv_metrics
cv_bias





# Model Results ----------------------------------------------------------





