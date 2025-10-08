library(tidyverse)
library(CleatRTools)

source("r/convert_team_abbreviation.R")
set_nfl_aws_creds()

# all_playcallers_old <- read_csv("../data/all_playcallers.csv")
all_playcallers_old<- aws.s3::s3read_using(FUN = read_csv, bucket = nfl_models_bucket_name, object = "data/all_playcallers.csv")
all_playcallers_new <- read_csv("../data/all_playcallers_new.csv")

# get all existing season, game_id, team, week combinations 
nfl_schedule <- nflreadr::load_schedules()
#keep only season, game_id, week, away_team, and home_team then pivot longer the teams
# nfl_schedule_long <- nfl_schedule |> 
#   select(season, game_id, week, away_team, home_team) |> 
#   pivot_longer(cols = c(away_team, home_team), 
#                names_to = "team_type", 
#                values_to = "team") |> 
#   select(-team_type) |> 
#   distinct()
# nfl_schedule_long <- convert_team_abbreviation(nfl_schedule_long, team_conversion)
# #and join on existing playcaller data. then write missing play callers to csv and manually fill in
# playcallers_to_fill <- nfl_schedule_long |> 
#   anti_join(all_playcallers_old,
#             by = c("season", "game_id", "team", "week")
#   ) |> 
#   mutate(off_play_caller = NA_character_,
#          def_play_caller = NA_character_,
#          head_coach = NA_character_) |> 
#   select(season, team, game_id, off_play_caller, def_play_caller, week, head_coach)
# playcallers_to_fill |> 
#   arrange(team, season, week) |> write_csv("all_playcallers_new.csv")

#adjust names of teams in the new dataset
all_playcallers_old <- convert_team_abbreviation(all_playcallers_old, team_conversion)
all_playcallers_new <- convert_team_abbreviation(all_playcallers_new, team_conversion)
#fix some still missing ones
all_playcallers |> filter(is.na(head_coach)) |> count(season, team)


#add in head coaches

all_playcallers <- all_playcallers_old |> 
  left_join(all_playcallers_new |> 
              select(season,
                     game_id,
                     team,
                     week,
                     head_coach), 
            by = c("season", "game_id", "team")
            )

all_playcallers |> filter(is.na(head_coach))

#find values with now matches
all_playcallers_additions <-  all_playcallers_new |> 
  anti_join(all_playcallers |> 
              select(season,
                     game_id,
                     team,
                     head_coach), 
            by = c("season", "game_id", "team")
  )
#add new values in

all_playcallers <- all_playcallers |> 
  bind_rows(all_playcallers_additions) |> 
  arrange(season,
          team,
          week)

# all_playcallers |> write_csv("../data/all_playcallers.csv")
all_playcallers |> aws.s3::s3write_using(FUN = write_csv, bucket = nfl_models_bucket_name, object = "data/all_playcallers.csv")

## write to sharepoint as well
# all_playcallers

sharepoint_root <- paste0("/NFL/", max(all_playcallers$season))
# Upload to SharePoint -- -FIX this function to work with ".png"!!ß
save_sharepoint(
  data = all_playcallers,
  file_name = "all_playcallers.csv",
  folder_path = sharepoint_root
)
