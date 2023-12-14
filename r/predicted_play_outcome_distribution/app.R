#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(shinyWidgets)
library(nflfastr)
library(xgboost)

nfl_teams <- nflreadr::load_teams()

nfl_teams_div <- nfl_teams %>% 
  arrange(team_conf,team_division, team_abbr) %>% 
  dplyr::select(team = team_abbr, 
                team_division) 
nfl_teams_div_list <- split(nfl_teams_div$team, nfl_teams_div$team_division)

## load xgboost models
xgb.load("../models/to_model_all_season.model")
xgb.load("../models/to_yrdline_all_season.model")
xgb.load("../models/no_to_yrdline_all_season.model")


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # First row of widgets -- 12 grid units per row
    fluidRow(
      column(2,
             pickerInput("posteam",
                         label = "Poss. Team",
                         choices = nfl_teams_div_list,
                         selected = 'BUF')
      ),
      column(2,
             sliderInput("yardline_100",
                         "Yards from Endzone",
                         min = 1,
                         max = 100,
                         value = 75)
             ),
      column(2,
             selectInput("down",
                         "Down",
                         choices = 1:4,
                         selected = 1)
      ),
      column(2,
             sliderInput("distance",
                         "Distance",
                         min = 1,
                         max = 25,
                         value = 10)
      ),
      column(2,
             sliderInput("play_clock",
                         "Play Clock",
                         min = 1,
                         max = 40,
                         value = 25)
      ),
      column(2,
             selectInput("location",
                         "Home/Away/Neutral",
                         choices = c('home','away','neutral'),
                         selected = "home")
      ),
    ),
    
    # Second row of widgets -- 12 grid units per row
    fluidRow(
      column(2,
             numericInput("team_score",
                         "Team Score",
                         value = 0,
                         min = 0,
                         max = 75)
      ),
      column(2,
             numericInput("opp_score",
                          "Opp Score",
                          value = 0,
                          min = 0,
                          max = 75)
             ),
      column(2,
             sliderInput("team_timeouts",
                         "Team T.O.",
                         min = 0,
                         max = 3,
                         value = 3)
      ),
      column(2,
             sliderInput("opp_timeouts",
                         "Opp T.O.",
                         min = 0,
                         max = 3,
                         value = 3)
             )
    ),
    
    # Third row of widgets -- 12 grid units per row
    fluidRow(
      column(2,
             checkboxInput("qb_dropback",
                           "Pass Play",
                           value = FALSE)
      ),
      column(2,
             selectInput("half",
                         "Half",
                         choices = c("1","2","OT"),
                         selected = "1")
      ),
      column(2,
             sliderInput("minutes_rem",
                         "Minutes",
                         min = 0,
                         max = 15,
                         value = 15)
      ),
      column(2,
             numericInput("seconds_rem",
                          "Seconds",
                          value = 0,
                          min = 0,
                          max = 60)
             ),
    ),
       # Fourth row of widgets -- 12 grid units per row
    fluidRow(
      column(2,
             checkboxInput("outdoors",
                           "Outdoors Stadium",
                           value = FALSE)
      ),
      column(2,
             checkboxInput("grass",
                           "Grass Field",
                           value = FALSE)
             ),
      column(2,
             sliderInput("temp",
                         "Temperature",
                         min = 20,
                         max = 100,
                         value = 75)
      ),
      column(2,
             sliderInput("wind",
                         "Wind (mph)",
                         min = 0,
                         max = 30,
                         value = 5)
             )
      ),
  
    
    # Show Plot
    fluidRow(
      plotOutput("distPlot")
      
    )
    
    
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  #take inputs and create data to use into model
  home <- ifelse(tolower(input$location) == 'home', 1, 0)
  away <- ifelse(tolower(input$location) == 'away', 1, 0)
  neutral <- ifelse(tolower(input$location) == 'neutral', 1, 0)
  qb_dropback <- ifelse(input$qb_dropback, 1, 0)
  ## temperature, wind, outdoors, & grass field
  outdoors_stadium <- ifelse(input$outdoors, 1, 0)
  surface_grass <- ifelse(input$grass, 1, 0)
  temp <- case_when(outdoors_stadium == 1, input$temp, -99)
  wind <- case_when(outdoors_stadium == 1, input$wind, -99)
  #down/distance
  down <- input$down
  ydstogo <- input$distance
  #current score
  current_tot_score <- input$team_score + input$opp_score
  current_score_diff <- input$team_score - input$opp_score
  #timeouts
  team_timeouts <- input$team_timeouts
  opp_timeouts <- input$opp_timeouts
  #clock
  half <- ifelse(input$half %in% c("1", "2"), as.numeric(input$half), 3)
  half_seconds_remaining <- case_when(half == 3 & input$minutes_rem >= 10 ~ 10*60,
                                      input$minutes_rem == 15 ~ input$minutes_rem*60,
                                      TRUE ~ input$minutes_rem*60 + input$seconds_rem
                                      )
  play_clock <- input$play_clock
  
  #posteam & home_team
  # 
  posteam <- input$posteam
  home_team <- case_when(home == 1 ~ posteam,
                         away == 1 ~ 'IND',#pretend the home_team is Colts (dome team) for now if it's not buffalo
                         neutral == 1 ~ posteam)
  
  

# Data Wrangling Predicted Distributions ----------------------------------

  
  #define all variables into one matrix/tibble
  input_tibble <- tibble(home,
                         away,
                         neutral,
                         qb_dropback,
                         ## ,
                         outdoors_stadium,
                         surface_grass,
                         temp,
                         wind,
                         #down,
                         down,
                         ydstogo,
                         #current,
                         current_tot_score,
                         current_score_diff,
                         #timeouts,
                         team_timeouts,
                         opp_timeouts,
                         #clock,
                         half,
                         half_seconds_remaining,
                         play_clock,
                         #teams
                         posteam,
                         home_team
  )
  
  
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
    mutate(ep = ifelse(yardline_100 == 0, 6.95, ep),#td value
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
  
  # Use approx to interpolate
  quantile_values <- approx(x = next_play_distribution$cdf_val,
                            y = next_play_distribution$epa,
                            xout = quantiles_to_save) %>% 
    bind_rows() %>% 
    rename(quantile = x,
           epa = y)
  #function saved to calculate quantile for this play
  impute_epa_quantil_fun <- approxfun(y = next_play_distribution$cdf_val,
                                      x = next_play_distribution$epa)
  
  ## combine output play outcome distributions & save
  predict_play_dist$play_quantile <- impute_epa_quantil_fun(predict_play_dist$epa)
  
  

# Plot Code ---------------------------------------------------------------

  
  output$distPlot <- renderPlot({

      
      
        # draw the histogram with the specified number of bins
      
      
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
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
