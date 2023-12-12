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
  
  output$distPlot <- renderPlot({

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
      
      ## convert to xgboost data thing
      dpred <- xgb.DMatrix(data = as.matrix(input_tibble)
                           )
      
      predictions <- predict(xg_yardline_classification_model,
                             reshape = TRUE,
                             dpred)
      
        # draw the histogram with the specified number of bins
      plot(predictions[1,], type = 'h')
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
