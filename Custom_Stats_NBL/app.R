library(shiny)
library(tidyverse)
library(shinydashboard)
library(writexl)

# Read in data
player_df <- read_csv("supercoach-data.csv")
season_schedule <- read_rds("season_schedule_2023_2024.rds")

actions <- c("Dunk", "Screen", "Technical Foul", "Deep Three")

ui <- dashboardPage(
  dashboardHeader(title = "Player Actions Tracker"),
  dashboardSidebar(
    sidebarMenu(
      selectInput('round', 'Select Round', unique(season_schedule$round)),
      uiOutput('match_dropdown'),
      uiOutput('team_dropdown'),
      uiOutput('player_dropdown'),
      actionButton("start_button", "Start Voice Recognition")  # Button to initiate voice recognition
      
    )
  ),
  dashboardBody(
    tags$head(
      tags$script(src = "voice_recognition.js")  # Include the JavaScript for voice recognition
    ),
    fluidRow(
      column(3, 
             div(uiOutput("buttons"), style = "text-align: center;"),
             br(),  # A line break for some spacing
      ),
      column(9, 
             downloadButton('downloadData', 'Export to Excel'), 
             tableOutput("table")
      )
    )
  )
)

server <- function(input, output, session) {
  
  output$match_dropdown <- renderUI({
    if (!is.null(input$round)) {
      available_matches <- season_schedule[season_schedule$round == input$round, "match"]
      selectInput('match', 'Select Match', available_matches)
    } else {
      return(NULL)
    }
  })
  
  output$team_dropdown <- renderUI({
    if (!is.null(input$match)) {
      selected_match <- season_schedule[season_schedule$match == input$match, c("home_team", "away_team")]
      selectInput('team', 'Select Team', c(selected_match$home_team, selected_match$away_team), multiple = FALSE)
    } else {
      return(NULL)
    }
  })
  
  output$player_dropdown <- renderUI({
    if (!is.null(input$team)) {
      available_players <- player_df[player_df$player_team %in% input$team, "player_name"]
      selectInput('player', 'Select Player', available_players)
    } else {
      return(NULL)
    }
  })
  
  player_data <- reactiveVal(data.frame(Team = character(0), Player = character(0), Action = character(0), Count = numeric(0)))
  
  output$buttons <- renderUI({
    do.call(tagList, lapply(actions, function(action) {
      div(
        style = "text-align: center; margin-bottom: 10px;",  # Add some margin for spacing between action rows
        tags$span(paste(action, "for", input$player)),
        actionButton(paste0(input$player, action, "Plus"), label = "+", style = "margin-left: 5px;"),
        actionButton(paste0(input$player, action, "Minus"), label = "-", style = "margin-left: 5px;")
      )
    }))
  })
  
  parse_voice_command <- function(command) {
    selected_match <- season_schedule[season_schedule$match == input$match,]
    home_team <- selected_match$home_team
    away_team <- selected_match$away_team
    players <- unique(player_df$player_name)
    actions <- c("increase dunk", "decrease dunk")  # Add other actions accordingly
    
    team_detected <- NULL
    player_detected <- NULL
    action_detected <- NULL
    
    # Detecting "home" or "away" in the command
    if (str_detect(command, "home")) {
      team_detected <- home_team
    } else if (str_detect(command, "away")) {
      team_detected <- away_team
    }
    
    for (player in players) {
      if (str_detect(command, player)) {
        player_detected <- player
        break
      }
    }
    
    for (action in actions) {
      if (str_detect(command, action)) {
        action_detected <- action
        break
      }
    }
    
    list(team = team_detected, player = player_detected, action = action_detected)
  }
  
  observeEvent(input$voice_command, {
    command <- input$voice_command
    
    parsed_command <- parse_voice_command(command)
    
    if (!is.null(parsed_command$team) && !is.null(parsed_command$player) && !is.null(parsed_command$action)) {
      # Update the dropdowns to reflect the spoken team and player
      updateSelectInput(session, "team", selected = parsed_command$team)
      updateSelectInput(session, "player", selected = parsed_command$player)
      
      action_id <- NULL
      if (parsed_command$action == "increase dunk") {
        action_id <- paste0(parsed_command$player, "DunkPlus")
      } else if (parsed_command$action == "decrease dunk") {
        action_id <- paste0(parsed_command$player, "DunkMinus")
      } 
      # Continue for other actions...
      
      if (!is.null(action_id)) {
        # Increment the button's value to simulate a click
        updateActionButton(session, action_id, value = input[[action_id]] + 1)
      }
    }
  })
  
  
  lapply(actions, function(action) {
    
    # Handle the "+" button
    observeEvent(input[[paste0(input$player, action, "Plus")]], {
      data <- player_data()
      row_idx <- which(data$Player == input$player & data$Action == action)
      
      if (length(row_idx) == 0) {
        new_row <- data.frame(
          Round = input$round,
          Match = input$match,
          Team = input$team,
          Player = input$player,
          Action = action,
          Count = 1
        )
        data <- rbind(data, new_row)
      } else {
        data[row_idx, "Count"] <- data[row_idx, "Count"] + 1
      }
      
      player_data(data)
    }, ignoreNULL = TRUE)
    
    # Handle the "-" button
    observeEvent(input[[paste0(input$player, action, "Minus")]], {
      data <- player_data()
      row_idx <- which(data$Player == input$player & data$Action == action)
      
      if (length(row_idx) > 0) {
        if (data[row_idx, "Count"] > 1) {
          # Decrease the count if it's greater than 1
          data[row_idx, "Count"] <- data[row_idx, "Count"] - 1
        } else {
          # Remove the row if the count is 1 (it will become 0 after decrement)
          data <- data[-row_idx, ]
        }
      }
      
      player_data(data)
    }, ignoreNULL = TRUE)
  })
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("PlayerActionCounts_", Sys.Date(), "_", format(Sys.time(), "%H%M%S"), ".xlsx", sep = "")
    },
    content = function(file) {
      write_xlsx(player_data(), file)
    }
  )
  
  
  output$table <- renderTable({
    player_data()
  })
}

shinyApp(ui, server)
