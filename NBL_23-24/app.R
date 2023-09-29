##%######################################################%##
#                                                          #
####                       Set up                       ####
#                                                          #
##%######################################################%##

# Libraries---------------------------------------------------------------------
library(shiny)
library(shinythemes)
library(tidyverse)
library(DT)
library(httr2)
library(nblR)
library(googlesheets4)
library(googledrive)

`%notin%` <- Negate(`%in%`)

# Google sheets authentication -------------------------------------------------
options(gargle_oauth_cache = ".secrets")
drive_auth(cache = ".secrets", email = "cuzzy.punting@gmail.com")
gs4_auth(token = drive_token())

# Load functions----------------------------------------------------------------
source("data_functions.R")

# Data--------------------------------------------------------------------------
combined_stats_table <- get_historical_data()
supercoach_data <- get_supercoach_data()
season_schedule_2023_2024 <- read_rds("season_schedule_2023_2024.rds")

# Google Sheets Data------------------------------------------------------------
ss_name <- gs4_find("NBL Data")
h2h_data <- read_sheet(ss = ss_name, sheet = "H2H")
player_points_data <- read_sheet(ss = ss_name, sheet = "Player Points")
player_assists_data <- read_sheet(ss = ss_name, sheet = "Player Assists")
player_rebounds_data <- read_sheet(ss = ss_name, sheet = "Player Rebounds")

# Arrange player props data by biggest discrepancy------------------------------

# Points
player_points_data <-
  player_points_data |> 
  group_by(player_name, match, line) |>
  mutate(odds_high = max(over_price), odds_low = min(over_price)) |> 
  mutate(diff = 1/odds_low - 1/odds_high) |>
  arrange(desc(diff), player_name, desc(over_price), line, match) |> 
  select(-diff, -odds_high, -odds_low)

# Assists
player_assists_data <-
  player_assists_data |> 
  group_by(player_name, match, line) |>
  mutate(odds_high = max(over_price), odds_low = min(over_price)) |> 
  mutate(diff = 1/odds_low - 1/odds_high) |>
  arrange(desc(diff), player_name,desc(over_price), line, match) |> 
  select(-diff, -odds_high, -odds_low)

# Rebounds
player_rebounds_data <-
  player_rebounds_data |> 
  group_by(player_name, match, line) |>
  mutate(odds_high = max(over_price), odds_low = min(over_price)) |> 
  mutate(diff = 1/odds_low - 1/odds_high) |>
  arrange(desc(diff), player_name, desc(over_price), line, match) |> 
  select(-diff, -odds_high, -odds_low)

# Add player empirical probabilities from 2022-2023 season----------------------

# Points
get_player_points_emp_prob <- function(line_value) {
  get_emp_prob(combined_stats_table,
               player_points_data$player_name,
               line = line_value,
               player_points) |> 
    mutate(line = line_value)
}

player_points_data <-
  player_points_data |> 
  mutate(implied_prob_over = round(1/over_price, 3)) |> 
  left_join(map(unique(player_points_data$line), get_player_points_emp_prob) |> bind_rows()) |> 
  mutate(diff = round(emp_prob_over - implied_prob_over, 3)) |> 
  select(-games_played)

# Assists
get_player_assists_emp_prob <- function(line_value) {
  get_emp_prob(combined_stats_table,
               player_assists_data$player_name,
               line = line_value,
               player_assists) |> 
    mutate(line = line_value)
}

player_assists_data <-
  player_assists_data |> 
  mutate(implied_prob_over = round(1/over_price, 3)) |> 
  left_join(map(unique(player_assists_data$line), get_player_assists_emp_prob) |> bind_rows()) |> 
  mutate(diff = round(emp_prob_over - implied_prob_over, 3)) |> 
  select(-games_played)

# Rebounds
get_player_rebounds_emp_prob <- function(line_value) {
  get_emp_prob(combined_stats_table,
               player_rebounds_data$player_name,
               line = line_value,
               player_rebounds_total) |> 
    mutate(line = line_value)
}

player_rebounds_data <-
  player_rebounds_data |> 
  mutate(implied_prob_over = round(1/over_price, 3)) |> 
  left_join(map(unique(player_rebounds_data$line), get_player_rebounds_emp_prob) |> bind_rows()) |> 
  mutate(diff = round(emp_prob_over - implied_prob_over, 3)) |> 
  select(-games_played)

# Get new category for player arbs----------------------------------------------

# Points-----------------------------------------------------------------------

# Unders
player_points_data_unders <-
  player_points_data |> 
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  rename(under_agency = agency)

# Overs
player_points_data_overs <-
  player_points_data |> 
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  rename(over_agency = agency)

# Full Join
player_points_arbs <-
  player_points_data_unders |> 
  full_join(player_points_data_overs, relationship = "many-to-many") |> 
  mutate(margin = 1/over_price + 1/under_price) |> 
  mutate(margin = 100*(1 - margin)) |>
  filter(margin > 0) |>
  arrange(desc(margin))
  
# Assists-----------------------------------------------------------------------

# Unders
player_assists_data_unders <-
  player_assists_data |> 
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  rename(under_agency = agency)

# Overs
player_assists_data_overs <-
  player_assists_data |> 
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  rename(over_agency = agency)

# Full Join
player_assists_arbs <-
  player_assists_data_unders |> 
  full_join(player_assists_data_overs, relationship = "many-to-many") |> 
  mutate(margin = 1/over_price + 1/under_price) |> 
  mutate(margin = 100*(1 - margin)) |>
  filter(margin > 0) |>
  arrange(desc(margin))


# Rebounds----------------------------------------------------------------------

# Unders
player_rebounds_data_unders <-
  player_rebounds_data |> 
  filter(!is.na(under_price)) |> 
  select(-over_price) |> 
  rename(under_agency = agency)

# Overs
player_rebounds_data_overs <-
  player_rebounds_data |> 
  filter(!is.na(over_price)) |> 
  select(-under_price) |> 
  rename(over_agency = agency)

# Full Join
player_rebounds_arbs <-
  player_rebounds_data_unders |> 
  full_join(player_rebounds_data_overs, relationship = "many-to-many") |> 
  mutate(margin = 1/over_price + 1/under_price) |> 
  mutate(margin = 100*(1 - margin)) |>
  filter(margin > 0) |>
  arrange(desc(margin))  

# Player names------------------------------------------------------------------
player_names <-
combined_stats_table |>
    mutate(player_full_name = paste(first_name, family_name)) |> 
    summarise(total_points = sum(player_points, na.rm = TRUE), .by = player_full_name) |> 
    arrange(desc(total_points)) |> 
    filter(player_full_name != "NA NA") |>
    distinct(player_full_name) |> 
    pull(player_full_name)

# Create function to filter and display player data
display_player_stats <- function(player_name, season_name, n_games) {
    combined_stats_table |>
        mutate(player_full_name = paste(first_name, family_name)) |>
        filter(player_full_name == player_name) |>
        filter(season == season_name) |>
        arrange(desc(match_time_utc)) |> 
        select(player_full_name,
               season,
               round_number,
               match_time_utc,
               team = name,
               opposition = opp_name,
               home_away,
               starter,
               player_points,
               player_rebounds_total,
               player_assists,
               player_steals,
               player_blocks,
               player_minutes
               ) |> 
        slice_head(n = n_games)
}

# Function to plot hit rate for prop lines
display_empirical_probabilities <-
    function(data, stat, line) {
        # Create Label variable
        if (stat == "player_points") {
            label = "Points"
        }
        else if (stat == "player_rebounds") {
            label = "Rebounds"
        }
        else {
            label = "Assists"
        }
        
        # Stat as symbol
        stat <- rlang::sym(stat)
        
        # number of games
        num_games = nrow(data)
        
        # Create data
        dat <- data %>%
            mutate(over_line = if_else(!!stat >= line, TRUE, FALSE)) %>%
            mutate(num_games_ago = row_number())
        
        # Hit rate vars
        hit_rate = paste0(round(mean(dat$over_line) * 100, 2), "%")
        
        # Implied Odds Var
        implied_odds = round(1/mean(dat$over_line), 3)
        
        # Player name var
        name_var = dat$player_full_name[1]
        
        # Create plot
        dat %>%
            ggplot(aes(
                x = num_games_ago,
                y = !!stat,
                color = over_line
            )) +
            geom_point(alpha = 0.8, size = 4) +
            geom_line(aes(group = 1), color = "black") +
            geom_hline(
                yintercept = line,
                linetype = "dashed",
                color = "black",
                linewidth = 1.5
            ) +
            scale_color_manual(values = c("TRUE" = "green", "FALSE" = "red")) +
            scale_x_reverse(breaks = seq(num_games, 0,-1)) +
            labs(
                title = paste0("Player Performance: ", name_var),
                subtitle = paste0("Hit Rate: ", hit_rate, " -> Implied Odds: ", implied_odds),
                x = "Number of Games Ago",
                y = label,
                color = "Over Line"
            ) +
            theme_minimal() +
            theme(
                plot.title = element_text(hjust = 0.5),
                legend.position = "bottom",
                legend.title = element_text(face = "bold")
            )
    }

# Create function to analyse performance with and without a team-mate-----------
with_without <- function(player_name, team_mate, season_name) {
    # Full table
    full_table <-
    combined_stats_table |>
        mutate(player_full_name = paste(first_name, family_name)) |>
        filter(season == season_name)
    
    # Player games played
    player_games <-
    full_table |> 
        filter(player_full_name == player_name) |>
        select(match_id, round_number, name, opp_name)
    
    # Team mate games played
    team_mate_games <-
    full_table |> 
        filter(player_full_name == team_mate) |>
        select(match_id, round_number, name, opp_name)
    
    # Player with teammate
    both <-
    full_table |> 
        filter(match_id %in% player_games$match_id & match_id %in% team_mate_games$match_id) |>
        filter(player_full_name == player_name) |>
        group_by(player_full_name) |>
        summarise(ppg = mean(player_points), apg = mean(player_assists), rpg = mean(player_rebounds_total), games = n()) |>
        mutate(teammate = team_mate, with_teammate = TRUE)
        
    
    # Player without teammate
    just_player <-
        full_table |> 
        filter(match_id %in% player_games$match_id & match_id %notin% team_mate_games$match_id) |>
        filter(player_full_name == player_name) |> 
        group_by(player_full_name) |>
        summarise(ppg = mean(player_points), apg = mean(player_assists), rpg = mean(player_rebounds_total), games = n()) |>
        mutate(teammate = team_mate, with_teammate = FALSE)
    
    # Combine and output
    bind_rows(both, just_player) |>
        relocate(teammate, with_teammate, .after =  player_full_name)
}

##%######################################################%##
#                                                          #
####                        App                         ####
#                                                          #
##%######################################################%##


##%######################################################%##
#                                                          #
####                       UI                            ####
#                                                          #
##%######################################################%##

ui <- fluidPage(
  titlePanel("NBL Market Analysis"),
  theme = shinytheme("united"),
  tabsetPanel(
    # First tab for Player Performance Analysis
    tabPanel("Player Performance Analysis", 
             sidebarLayout(
               sidebarPanel(
                 selectInput("player_name", "Enter Player's Name", choices = player_names),
                 selectInput(
                   "season_name",
                   "Season",
                   choices = c(
                     "2023-2024",
                     "2022-2023",
                     "2021-2022",
                     "2020-2021",
                     "2019-2020",
                     "2018-2019",
                     "2017-2018",
                     "2016-2017",
                     "2015-2016"
                   )
                 ),
                 numericInput(
                   "n_games",
                   "Number of Games to Display",
                   value = 10,
                   min = 1
                 ),
                 selectInput(
                   "stat",
                   "Statistic to Display",
                   choices = list(
                     "Points" = "player_points",
                     "Rebounds" = "player_rebounds_total",
                     "Assists" = "player_assists"
                   ),
                   selected = "player_points"
                 ),
                 numericInput("line", "Set Prop Line", value = 20, min = 0),
                 h3("Player Stats Plot"),
                 plotOutput("player_stats_plot", height = "400px")
               ),
               mainPanel(
                 h3("Player Stats Table"),
                 dataTableOutput("player_stats_table")
               )
             )
    ),
    # Second tab for Additional Data
    tabPanel("Markets",
             sidebarLayout(sidebarPanel(
               selectInput(
                 "additional_data",
                 "Choose Data",
                 choices = c(
                   "H2H",
                   "Player Points",
                   "Player Rebounds",
                   "Player Assists",
                   "Point Arbs",
                   "Assist Arbs",
                   "Rebound Arbs"
                 )
               ),
               selectizeInput("agencies",
                              "Choose Agencies",
                              choices = unique(player_points_data$agency),
                              multiple = TRUE,
                              selected = unique(player_points_data$agency))
             ),
             mainPanel(
               h3("Additional Data Table"),
               dataTableOutput("additional_data_table")
             )))
  )
)

##%######################################################%##
#                                                          #
####                   Server logic                     ####
#                                                          #
##%######################################################%##

server <- function(input, output) {
    
    output$player_stats_table <- renderDataTable({
        display_player_stats(input$player_name, input$season_name, input$n_games)
    })
    
    output$player_stats_plot <- renderPlot({
        data <- display_player_stats(input$player_name, input$season_name, input$n_games)
        display_empirical_probabilities(data, input$stat, input$line)
    })
    
    output$additional_data_table <- renderDataTable({
      switch(input$additional_data,
             "H2H" = h2h_data |> filter(home_agency %in% input$agencies & away_agency %in% input$agencies),
             "Player Points" = player_points_data |> filter(agency %in% input$agencies),
             "Player Rebounds" = player_rebounds_data |> filter(agency %in% input$agencies),
             "Player Assists" = player_assists_data |> filter(agency %in% input$agencies),
             "Point Arbs" = point_arbs_data |> filter(over_agency %in% input$agencies & under_agency %in% input$agencies),
             "Assist Arbs" = assist_arbs_data |> filter(over_agency %in% input$agencies & under_agency %in% input$agencies),
             "Rebound Arbs" = rebound_arbs_data |> filter(over_agency %in% input$agencies & under_agency %in% input$agencies))
    })
}

##%######################################################%##
#                                                          #
####                       Run                          ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)