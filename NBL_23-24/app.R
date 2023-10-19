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
library(readxl)
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
source("data_functions.R", local = TRUE)

# Data--------------------------------------------------------------------------
combined_stats_table <- get_historical_data()
supercoach_data <- get_supercoach_data()
season_schedule_2023_2024 <- read_rds("season_schedule_2023_2024.rds")
player_positions_dvp <- read_excel("NBL-Players-List.xlsx")

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
  # Last Season
  last_season <-
  get_emp_prob(combined_stats_table,
               player_points_data$player_name,
               line = line_value,
               player_points) |>
    mutate(line = line_value)
  
  # Last 3
  last_3 <-
    get_emp_prob_n_games(stats_data = combined_stats_table,
                         player_list = player_points_data$player_name,
                         line = line_value,
                         stat = player_points,
                         n_games = 3) |> 
    rename(emp_prob_last_3 = emp_prob_last_n)
  
  # Last 5
  last_5 <-
    get_emp_prob_n_games(stats_data = combined_stats_table,
                         player_list = player_points_data$player_name,
                         line = line_value,
                         stat = player_points,
                         n_games = 5) |> 
    rename(emp_prob_last_5 = emp_prob_last_n)
  
  list(last_season, last_3, last_5) |> reduce(left_join)
}

player_points_data <-
  player_points_data |>
  mutate(implied_prob_over = round(1/over_price, 3)) |>
  left_join(map(unique(player_points_data$line), get_player_points_emp_prob) |> bind_rows()) |>
  mutate(diff_last_season = round(emp_prob_over - implied_prob_over, 3)) |>
  mutate(diff_last_3 = round(emp_prob_last_3 - implied_prob_over, 3)) |>
  mutate(diff_last_5 = round(emp_prob_last_5 - implied_prob_over, 3)) |>
  select(-games_played) |> 
  ungroup()


# Assists
get_player_assists_emp_prob <- function(line_value) {
  # Last Season
  last_season <-
    get_emp_prob(combined_stats_table,
                 player_assists_data$player_name,
                 line = line_value,
                 player_assists) |>
    mutate(line = line_value)
  
  # Last 3
  last_3 <-
    get_emp_prob_n_games(stats_data = combined_stats_table,
                         player_list = player_assists_data$player_name,
                         line = line_value,
                         stat = player_assists,
                         n_games = 3) |> 
    rename(emp_prob_last_3 = emp_prob_last_n)
  
  # Last 5
  last_5 <-
    get_emp_prob_n_games(stats_data = combined_stats_table,
                         player_list = player_assists_data$player_name,
                         line = line_value,
                         stat = player_assists,
                         n_games = 5) |> 
    rename(emp_prob_last_5 = emp_prob_last_n)
  
  list(last_season, last_3, last_5) |> reduce(left_join)
}

player_assists_data <-
  player_assists_data |>
  mutate(implied_prob_over = round(1/over_price, 3)) |>
  left_join(map(unique(player_assists_data$line), get_player_assists_emp_prob) |> bind_rows()) |>
  mutate(diff_last_season = round(emp_prob_over - implied_prob_over, 3)) |>
  mutate(diff_last_3 = round(emp_prob_last_3 - implied_prob_over, 3)) |>
  mutate(diff_last_5 = round(emp_prob_last_5 - implied_prob_over, 3)) |>
  select(-games_played) |> 
  ungroup()

# Rebounds
get_player_rebounds_emp_prob <- function(line_value) {
  # Last Season
  last_season <-
    get_emp_prob(combined_stats_table,
                 player_rebounds_data$player_name,
                 line = line_value,
                 player_rebounds_total) |>
    mutate(line = line_value)
  
  # Last 3
  last_3 <-
    get_emp_prob_n_games(stats_data = combined_stats_table,
                         player_list = player_rebounds_data$player_name,
                         line = line_value,
                         stat = player_rebounds_total,
                         n_games = 3) |> 
    rename(emp_prob_last_3 = emp_prob_last_n)
  
  # Last 5
  last_5 <-
    get_emp_prob_n_games(stats_data = combined_stats_table,
                         player_list = player_rebounds_data$player_name,
                         line = line_value,
                         stat = player_rebounds_total,
                         n_games = 5) |> 
    rename(emp_prob_last_5 = emp_prob_last_n)
  
  list(last_season, last_3, last_5) |> reduce(left_join)
}

player_rebounds_data <-
  player_rebounds_data |>
  mutate(implied_prob_over = round(1/over_price, 3)) |>
  left_join(map(unique(player_rebounds_data$line), get_player_rebounds_emp_prob) |> bind_rows()) |>
  mutate(diff_last_season = round(emp_prob_over - implied_prob_over, 3)) |>
  mutate(diff_last_3 = round(emp_prob_last_3 - implied_prob_over, 3)) |>
  mutate(diff_last_5 = round(emp_prob_last_5 - implied_prob_over, 3)) |>
  select(-games_played) |> 
  ungroup()

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
    summarise(total_points = sum(player_points, na.rm = TRUE), .by = player_full_name, played_2022_23 = sum(season %in% c("2023-2024", "2023-2022"))) |> 
    arrange(desc(played_2022_23), desc(total_points)) |> 
    filter(player_full_name != "NA NA") |>
    distinct(player_full_name) |> 
    pull(player_full_name)

# Player Teams By Season--------------------------------------------------------
player_teams_by_season <-
  combined_stats_table |>
  mutate(player_full_name = paste(first_name, family_name)) |>
  group_by(player_full_name, season) |>
  summarise(team = first(name)) |>
  filter(!is.na(team) & !is.na(season))

# Current player names with props-----------------------------------------------
prop_player_names <-
  bind_rows(
    player_points_data |> distinct(player_name),
    player_assists_data |> distinct(player_name),
    player_rebounds_data |> distinct(player_name)
  ) |> 
  distinct(player_name) |>
  pull(player_name)

# Function to get all team mates for a given player name and season
get_player_team_mates <-
  function(player_name, season_name) {
    player_teams_by_season |>
        filter(season == season_name) |>
        group_by(team) |> 
        filter(any(player_full_name == player_name)) |>
        pull(player_full_name)
}

# Create function to filter and display player data
display_player_stats <- function(player_name, season_name, n_games, home_or_away = c("home", "away")) {
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
               pace,
               starter,
               player_points,
               player_three_pointers_made,
               player_rebounds_total,
               player_assists,
               player_steals,
               player_blocks,
               player_minutes
               ) |> 
        slice_head(n = n_games) |> 
    filter(home_away %in% home_or_away)
}

# Function to plot hit rate for prop lines
display_empirical_probabilities <-
    function(data, stat, line) {
        # Create Label variable
        if (stat == "player_points") {
            label = "Points"
        }
        else if (stat == "player_three_pointers_made") {
            label = "Three Pointers"
        }
        else if (stat == "player_rebounds_total") {
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
        relocate(teammate, with_teammate, .after =  player_full_name) |> 
      mutate(ppg = round(ppg, 2), apg = round(apg, 2), rpg = round(rpg, 2))
}

# Performance vs team-----------------------------------------------------------

# Get relevant stats and join with positions
stats_2023_2024 <-
  combined_stats_table |>
  filter(season == "2023-2024") |>
  transmute(
    player_name = paste(first_name, family_name),
    player_team = name,
    opposition_team = opp_name,
    round_number,
    starter,
    match_time_utc,
    home_away,
    starter,
    player_minutes = ms(player_minutes),
    player_points,
    player_three_pointers_made,
    player_rebounds_total,
    player_assists,
    player_steals,
    player_blocks
  ) |> 
  left_join(player_positions_dvp) |> 
  filter(!is.na(Rating)) |>
  rename(position = position_1)

# Get only 1 position column
stats_2023_2024 <-
  stats_2023_2024 |>
  bind_rows(
    stats_2023_2024 |>
      select(-position) |>
      filter(!is.na(position_2)) |>
      rename(position = position_2)
  ) |> 
  select(-position_2)

player_position_performance <-
  stats_2023_2024 |> 
  group_by(opposition_team, position) |> 
  summarise(sample_size = n(),
            avg_points = round(mean(player_points), 2),
            avg_assists = round(mean(player_assists), 2),
            avg_rebounds = round(mean(player_rebounds_total), 2)) |>
  arrange(position, desc(avg_points)) |> 
  ungroup()

# Function to get last n games performance--------------------------------------
get_last_n_stats <- function(n_games = 5, player_full_name) {
  return_df <-
    combined_stats_table |>
    mutate(player_name = paste(first_name, family_name)) |>
    filter(player_name == player_full_name) |>
    arrange(desc(match_time_utc)) |>
    slice_head(n = n_games) |>
    select(
      season,
      round_number,
      match_time_utc,
      opp_name,
      full_score,
      opp_score,
      home_away,
      starter,
      matches("^player")
    ) |>
    t() |>
    as.data.frame() |> 
    select(-player_name)
  
  names(return_df) <- str_remove_all(names(return_df), "V")
  
  return_df$var <- row.names(return_df)
  
  return_df |>
    relocate(var, .before = `1`) |> 
    as_tibble()
    
}

# Team Trends Analysis----------------------------------------------------------

# Minutes Played
calculate_player_minutes <- function(team) {
combined_stats_table |> 
  filter(season == "2023-2024") |>
  filter(name == team) |> 
  select(first_name, family_name, round_number, match_time_utc, starter, player_minutes) |> 
  arrange(match_time_utc) |> 
  group_by(match_time_utc) |>
  mutate(match_number = cur_group_id()) |> 
  relocate(match_number, .after = round_number) |>
  group_by(first_name, family_name) |>
  mutate(player_minutes = ms(player_minutes)) |> 
  mutate(player_minutes = period_to_seconds(player_minutes)/60) |>
  mutate(avg_mins = mean(player_minutes, na.rm = TRUE)) |>
  mutate(avg_mins = round(avg_mins, digits = 2)) |>
  mutate(player_minutes = round(player_minutes, digits = 2)) |>
  ungroup() |>
  mutate(player_name = paste(first_name, family_name)) |> 
  select(player_name, avg_mins, match_number, player_minutes) |> 
  pivot_wider(names_from = match_number, values_from = player_minutes) |> 
  select(player_name, any_of(as.character(1:28)), AVG = avg_mins) |> 
  arrange(desc(AVG))}

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
                   value = 35,
                   min = 1
                 ),
                 selectInput(
                   "home_games_only",
                   "Show Which Games?",
                   choices = list("Home" = "home", "Away" = "away"),
                   multiple = TRUE,
                   selected = c("home", "away"),
                   selectize = TRUE
                 ),
                 selectInput(
                   "stat",
                   "Statistic to Display",
                   choices = list(
                     "Points" = "player_points",
                     "3 Pointers Made" = "player_three_pointers_made",
                     "Rebounds" = "player_rebounds_total",
                     "Assists" = "player_assists"
                   ),
                   selected = "player_points"
                 ),
                 numericInput("line", "Set Prop Line", value = 10, min = 0),
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
               selectInput("player_names_props", "Enter Player's Name", choices = prop_player_names, multiple = TRUE),
               selectizeInput("agencies",
                              "Choose Agencies",
                              choices = unique(player_points_data$agency),
                              multiple = TRUE,
                              selected = unique(player_points_data$agency)),
               checkboxInput("unders_shown", "Show Only Markets with Both Unders and Overs", value = FALSE),
               checkboxInput("pos_diff", "Show Only Markets with Positive Difference", value = FALSE),
               checkboxInput("neg_diff", "Show Only Markets with Negative Difference", value = FALSE),
             ),
             mainPanel(
               h3("Additional Data Table"),
               dataTableOutput("additional_data_table")
             ))),
    # Third tab for Player Performance with and without a team-mate
    tabPanel("Player Performance with and without a team-mate",
             sidebarLayout(sidebarPanel(
               selectInput("player_name_2", "Enter Player's Name", choices = player_names),
               selectInput("team_mate", "Enter Team-mate's Name", choices = player_names),
               selectInput(
                 "season_name_2",
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
               )
             ),
             mainPanel(
               h3(""),
               dataTableOutput("player_stats_table_2")
             ))),
    
    # Fourth tab for Player Performance vs Team
    tabPanel("Player Performance vs Team",
             sidebarLayout(sidebarPanel(
               selectInput("position", "Enter Position", choices = player_position_performance$position, selectize = TRUE, multiple = TRUE, selected = player_position_performance$position),
               selectInput("opposition_team", "Enter Opposition Team", choices = player_position_performance$opposition_team, selectize = TRUE, multiple = TRUE, selected = player_position_performance$opposition_team)
             ),
             mainPanel(
               h3(""),
               dataTableOutput("player_stats_table_3")
             ))),
    
    # Fifth tab for last n games stats
    tabPanel("Last n Games Stats",
             sidebarLayout(sidebarPanel(
               selectInput("player_name_3", "Enter Player's Name", choices = player_names),
               numericInput(
                 "n_games_2",
                 "Number of Games to Display",
                 value = 5,
                 min = 1
               )
             ),
             mainPanel(
               h3(""),
               dataTableOutput("player_stats_table_4")
             ))),
    
    # Sixth tab for Team - Player Metrics
    tabPanel("Team - Player Metrics",
             sidebarLayout(sidebarPanel(
               selectInput("team_name", "Enter Team's Name", choices = unique(combined_stats_table$name)),
             ),
             mainPanel(
               h3(""),
               dataTableOutput("player_stats_table_5")
             ))),
  )
)

##%######################################################%##
#                                                          #
####                   Server logic                     ####
#                                                          #
##%######################################################%##

server <- function(input, output) {
    
    output$player_stats_table <- renderDataTable({
        display_player_stats(input$player_name, input$season_name, input$n_games, input$home_games_only)
    })
    
    output$player_stats_plot <- renderPlot({
        data <- display_player_stats(input$player_name, input$season_name, input$n_games, input$home_games_only)
        display_empirical_probabilities(data, input$stat, input$line)
    })
    
    output$player_stats_table_2 <- renderDataTable({
      with_without(player_name = input$player_name_2, team_mate = input$team_mate, season_name = input$season_name_2)
    })
    
    observe(updateSelectInput(session = getDefaultReactiveDomain(), "team_mate", choices = get_player_team_mates(input$player_name_2, input$season_name_2)))
    
    output$player_stats_table_3 <- renderDataTable({
      player_position_performance |> filter(position %in% input$position & opposition_team %in% input$opposition_team)
    })
    
    output$player_stats_table_4 <- renderDataTable({
      get_last_n_stats(player_full_name = input$player_name_3, n_games = input$n_games_2)
    })
    
    output$player_stats_table_5 <- renderDataTable({
      calculate_player_minutes(team = input$team_name)
    })
    
    output$additional_data_table <- renderDataTable({
      data_to_display <- switch(input$additional_data,
                                "H2H" = h2h_data,
                                "Player Points" = player_points_data,
                                "Player Rebounds" = player_rebounds_data,
                                "Player Assists" = player_assists_data,
                                "Point Arbs" = player_points_arbs,
                                "Assist Arbs" = player_assists_arbs,
                                "Rebound Arbs" = player_rebounds_arbs)
      
      # Common filter for the first four cases
      if (input$additional_data %in% c("Player Points", "Player Rebounds", "Player Assists")) {
        data_to_display <- data_to_display |>
          filter(agency %in% input$agencies)
        if (input$unders_shown) {
          data_to_display <- data_to_display |> filter(!is.na(under_price))
        }
        if (length(input$player_names_props) > 0) {
          data_to_display <- data_to_display |> filter(player_name %in% input$player_names_props)
        }
        if (input$pos_diff) {
          data_to_display <- data_to_display |> filter(diff_last_season > 0 & diff_last_3 > 0 & diff_last_5 > 0)
        }
        if (input$neg_diff) {
          data_to_display <- data_to_display |> filter(diff_last_season < 0 & diff_last_3 < 0 & diff_last_5 < 0)
        }
      }
      
      # Special filter for H2H
      if (input$additional_data == "H2H") {
        data_to_display <- data_to_display |> filter(home_agency %in% input$agencies & away_agency %in% input$agencies)
      }
      
      # Special filters for Arbs
      if (input$additional_data %in% c("Point Arbs", "Assist Arbs", "Rebound Arbs")) {
        data_to_display <- data_to_display |> filter(over_agency %in% input$agencies & under_agency %in% input$agencies)
      }
      
      data_to_display
    })
}

##%######################################################%##
#                                                          #
####                       Run                          ####
#                                                          #
##%######################################################%##

shinyApp(ui = ui, server = server)
