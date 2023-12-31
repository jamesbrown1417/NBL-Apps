##%######################################################%##
#                                                          #
####                Get historical Data                 ####
#                                                          #
##%######################################################%##

get_historical_data <- function() {
  
  #==============================================================================
  # Get Data
  #==============================================================================
  
  # Team Box Scores
  match_results <-
    nbl_results(wide_or_long = "long") |>
    mutate(match_time_utc = ymd_hms(match_time_utc)) |>
    arrange(desc(match_time_utc))
  
  # Team Box Scores
  team_box_scores <-
    nblR::nbl_box_team() |>
    relocate(ot_score, .after = p4_score) |>
    select(-c(short_name, opp_short_name, efficiency_custom, tot_eff_1:tot_eff_7, -full_score, -opp_full_score)) |>
    left_join(match_results |>
                select(match_id, round_number, match_time_utc, extra_periods_used) |>
                distinct(match_id, .keep_all = TRUE)) |>
    relocate(round_number, match_time_utc, extra_periods_used, .after = season) |>
    arrange(desc(match_time_utc))
  
  # Player Box Scores
  player_box_scores <-
    nbl_box_player() |>
    select(match_id,
           first_name,
           family_name,
           playing_position,
           starter,
           shirt_number,
           minutes:active,
           points_fast_break:plus_minus_points)
  
  # Player Team Data
  team_data <-
    nblR::nbl_pbp() |> 
    select(match_id, team_name, first_name, family_name) |> 
    distinct(match_id, team_name, first_name, family_name, .keep_all = TRUE)
  
  # Join with Player Box Scores
  player_box_scores <-
    player_box_scores |> 
    left_join(team_data) |> 
    relocate(team_name, .after = family_name) |> 
    rename(name = team_name)
  
  # Combined Stats Table----------------------------------------------
  
  # Rename variables
  team_box_scores <-
    team_box_scores |>
    rename(
      match_minutes = minutes,
      match_points = points,
      match_field_goals_made = field_goals_made,
      match_field_goals_attempted = field_goals_attempted,
      match_field_goals_percentage = field_goals_percentage,
      match_three_pointers_made = three_pointers_made,
      match_three_pointers_attempted = three_pointers_attempted,
      match_three_pointers_percentage = three_pointers_percentage,
      match_two_pointers_made = two_pointers_made,
      match_two_pointers_attempted = two_pointers_attempted,
      match_two_pointers_percentage = two_pointers_percentage,
      match_free_throws_made = free_throws_made,
      match_free_throws_attempted = free_throws_attempted,
      match_free_throws_percentage = free_throws_percentage,
      match_rebounds_defensive = rebounds_defensive,
      match_rebounds_offensive = rebounds_offensive,
      match_rebounds_total = rebounds_total,
      match_assists = assists,
      match_turnovers = turnovers,
      match_steals = steals,
      match_blocks = blocks,
      match_blocks_received = blocks_received,
      match_fouls_personal = fouls_personal,
      match_fouls_received = fouls_on,
      match_points_second_chance = points_second_chance,
      match_points_fast_break = points_fast_break,
      match_points_in_the_paint = points_in_the_paint
    ) |>
    filter(!is.na(p1_score) &
             !is.na(p2_score) &
             !is.na(p3_score) &
             !is.na(p4_score))
  
  player_box_scores <-
    player_box_scores |>
    rename(
      player_minutes = minutes,
      player_points = points,
      player_field_goals_made = field_goals_made,
      player_field_goals_attempted = field_goals_attempted,
      player_field_goals_percentage = field_goals_percentage,
      player_three_pointers_made = three_pointers_made,
      player_three_pointers_attempted = three_pointers_attempted,
      player_three_pointers_percentage = three_pointers_percentage,
      player_two_pointers_made = two_pointers_made,
      player_two_pointers_attempted = two_pointers_attempted,
      player_two_pointers_percentage = two_pointers_percentage,
      player_free_throws_made = free_throws_made,
      player_free_throws_attempted = free_throws_attempted,
      player_free_throws_percentage = free_throws_percentage,
      player_rebounds_defensive = rebounds_defensive,
      player_rebounds_offensive = rebounds_offensive,
      player_rebounds_total = rebounds_total,
      player_assists = assists,
      player_turnovers = turnovers,
      player_steals = steals,
      player_blocks = blocks,
      player_blocks_received = blocks_received,
      player_fouls_personal = fouls_personal,
      player_fouls_received = fouls_on,
      player_points_second_chance = points_second_chance,
      player_points_fast_break = points_fast_break,
      player_points_in_the_paint = points_in_the_paint
    )
  
  # Create pace variable----------------------------------------------------------
  # Get vars
  home_team_match_data <-
    team_box_scores |>
    filter(home_away == "home") |>
    select(match_id,
           match_minutes,
           home_name = name,
           home_fga = match_field_goals_attempted,
           home_fgm = match_field_goals_made,
           home_fta = match_free_throws_attempted,
           home_dreb = match_rebounds_defensive,
           home_oreb = match_rebounds_offensive,
           home_to = match_turnovers)
  
  away_team_match_data <-
    team_box_scores |>
    filter(home_away == "away") |> 
    select(match_id,
           match_minutes,
           away_name = name,
           away_fga = match_field_goals_attempted,
           away_fgm = match_field_goals_made,
           away_fta = match_free_throws_attempted,
           away_dreb = match_rebounds_defensive,
           away_oreb = match_rebounds_offensive,
           away_to = match_turnovers)
  
  # get PACE
  pace <-
    home_team_match_data |> 
    left_join(away_team_match_data, by = c("match_id", "match_minutes")) |> 
    mutate(match_minutes = as.integer(str_extract(match_minutes, "[0-9]+(?=:)"))) |>
    mutate(match_minutes = round(match_minutes / 5) * 5) |>
    filter(match_minutes >= 200) |> 
    mutate(home_possessions = (home_fga + 0.4 * home_fta - 1.07 * (home_oreb / (home_oreb + away_dreb)) * (home_fga - home_fgm) + home_to)) |>
    mutate(away_possessions = (away_fga + 0.4 * away_fta - 1.07 * (away_oreb / (away_oreb + home_dreb)) * (away_fga - away_fgm) + away_to)) |>
    mutate(possessions = (home_possessions + away_possessions) / 2) |>
    mutate(pace = 200*(possessions) / match_minutes) |> 
    transmute(match_id, possessions = round(possessions, 1), pace = round(pace, 1))
  
  # Combine
    team_box_scores |>
    full_join(player_box_scores,
              by = c("match_id", "name"),
              relationship = "many-to-many") |> 
    left_join(pace) |> 
      mutate(first_name = str_replace(first_name, "^Mitch$", "Mitchell")) |> 
      mutate(first_name = str_replace(first_name, "^Jordon$", "Jordan")) |> 
      mutate(first_name = str_replace(first_name, "^Dj$", "DJ")) |>
      mutate(family_name = str_replace(family_name, "^Mccarron$", "McCarron")) |>
      mutate(family_name = str_replace(family_name, "^Le'afa$", "Le'Afa")) |>
      mutate(family_name = str_replace(family_name, "^Mcdaniel$", "McDaniel")) |>
      mutate(family_name = str_replace(family_name, "^Kell Iii$", "Kell")) |>
      mutate(family_name = str_replace(family_name, "^Mcveigh$", "McVeigh")) |> 
      mutate(family_name = str_replace(family_name, "^Mcdowell$", "McDowell")) |> 
      mutate(match_minutes = as.integer(str_extract(match_minutes, "[0-9]+(?=:)"))) |>
      mutate(match_minutes = round(match_minutes / 5) * 5) |> 
      mutate(usage = (100*(0.33*player_assists + player_field_goals_attempted + 0.44*player_free_throws_attempted + player_turnovers)) / possessions) |> 
      mutate(starter = factor(starter, labels = c("Bench", "Starter")))
    }

##%######################################################%##
#                                                          #
####                Get Supercoach Data                 ####
#                                                          #
##%######################################################%##

safe_subscript <- function(x, index) {
  if (index <= length(x) && index > 0) {
    return(x[[index]])
  } else {
    return(NULL)
  }
}

get_supercoach_data <- function() {
  
  # Supercoach API URL
  url = "https://supercoach.dailytelegraph.com.au/2023/api/nbl/classic/v1/players-cf?embed=notes%2Codds%2Cplayer_stats%2Cpositions&round=0&xredir=1"
  
  # Make request
  req <- request(url)
  
  # Get response
  resp <- req_perform(req)
  
  # Process response
  all_data <- resp |> resp_body_json()
  
  # Create a function to extract the data from the json list for each player
  get_supercoach_data <- function(player_data) {
    tibble(
      player_id = player_data$id,
      player_name = paste(player_data$first_name, player_data$last_name),
      player_first_name = player_data$first_name,
      player_last_name = player_data$last_name,
      player_team = player_data$team$name,
      player_team_id = player_data$team_id,
      previous_games = player_data$previous_games,
      previous_average = player_data$previous_average,
      previous_total = player_data$previous_total,
      injury_suspension_status = player_data$injury_suspension_status,
      injury_suspension_status_text = player_data$injury_suspension_status_text,
      locked = player_data$locked,
      active = player_data$active,
      played_status = player_data$played_status$display,
      supercoach_price = player_data$player_stats[[1]]$price,
      points = player_data$player_stats[[1]]$points_scored,
      games = player_data$player_stats[[1]]$games,
      minutes_played = player_data$player_stats[[1]]$minutes_played,
      total_rebouds = player_data$player_stats[[1]]$total_rebounds,
      total_assists = player_data$player_stats[[1]]$total_assists,
      total_steals = player_data$player_stats[[1]]$total_steals,
      total_blocks = player_data$player_stats[[1]]$total_blocks,
      total_turnovers = player_data$player_stats[[1]]$total_turnovers,
      supercoach_position_1 = safe_subscript(player_data$positions, 1)$position,
      supercoach_position_2 = safe_subscript(player_data$positions, 2)$position
    )
  }
  
  # Map to list
    map(all_data, get_supercoach_data) |> 
      bind_rows() |> 
      mutate(player_last_name = str_replace_all(player_last_name, "Kell III", "Kell")) |> 
      mutate(player_name = str_replace_all(player_name, "Kell III", "Kell")) |> 
      mutate(player_first_name = str_replace_all(player_first_name, "^Mitch", "Mitchell")) |> 
      mutate(player_name = str_replace_all(player_name, "^Mitch", "Mitchell")) |> 
      mutate(player_first_name = str_replace_all(player_first_name, "^Jordon", "Jordan")) |> 
      mutate(player_name = str_replace_all(player_name, "^Jordon", "Jordan"))
    
}

##%######################################################%##
#                                                          #
####      Get Empirical Probabilities - Season          ####
#                                                          #
##%######################################################%##

# Function to calculate empirical probabilities
get_emp_prob <- function(stats_data, player_list, stat, line, season_name = "2022-2023") {
  
  # Filter to season
  combined_stats_table |>
    filter(season == season_name) |>
    
    # Filter to players
    mutate(player_name = paste(first_name, family_name)) |>
    filter(player_name %in% player_list) |>

    # Group by player
    group_by(player_name) |>
    
    # Calculate empirical probability
    summarise(games_played = n(),
              emp_prob_over = mean({{ stat }} >= {{ line }})) |>
    filter(games_played >= 10) |> 
    arrange(desc(emp_prob_over)) |> 
    mutate(emp_prob_over = round(emp_prob_over, 3))
}

##%######################################################%##
#                                                          #
####      Get last n game empirical probabilities       ####
#                                                          #
##%######################################################%##


# Function to calculate empirical probabilities
get_emp_prob_n_games <- function(stats_data, player_list, stat, line, n_games = 3) {
  
  # Filter to players
  combined_stats_table |>
    mutate(player_name = paste(first_name, family_name)) |>
    filter(player_name %in% player_list) |>
    
    # Group by player
    group_by(player_name) |>
    
    # Arrange and get last n games
    arrange(player_name, desc((match_time_utc))) |> 
    slice_head(n = n_games) |> 
    
    # Calculate empirical probability
    summarise(games_played = n(),
              emp_prob_last_n = mean({{ stat }} >= {{ line }})) |>
    filter(games_played >= n_games) |> 
    arrange(desc(emp_prob_last_n)) |> 
    mutate(emp_prob_last_n = round(emp_prob_last_n, 3)) |> 
    select(-games_played)
}

##%######################################################%##
#                                                          #
####           Market Manipulation Calculator           ####
#                                                          #
##%######################################################%##

market_manipulation_calc <-
  function(odds_a, stake_a, odds_b, stake_b) {
    # If a wins
    a_wins <- odds_a * stake_a - stake_a - stake_b
    
    # If b wins
    b_wins <- odds_b * stake_b - stake_a - stake_b
    
    # Get equivalent odds for just a bet on b
    equiv_odds <- 1 + (abs(b_wins) / abs(a_wins))
    
    glue::glue("This is the equivalent of betting ${abs(a_wins)} on outcome b at {round(equiv_odds, 2)}")
  }
