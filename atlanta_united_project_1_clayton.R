### Atlanta United Data Scientist Project 1 - Predicting Player Outcomes

## AUTHORED BY
# - Author: Nathan Clayton
# - Email: nathanaclayton25@gmail.com
# - Date Created: 2025-03-18
# - Last Modified: 2025-03-21

## Load Required Packages
install_and_load <- function(packages) {
  install.packages(setdiff(packages, installed.packages()[,"Package"]), dependencies = TRUE)
  lapply(packages, library, character.only = TRUE)
}

packages <- c("tidyverse", "slider", "lubridate", "caret", "zoo", "dplyr", "randomForest", "ggthemes", "teamcolors")
install_and_load(packages)

## Data Ingestion
load_data <- function(file_path) {
  read.csv(file_path)
}

match_log <- load_data("C:/Users/naclayt/OneDrive - Emory/Personal/matchlog_data.csv")
schedule <- load_data("C:/Users/naclayt/OneDrive - Emory/Personal/schedule.csv")

## Data Cleaning
clean_match_log <- function(match_log, schedule) {
  match_log <- match_log %>%
    group_by(season_id) %>%
    mutate(season_name = first(na.omit(season_name))) %>% # Here I replaced all NA values in the season_name column by searching the season_id column for a match and retrieving the corresponding season_name
    ungroup() %>%
    left_join(schedule %>% select(match_id, match_date), by = "match_id") %>%
    mutate(match_date = as.Date(match_date)) # By joining on the match_id variable, I can add match_date to my match_log dataset which will make it easier to calculate rolling averages at both the player and club levels

  
  schedule <- schedule %>% mutate(match_date = as.Date(match_date, format = "%Y-%m-%d"))
  
  list(match_log = match_log, schedule = schedule)
}

cleaned_data <- clean_match_log(match_log, schedule)
match_log <- cleaned_data$match_log
schedule <- cleaned_data$schedule

## Compute Rolling Averages
compute_rolling_averages <- function(match_log) {
  # Filter for the 2025 season first
  player_data <- match_log %>%
    filter(season_name == 2025) %>%  # Ensure only active 2025 players are considered
    group_by(player_id) %>%
    mutate(
      avg_np_xg_5 = rollapply(player_match_np_xg, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
      avg_xa_5 = rollapply(player_match_xa, width = 5, FUN = mean, fill = NA, align = "right", partial = TRUE),
      avg_np_xg_xa_5 = avg_np_xg_5 + avg_xa_5
    ) %>%
    ungroup() %>%
    filter(!is.na(match_date)) %>%
    select(player_name, player_id, team_name, team_id, match_date, avg_xa_5, avg_np_xg_5, avg_np_xg_xa_5)
  
  # Compute expected player volume
  player_expected_volume <- match_log %>%
    filter(season_name == 2025) %>%  # Ensure volume metrics only consider 2025
    group_by(player_name, player_id) %>%
    summarize(
      avg_minutes = mean(player_match_minutes, na.rm = TRUE),
      avg_shots = mean(player_match_np_shots, na.rm = TRUE),
      avg_key_passes = mean(player_match_key_passes, na.rm = TRUE),
      .groups = "drop"
    )
  
  # Join and ensure one row per player 
  player_data <- left_join(player_data, player_expected_volume, by = "player_id") %>%
    arrange(desc(match_date)) %>%  # Sort so the most recent row is first
    distinct(player_id, .keep_all = TRUE)  # Keep only the most recent row per player
  
  return(player_data)
}

player_data <- compute_rolling_averages(match_log)

## Compute Team Defensive Stats
compute_defensive_stats <- function(match_log, schedule) {
  team_stats <- match_log %>%
    filter(season_name == 2025) %>%
    left_join(schedule, by = "match_id") %>%
    mutate(opponent_team_id = ifelse(team_id == home_team_id, away_team_id, home_team_id)) %>%
    group_by(match_id, opponent_team_id) %>%
    summarise(
      match_np_xg_xa_allowed = sum(player_match_np_xg + player_match_xa, na.rm = TRUE),
      total_goals_allowed = sum(player_match_goals, na.rm = TRUE),
      total_assists_allowed = sum(player_match_assists, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    group_by(opponent_team_id) %>%
    summarise(
      avg_np_xg_xa_allowed_2025 = mean(match_np_xg_xa_allowed, na.rm = TRUE),
      total_goals_allowed_2025 = sum(total_goals_allowed, na.rm = TRUE),
      total_assists_allowed_2025 = sum(total_assists_allowed, na.rm = TRUE),
      matches_played = n(),
      avg_np_g_a_allowed_2025 = (sum(total_goals_allowed, na.rm = TRUE) + sum(total_assists_allowed, na.rm = TRUE)) / n(),
      .groups = "drop"
    ) %>% rename(team_id = opponent_team_id)
  
  team_stats %>% left_join(match_log %>% select(team_id, team_name) %>% distinct(), by = "team_id")
}

defensive_performance <- compute_defensive_stats(match_log, schedule)

defensive_performance <- defensive_performance %>%
  mutate(def_over_under_perf = avg_np_xg_xa_allowed_2025 - avg_np_g_a_allowed_2025)

## Filter Schedule for Specific Dates
filter_schedule <- function(schedule, dates) {
  schedule %>% filter(match_date %in% dates)
}

filter_dates <- as.Date(c("2025-03-22", "2025-03-23"))
filtered_schedule <- filter_schedule(schedule, filter_dates)

## Merge Player Data with Schedule
merge_player_schedule <- function(player_data, schedule, defensive_performance) {
  player_data <- player_data %>%
    left_join(schedule %>% select(home_team_id, away_team_id, match_id), by = c("team_id" = "home_team_id")) %>%
    mutate(weekend_opponent = ifelse(!is.na(away_team_id), away_team_id, NA)) %>%
    left_join(schedule %>% select(home_team_id, away_team_id, match_id), by = c("team_id" = "away_team_id")) %>%
    mutate(weekend_opponent = ifelse(is.na(weekend_opponent) & !is.na(home_team_id), home_team_id, weekend_opponent)) %>%
    filter(!is.na(weekend_opponent)) %>%
    left_join(defensive_performance %>% select(team_id, avg_np_xg_xa_allowed_2025), 
              by = c("weekend_opponent" = "team_id")) %>%
    rename(avg_xg_xa_conceded = avg_np_xg_xa_allowed_2025)
  
  player_data
}

player_data <- merge_player_schedule(player_data, filtered_schedule, defensive_performance)

## Train Random Forest Model
train_random_forest <- function(player_data) {
  features <- c("avg_np_xg_5", "avg_xa_5", "avg_shots", "avg_key_passes", "avg_xg_xa_conceded")
  player_data_model <- player_data %>% select(all_of(features), np_xG_xA = avg_np_xg_xa_5) %>% na.omit()
  
  set.seed(42)
  rf_model <- randomForest(np_xG_xA ~ ., data = player_data_model, ntree = 200, mtry = 3, importance = TRUE)
  
  player_data$predicted_np_xg_xa <- round(predict(rf_model, player_data), 4)
  
  player_data %>% 
    select(player_name.x, team_name, weekend_opponent, avg_xg_xa_conceded, predicted_np_xg_xa) %>%
    arrange(desc(predicted_np_xg_xa)) %>%
    head(10)  # Get the top 10 predictions
}

predictions <- train_random_forest(player_data)

## Data Visualization

mls_colors <- teamcolors %>%
  filter(league == "mls") %>%
  select(name, primary, secondary)  # Select team name & colors

print(mls_colors)

team_color_palette <- setNames(mls_colors$primary, mls_colors$name)

predictions <- predictions %>%
  arrange(desc(predicted_np_xg_xa)) %>%
  mutate(rank = row_number())

predictions <- predictions %>%
  mutate(team_name = str_trim(team_name)) %>%  # Remove spaces
  mutate(team_name = case_when(
    team_name == "Chicago Fire" ~ "Chicago Fire",
    team_name == "Vancouver Whitecaps" ~ "Vancouver Whitecaps FC",
    team_name == "XXX" ~ "Philadelphia Union",
    team_name == "San Diego FC" ~ "San Diego FC",
    team_name == "Minnesota United" ~ "Minnesota United FC",
    team_name == "Seattle Soudners FC" ~ "Seattle Sounders FC",
    TRUE ~ team_name  # Keep original if no match
  ))

ggplot(predictions, aes(x = reorder(player_name.x, predicted_np_xg_xa), 
                        y = predicted_np_xg_xa, fill = team_name)) +
  geom_col(width = 0.6) +
  
  # Add team name outside the bar
  geom_text(aes(label = team_name), 
            hjust = -0.2,  # Slightly outside the bar
            color = "black", 
            fontface = "bold", 
            size = 3.5) +
  
  # Add predicted value inside the bar
  geom_text(aes(label = round(predicted_np_xg_xa, 2)), 
            position = position_stack(vjust = 0.5),  # Centered inside the bar
            color = "white", fontface = "bold", size = 3.5) +
  
  scale_fill_manual(values = team_color_palette, guide = "none") +  
  
  labs(
    title = "Top Predicted Non-Penalty xG + xA for Upcoming Fixture",
    x = "Player",
    y = "Predicted np_xG + xA"
  ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.5))) +
  coord_flip() +  # Horizontal bars
  theme_minimal() +
  theme(
    axis.text.y = element_text(size = 10),
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  )
