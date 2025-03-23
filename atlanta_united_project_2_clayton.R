# Atlanta United Striker Analysis
# Author: Nathan Clayton
# Date: 03-18-25

# Load Libraries
library(tidyverse)
library(lubridate)
library(caret)
library(ggplot2)
library(knitr)
library(reshape2)
library(ggrepel)
library(ggthemes)

# Function to Load Data
load_data <- function(file_path) {
  readRDS(file_path)
}

# Function to Filter Data
filter_data <- function(event_log) {
  event_log %>%
    filter(position.id %in% c(18:25)) %>%
    select(id, match_id, player.id, team.id, position.id, position.name, under_pressure, 
           avevelocity, shot.statsbomb_xg, shot.shot_execution_xg, 
           shot.shot_execution_xg_uplift, shot.outcome.name)
}

# Function to Calculate League Averages
calculate_league_averages <- function(filtered_event_log) {
  list(
    shot_execution = mean(filtered_event_log$shot.shot_execution_xg, na.rm = TRUE),
    shot_quality = mean(filtered_event_log$shot.statsbomb_xg, na.rm = TRUE),
    quality_under_pressure = mean(filtered_event_log$shot.statsbomb_xg[filtered_event_log$under_pressure == TRUE], na.rm = TRUE)
  )
}

# Function to Compute Player Stats
compute_player_stats <- function(filtered_event_log, league_avg) {
  filtered_event_log %>%
    filter(!is.na(shot.statsbomb_xg), !is.na(shot.shot_execution_xg)) %>%
    group_by(player.id) %>%
    summarise(
      avg_shots_per_match = mean(n(), na.rm = TRUE),
      avg_shot_execution = mean(shot.shot_execution_xg, na.rm = TRUE),
      avg_shot_quality = mean(shot.statsbomb_xg, na.rm = TRUE),
      avg_quality_under_pressure = mean(shot.statsbomb_xg[under_pressure == TRUE], na.rm = TRUE)
    ) %>%
    ungroup() %>%
    filter(avg_shots_per_match >= 1.5 &
             avg_shot_execution > league_avg$shot_execution &
             avg_shot_quality > league_avg$shot_quality &
             avg_quality_under_pressure > league_avg$quality_under_pressure)
}

# Function to Rank Players by Effectiveness
rank_players <- function(player_aggregated_stats) {
  player_aggregated_stats %>%
    mutate(effectiveness_score = (avg_shot_execution + avg_shot_quality + avg_quality_under_pressure) / 3) %>%
    arrange(desc(effectiveness_score)) %>%
    slice_max(order_by = effectiveness_score, n = 10)
}

# Function to Generate Shot Execution vs. Quality Plot
plot_shot_execution_vs_quality <- function(player_aggregated_stats_rank) {
  ggplot(player_aggregated_stats_rank, aes(x = avg_shot_quality, y = avg_shot_execution, size = effectiveness_score, label = player.id)) +
    geom_point(aes(color = as.factor(player.id)), alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "red", fill = "pink", alpha = 0.2, linetype = "dashed") + # Trendline with shading
    geom_text_repel(size = 3, box.padding = 0.5, max.overlaps = Inf) +
    labs(title = "Shot Execution vs. Shot Quality",
         x = "Average Shot Quality",
         y = "Average Shot Execution",
         size = "Effectiveness Score") +
    theme_tufte() +
    theme(legend.position = "none") # Remove legend
}

# Function to Generate Heatmap
plot_heatmap <- function(player_aggregated_stats_rank, league_avg) {
  melted_data <- melt(player_aggregated_stats_rank, id.vars = "player.id", measure.vars = c("avg_shot_quality", "avg_shot_execution", "avg_quality_under_pressure"))
  
  league_avg_values <- data.frame(
    variable = c("avg_shot_quality", "avg_shot_execution", "avg_quality_under_pressure"),
    value = c(league_avg$shot_quality, league_avg$shot_execution, league_avg$quality_under_pressure)
  )
  
  ggplot(melted_data, aes(x = variable, y = reorder(as.factor(player.id), value), fill = value)) +
    geom_tile(color = "white") +
    geom_text(aes(label = round(value, 2)), color = "black", size = 3) +
    geom_hline(data = league_avg_values, aes(yintercept = value), linetype = "dashed", color = "red", size = 1) +
    scale_fill_gradient2(low = "red", mid = "white", high = "blue", midpoint = mean(league_avg_values$value)) +
    labs(title = "Shot Metrics Heatmap for Top 10 Players (vs. League Average)",
         x = "Metric",
         y = "Player ID") +
    theme_minimal()
}

# Function to Generate Table Plot of Top 10 Ranked Players
plot_top_10_table <- function(player_aggregated_stats_rank) {
  table_plot <- player_aggregated_stats_rank %>%
    select(player.id, avg_shots_per_match, avg_shot_execution, avg_shot_quality, avg_quality_under_pressure, effectiveness_score) %>%
    arrange(desc(effectiveness_score)) %>%
    mutate(player.id = as.factor(player.id))
  
  ggplot(table_plot, aes(y = reorder(player.id, -effectiveness_score))) +
    geom_tile(aes(x = 1, fill = effectiveness_score), color = "white") +
    geom_text(aes(x = 1, label = round(effectiveness_score, 2)), color = "black", size = 5) +
    scale_fill_gradient(low = "lightblue", high = "darkblue") +
    labs(title = "Top 10 Players by Effectiveness Score", x = NULL, y = "Player ID") +
    theme_minimal() +
    theme(axis.text.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")
}

plot_top_10_effectiveness <- function(player_aggregated_stats_rank) {
  ggplot(player_aggregated_stats_rank, aes(x = reorder(player.id, effectiveness_score), y = effectiveness_score)) +
    geom_col(fill = "black", width = 0.4) +  # Make columns thinner and black
    geom_text(aes(label = sprintf("%.3f", effectiveness_score)), hjust = 1.2, color = "white", size = 4) +  # Adjust labels to avoid cutoff
    labs(title = "Top 10 Players by Effectiveness Score",
         x = "Player ID",
         y = "Effectiveness Score") +
    theme_minimal() +
    coord_flip()
}


# Main Execution
file_path <- "C:/Users/naclayt/OneDrive - Emory/Personal/atlutd_datascientist_project2_eventdata.rds"
event_log <- load_data(file_path)
filtered_event_log <- filter_data(event_log)
league_avg <- calculate_league_averages(filtered_event_log)
player_aggregated_stats <- compute_player_stats(filtered_event_log, league_avg)
player_aggregated_stats_rank <- rank_players(player_aggregated_stats)

# Print Results
print(player_aggregated_stats_rank)
print(player_aggregated_stats_rank %>% slice_max(order_by = effectiveness_score, n = 3) %>% select(player.id, effectiveness_score))

# Generate Plots
plot_shot_execution_vs_quality(player_aggregated_stats_rank)
plot_heatmap(player_aggregated_stats_rank, league_avg)
plot_top_10_effectiveness(player_aggregated_stats_rank)

