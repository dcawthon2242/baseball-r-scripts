library(dplyr)
library(jsonlite)
library(Metrics) # For R-squared

# ... (Previous code for data loading and preprocessing remains the same)

# Function to calculate weighted trajectory distance and R-squared
calculate_r_squared <- function(data, x_weight, y_weight, z_weight) {
  
  data_grouped <- data %>% group_by(game_pk, at_bat_number)
  
  data_grouped$weighted_pitch_trajectory_distance <- NA
  
  for (i in 1:nrow(data_grouped)) {
    at_bat_data <- data_grouped[i, ]
    pitch_data <- at_bat_data$trajectory
    
    pitch_df <- data.frame(t = seq(0, at_bat_data$time_of_flight, length.out = 50),
                           x = pitch_data[[1]]$x,
                           y = pitch_data[[1]]$y,
                           z = pitch_data[[1]]$z)
    
    if (i > 1) {
      prev_pitch_data <- data_grouped[i - 1, ]$trajectory
      prev_pitch_df <- data.frame(t = seq(0, data_grouped[i - 1, ]$time_of_flight, length.out = 50),
                                  x = prev_pitch_data[[1]]$x,
                                  y = prev_pitch_data[[1]]$y,
                                  z = prev_pitch_data[[1]]$z)
      
      t1 <- seq(0, max(c(pitch_df$t, prev_pitch_df$t)), length.out = 200)
      x1 <- rep(0, 200)
      x2 <- rep(0, 200)
      y1 <- rep(0, 200)
      y2 <- rep(0, 200)
      z1 <- rep(0, 200)
      z2 <- rep(0, 200)
      h1 <- 0.01
      h2 <- 0.01
      
      for (j in 1:200) {
        x1[j] <- sum(exp(-0.5 * ((t1[j] - pitch_df$t) / h1)^2) * pitch_df$x) /
          sum(exp(-0.5 * ((t1[j] - pitch_df$t) / h1)^2))
        y1[j] <- sum(exp(-0.5 * ((t1[j] - pitch_df$t) / h1)^2) * pitch_df$y) /
          sum(exp(-0.5 * ((t1[j] - pitch_df$t) / h1)^2))
        z1[j] <- sum(exp(-0.5 * ((t1[j] - pitch_df$t) / h1)^2) * pitch_df$z) /
          sum(exp(-0.5 * ((t1[j] - pitch_df$t) / h1)^2))
        x2[j] <- sum(exp(-0.5 * ((t1[j] - prev_pitch_df$t) / h2)^2) * prev_pitch_df$x) /
          sum(exp(-0.5 * ((t1[j] - prev_pitch_df$t) / h2)^2))
        y2[j] <- sum(exp(-0.5 * ((t1[j] - prev_pitch_df$t) / h2)^2) * prev_pitch_df$y) /
          sum(exp(-0.5 * ((t1[j] - prev_pitch_df$t) / h2)^2))
        z2[j] <- sum(exp(-0.5 * ((t1[j] - prev_pitch_df$t) / h2)^2) * prev_pitch_df$z) /
          sum(exp(-0.5 * ((t1[j] - prev_pitch_df$t) / h2)^2))
      }
      
      distance_df <- data.frame(t1, d1 = rep(0, 200), x1, y1, z1, x2, y2, z2)
      distance_df$d1 <- sqrt((x_weight * (distance_df$x1 - distance_df$x2))^2 + (y_weight * (distance_df$y1 - distance_df$y2))^2 + (z_weight * (distance_df$z1 - distance_df$z2))^2) # Weighted Distance
      delta <- distance_df$t1[2] - distance_df$t1[1]
      distance <- sum(delta * distance_df$d1)
      
      data_grouped$weighted_pitch_trajectory_distance[i] <- distance
    }
  }
  
  data_grouped$weighted_pitch_trajectory_distance[data_grouped$pitch_number == 1] <- NA
  data_grouped <- data_grouped %>%
    group_by(game_pk, at_bat_number) %>%
    mutate(
      skip_occurred = pitch_number - lag(pitch_number, default = 0) > 1,
      weighted_pitch_trajectory_distance = ifelse(skip_occurred, NA, weighted_pitch_trajectory_distance)
    ) %>%
    ungroup()
  data_grouped <- data_grouped %>% select(-skip_occurred)
  
  # Calculate R-squared
  r_squared <- cor(data_grouped$weighted_pitch_trajectory_distance, data_grouped$swing_decision_rv, use = "complete.obs")^2
  return(r_squared)
}

# Optimization using optim (more robust than brute force)
optimize_weights <- function(data) {
  optimization_result <- optim(par = c(1, 1, 1), # Initial weights
                               fn = function(weights) {
                                 -calculate_r_squared(data, weights[1], weights[2], weights[3]) # Negative because optim minimizes
                               },
                               lower = c(0, 0, 0), # Weights must be non-negative
                               upper = c(10,10,10),
                               method = "L-BFGS-B") # Use bounded optimization
  
  best_weights <- optimization_result$par
  best_r_squared <- -optimization_result$value
  return(list(weights = best_weights, r_squared = best_r_squared))
}

# Run the optimization
optimization_results <- optimize_weights(pbp_2022_2024_subset)

# Print the results
print(paste("Optimized X weight:", optimization_results$weights[1]))
print(paste("Optimized Y weight:", optimization_results$weights[2]))
print(paste("Optimized Z weight:", optimization_results$weights[3]))
print(paste("Best R-squared:", optimization_results$r_squared))

# ... (Rest of your code to save the data)