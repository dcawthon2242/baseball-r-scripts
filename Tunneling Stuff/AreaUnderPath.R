# Load necessary libraries
install.packages("dplyr")
library(dplyr)
install.packages("tidyverse")
library(tidyverse)
install.packages("devtools")
library(devtools)
install.packages("Rcpp")
library(Rcpp)
devtools::install_github("BillPetti/baseballr", force = TRUE)
library(baseballr)
install.packages("ggplot2")
library(ggplot2)
library(purrr)

# Test for data
test_data <- scrape_statcast_savant(
  start_date = Sys.Date() - 1,
  end_date = Sys.Date(),
  playerid = NULL,
  player_type = "pitcher"
)

# Make a 2024 dataset
days <- seq(from = as.Date('2025-03-05'), to = Sys.Date(), by = 'days')
merged_data <- data.frame()

for (i in seq_along(days)) {
  current_date <- days[i]
  
  # Scrape data for the current date
  daily_data <- scrape_statcast_savant(
    start_date = current_date,
    end_date = current_date,
    playerid = NULL,
    player_type = "pitcher"
  )
  
  # Only bind rows if there is valid data
  if (!is.null(daily_data) && nrow(daily_data) > 0) {
    merged_data <- bind_rows(merged_data, daily_data)
  }
}

pbp_2025_2 <- bind_rows(pbp_2025_2, merged_data)

# Define the parameters for the quadratic equation
pbp_2024 <- pbp_2024 %>%
  mutate(
    a = 0.5 * ay0,  # Coefficient for t^2
    b = vy0,       # Coefficient for t
    c = Extension - 60.5,  # Constant term
    
    # Calculate the discriminant
    discriminant = b^2 - 4 * a * c,
    
    # Calculate time_of_flight using the quadratic formula
    time_of_flight = ifelse(discriminant >= 0,
                            (-b - sqrt(discriminant)) / (2 * a) * -1, 
                            NA)
  )

# Filter the dataset to keep only rows with odd row_index values (ONLY DO THIS FOR BASEBALLR)
pbp_2024 <- pbp_2024 %>%
  mutate(row_index = row_number()) %>%
  filter(row_index %% 2 != 0) %>%
  select(-row_index)

pbp_2024 <- pbp_2024 %>%
  mutate(reaction_zone = time_of_flight - .175) %>%
  arrange(game_pk, at_bat_number, inning, inning_topbot, pitch_number)

# Save the pbp_2024 dataset as a CSV file
write.csv(pbp_2024, file = "~/Desktop/pbp_2024.csv", row.names = FALSE)

# Function to calculate the position for vectorized inputs
calculate_position_vectorized <- function(vx0, vy0, vz0, ax, ay, az, t) {
  x <- vx0 * t + 0.5 * ax * t^2
  y <- vy0 * t + 0.5 * ay * t^2
  z <- vz0 * t + 0.5 * az * t^2
  return(cbind(x, y, z))
}

# Add the trajectory column while handling NA values in `time_of_flight`
pbp_2022_2024 <- pbp_2022_2024 %>%
  mutate(
    trajectory = pmap(
      list(vx0 = vx0, vy0 = vy0, vz0 = vz0, ax = ax, ay = ay, az = az, t = time_of_flight),
      ~ ifelse(is.na(..7), NA, calculate_position_vectorized(..1, ..2, ..3, ..4, ..5, ..6, ..7))
    )
  )

library(dplyr)
library(purrr)
library(ggplot2)
library(Rcpp)

calculate_area_between_trajectories <- function(trajectory1, trajectory2) {
  # Check if either trajectory is NULL or empty
  if (is.null(trajectory1) || nrow(trajectory1) == 0 || is.null(trajectory2) || nrow(trajectory2) == 0) {
    return(NA_real_)
  }
  
  # Define the time sequence for interpolation
  t_seq <- seq(0, min(max(trajectory1[,3], na.rm = TRUE), max(trajectory2[,3], na.rm = TRUE)), length.out = 50)
  
  # Interpolate positions
  trajectory1_interp <- tryCatch({
    approx(trajectory1[,3], trajectory1[,1:2], xout = t_seq, rule = 2)$y
  }, error = function(e) {
    print("Error in trajectory1 interpolation:")
    print(e)
    return(matrix(NA_real_, nrow = length(t_seq), ncol = 2))
  })
  
  trajectory2_interp <- tryCatch({
    approx(trajectory2[,3], trajectory2[,1:2], xout = t_seq, rule = 2)$y
  }, error = function(e) {
    print("Error in trajectory2 interpolation:")
    print(e)
    return(matrix(NA_real_, nrow = length(t_seq), ncol = 2))
  })
  
  # Debug output
  print(head(trajectory1_interp))
  print(head(trajectory2_interp))
  
  # Ensure the interpolated trajectories have the same length
  if (nrow(trajectory1_interp) != nrow(trajectory2_interp)) {
    print("Trajectory lengths differ after interpolation")
    return(NA_real_)
  }
  
  # Calculate Euclidean distances between corresponding points
  distances <- mapply(
    function(x1, y1, x2, y2) {
      sqrt((x2 - x1)^2 + (y2 - y1)^2)
    },
    trajectory1_interp[,1], trajectory1_interp[,2],
    trajectory2_interp[,1], trajectory2_interp[,2]
  )
  
  # Calculate the area using the trapezoidal rule
  area <- sum(distances * diff(t_seq))
  return(area)
}

processed_chunks <- processed_chunks %>%
  arrange(game_pk, at_bat_number, pitch_number) %>%
  group_by(game_pk, at_bat_number) %>%
  mutate(
    # Check positions for lag and current pitch
    area_between_paths = map2_dbl(
      lag(positions), positions,
      function(x, y) {
        print("Debugging map2_dbl call:")
        print(class(x))
        print(class(y))
        if (is.null(x) || is.null(y)) {
          print("One of the positions is NULL")
          return(NA_real_)
        }
        if (nrow(x) == 0 || nrow(y) == 0) {
          print("One of the position matrices is empty")
          return(NA_real_)
        }
        calculate_area_between_trajectories(as.matrix(x), as.matrix(y))
      }
    )
  ) %>%
  ungroup()
