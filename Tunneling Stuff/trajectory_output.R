library(dplyr)
library(tidyr)

# Define the function to handle a dataframe and process each row
calculate_and_append_trajectory <- function(dataset) {
  
  # Loop through each row of the dataset
  dataset_with_trajectory <- dataset %>%
    rowwise() %>%
    mutate(
      trajectory_lists = list(if(is.finite(reaction_zone) && reaction_zone > 0) {
        # Generate time points based on the reaction_zone for the current row
        time_points <- seq(0, reaction_zone, length.out = 50)
        
        # Calculate the trajectory for each time point
        x_trajectory <- release_pos_x + vx0 * time_points + 0.5 * ax * time_points^2
        y_trajectory <- release_extension + vy0 * time_points + 0.5 * ay * time_points^2
        z_trajectory <- release_pos_z + vz0 * time_points + 0.5 * az * time_points^2
        
        # Store as a list with three vectors
        list(x_trajectory, y_trajectory, z_trajectory)
      } else {
        # If reaction_zone is not finite, assign NA vectors
        list(rep(NA_real_, 50), rep(NA_real_, 50), rep(NA_real_, 50))
      })
    ) %>%
    ungroup()
  
  return(dataset_with_trajectory)
}

# Apply the function to your dataset
pbp_2022_2024 <- calculate_and_append_trajectory(pbp_2022_2024)

# Now the pbp_2022_2024 dataframe has a column called trajectory_lists
# which contains a list with three vectors (x, y, z) for each row

# Verify the structure of the first few entries to ensure compatibility
head(pbp_2022_2024$trajectory_lists, 3)

# Convert the list column to character strings
pbp_2024_2_pitches <- pbp_2024_2_pitches %>%
  mutate(trajectory = sapply(trajectory, toString))

# Define the full file path
file_path <- "/Users/a13105/Downloads/pbp_2024_2_pitches_w_trajectories.csv"

# Write the dataset to a CSV file
write.csv(pbp_2024_2_pitches, file = file_path, row.names = FALSE)

# Print a confirmation message
cat("CSV file has been saved to:", file_path, "\n")







# Load necessary library
library(ggplot2)

# Assuming 'trajectory_data' is already loaded in your environment
# Example structure of trajectory_data dataframe
# trajectory_data <- data.frame(
#   X = c(...), 
#   Y = c(...),
#   Time = c(...)
# )

# Load necessary library
library(ggplot2)

# Assuming 'trajectory_data' is already loaded in your environment
# Example structure of trajectory_data dataframe
# trajectory_data <- data.frame(
#   X = c(...), 
#   Y = c(...),
#   Time = c(...)
# )

# Define the row indices for the two lines
first_half <- 1:50
second_half <- 51:100

# Create the plot
plot <- ggplot() +
  geom_line(data = trajectory_data[first_half, ], aes(x = x, y = z), color = 'blue') +
  geom_line(data = trajectory_data[second_half, ], aes(x = x, y = z), color = 'red') +
  labs(
    title = "Trajectory Lines Over Time",
    x = "X",
    y = "Z"
  ) +
  xlim(0, 50) + # Limit x-axis from 0 to 50 units
  theme_minimal()

# Save the plot to a file
ggsave("trajectory_plot.png", plot = plot, width = 10, height = 6)
