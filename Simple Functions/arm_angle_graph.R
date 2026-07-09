arm_angle_graph <- function(dataset, pitcher, pitcher_column, arm_angle_column) {
  library(dplyr)
  
  # Filter dataset for the specified pitcher
  player_dataset <- filter(dataset, pitcher_column == pitcher)
  print(player_dataset)
  
  # Get the mean arm angle
  mean_arm_angle <- mean(player_dataset$arm_angle_column, na.rm = TRUE)
  
  # Define parameters for the arm angle line
  x0 <- 0
  y0 <- 0
  d <- 24
  angle <- mean_arm_angle 
  end_x <- x0 + d * cos(angle)
  end_y <- y0 + d * sin(angle)
  
  print(angle)
  
  # Return a geom_segment layer with fixed values
  geom_segment(
    x = x0, y = y0, xend = end_x, yend = end_y,
    color = "blue",
    size = 1.2,
    inherit.aes = FALSE
  )
}
