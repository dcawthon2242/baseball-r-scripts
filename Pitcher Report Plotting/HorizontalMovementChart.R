horizontal_movement_chart <- function(player_name, dataset) {
  
  pitch_equation <- function(a, v, p0, t) {
    a * t^2 + v * t + p0
  }
  
  # Filter and clean data
  filtered_data <- dataset %>%
    filter(Pitcher == player_name) %>%
    filter(is.finite(ZoneTime)) %>%
    drop_na(ax0, vx0, x0, az0, vz0, z0, TaggedPitchType)
  
  if (nrow(filtered_data) == 0) {
    stop("No valid data for this player.")
  }
  
  # Add reaction time and coordinates
  filtered_data <- filtered_data %>%
    mutate(
      ReactionTime = ZoneTime - 0.15,
      x_react = pitch_equation(ax0, vx0, x0, ReactionTime),
      z_react = pitch_equation(az0, vz0, z0, ReactionTime)
    )
  
  pitch_types <- unique(filtered_data$TaggedPitchType)
  colors <- rainbow(length(pitch_types))
  
  # Base plot
  plot(NA, xlim = c(-3, 3), ylim = c(0, 6),
       xlab = "Horizontal Break (ft)", ylab = "Vertical Break (ft)",
       main = paste("X-Z Plane Pitch Trajectories:", player_name))
  
  # Transparent circle with visible border
  draw_circle <- function(center_x, center_z, r, border_col = "black", lwd = 1) {
    theta <- seq(0, 2*pi, length.out = 100)
    x <- center_x + r * cos(theta)
    z <- center_z + r * sin(theta)
    polygon(x, z, col = NA, border = border_col, lwd = lwd)
  }
  
  for (i in seq_along(pitch_types)) {
    type <- pitch_types[i]
    pitch_data <- filtered_data %>% filter(TaggedPitchType == type)
    
    if (nrow(pitch_data) < 3) next
    
    # Averages for trajectory
    mean_ax0 <- mean(pitch_data$ax0)
    mean_vx0 <- mean(pitch_data$vx0)
    mean_x0  <- mean(pitch_data$x0)
    mean_az0 <- mean(pitch_data$az0)
    mean_vz0 <- mean(pitch_data$vz0)
    mean_z0  <- mean(pitch_data$z0)
    mean_time <- mean(pitch_data$ZoneTime) + 0.1
    
    # Trajectory path
    time_seq <- seq(0, mean_time, length.out = 100)
    x_traj <- pitch_equation(mean_ax0, mean_vx0, mean_x0, time_seq)
    z_traj <- pitch_equation(mean_az0, mean_vz0, mean_z0, time_seq)
    lines(x_traj, z_traj, col = colors[i], lwd = 2)
  }
  
  legend("topright", legend = pitch_types, col = colors, lty = 1, lwd = 2, pch = 19, cex = 0.8)
}

horizontal_movement_chart("Gurnea, Chad", D1TM25)
