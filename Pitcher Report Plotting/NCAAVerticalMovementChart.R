vertical_movement_chart <- function(player_name, dataset) {
  
  pitch_equation <- function(az0, vz0, z0, x) {
    a = az0
    b = vz0
    c = z0
    y = a * x^2 + b * x + c
    return(y)
  }
  
  # Filter data for the specified player and pitch type
  filtered_data <- dataset %>% filter(Pitcher == player_name)
  
  # Remove missing or non-finite values in ZoneTime
  filtered_data <- filtered_data[is.finite(filtered_data$ZoneTime), ]
  
  # Group by TaggedPitchType and calculate means
  summarized_data <- filtered_data %>% 
    group_by(TaggedPitchType) %>%
    summarize(
      az0 = mean(az0, na.rm = TRUE),
      vz0 = mean(vz0, na.rm = TRUE),
      z0 = mean(z0, na.rm = TRUE),
      ZoneTime = mean(ZoneTime, na.rm = TRUE)
    )
  
  common_ZoneTime <- min(summarized_data$ZoneTime)
  
  # Plot the results
  x <- seq(0, common_ZoneTime, length.out = 100)  # increase the number of points for a smoother curve
  border <- common_ZoneTime - 0.03
  plot(x, rep(NA, length(x)), type = "n", col = 'red',
       main = paste("Vertical Pitch Trajectory for", player_name),
       xlab = "Seconds", ylab = "Height", ylim = c(0, 20), xlim = range(0.02, border))
  
  # Loop through pitch types and print debug information
  for (i in 1:nrow(summarized_data)) {
    pitch_type <- summarized_data$TaggedPitchType[i]
    y <- pitch_equation(summarized_data$az0[i], summarized_data$vz0[i], summarized_data$z0[i], x)


       
    if (length(y) == 100) {  # modify this condition based on the expected length
      lines(x, y, type = "l", col = rainbow(nrow(summarized_data))[i])
    }
  }
  rect(0.1, -2, 0.25, 25, col = rgb(1, 0, 0, 0.2), border = NA)
  text(border-0.035,15.7, "*View From 3B", cex = 0.7)
  text(0.175, 20, "Hitter Reaction Zone", cex = 0.7)
  # Add legend
  legend("topright", legend = summarized_data$TaggedPitchType, col = rainbow(nrow(summarized_data)), lty = 1, cex = 0.8)
}
vertical_movement_chart("Morones, Andrew", TM_Summary_10_05_11_12)