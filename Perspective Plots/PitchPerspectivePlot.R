horizontal_movement_chart <- function(player_name, dataset, pitch_type = NULL, batter_side = NULL, arc_radius = 1.6, is_export = FALSE) {
  
  if (!requireNamespace("shape", quietly = TRUE)) install.packages("shape")
  library(shape)
  library(png)
  library(dplyr)
  
  pitch_equation <- function(a, v, p0, t) a * t^2 + v * t + p0
  
  # Initial filtering
  filtered_data <- dataset %>%
    filter(Pitcher == player_name, is.finite(ZoneTime), !is.na(x_swing_run_value), !is.na(TaggedPitchType))
  
  if (!is.null(pitch_type)) filtered_data <- filtered_data %>% filter(TaggedPitchType == pitch_type)
  if (!is.null(batter_side)) filtered_data <- filtered_data %>% filter(BatterSide == batter_side)
  
  if (nrow(filtered_data) == 0) stop("No valid data for this player.")
  
  # Tighten release point filtering
  mean_x0 <- mean(filtered_data$x0)
  mean_z0 <- mean(filtered_data$z0)
  max_release_dist <- 0.25  
  
  filtered_data <- filtered_data %>%
    mutate(release_dist = sqrt((x0 - mean_x0)^2 + (z0 - mean_z0)^2)) %>%
    filter(release_dist <= max_release_dist)
  
  # Prepare recent 50 for extreme xRV trajectories (include ReactionTime NOW)
  recent_50 <- filtered_data %>%
    arrange(desc(ZoneTime)) %>%
    head(50) %>%
    mutate(
      ReactionTime = ZoneTime - 0.15,
      x_react = pitch_equation(ax0, vx0, x0, ReactionTime),
      z_react = pitch_equation(az0, vz0, z0, ReactionTime)
    )
  
  # Filter to the 20 most recent pitches for display
  filtered_data <- recent_50 %>%
    head(20)
  
  color_scale <- colorRampPalette(c("blue", "gray", "red"))
  pitcher_throws <- unique(filtered_data$PitcherThrows)
  batter_stands <- unique(filtered_data$BatterSide)
  
  x_limits <- c(-9, 9)
  z_limits <- c(-6, 13)
  
  # Title setup
  name_parts <- unlist(strsplit(player_name, ", "))
  formatted_name <- if (length(name_parts) == 2) paste(name_parts[2], name_parts[1]) else player_name
  pitch_type_str <- ifelse(is.null(pitch_type), "All Pitches", pitch_type)
  title_text <- paste(formatted_name, pitch_type_str, "from", batter_stands, "Handed Hitter Perspective - 2025")
  
  # Load batter image
  img_path <- "C:\\Users\\dcawt\\Downloads\\BatterPerspective.png"
  batter_img <- readPNG(img_path)
  
  # Initialize plot
  par(mar = c(0, 0, 4.5, 0))
  plot(NA, xlim = x_limits, ylim = z_limits, xlab = "", ylab = "", 
       main = title_text, cex.main = 0.9, axes = FALSE)
  mtext("Red/Blue dots: 150ms Before Plate (Last Point a Hitter Can React.\n Black line: Average.\n Red/Blue lines: Best/Worst Pitches to Hit Out of Last 50 Pitches.", 
        side = 3, line = 0.25, cex = 0.4, col = "black")
  # ---- STRIKE ZONE RECTANGLE ----
  rect(xleft = -2.5, ybottom = -3.5, xright = 2.5, ytop = 7, border = "black", lwd = 2, lty = 2)
  
  # ---- MOUND DRAWING (Full Detail) ----
  xleft <- -0.75
  xright <- 0.15
  ybottom <- 3.25
  ytop <- 3.2
  
  center_x <- (xleft + xright) / 2
  center_y <- (ybottom + ytop) / 2
  
  angle_deg <- if (batter_stands == "Right") 359 else 1
  angle_rad <- angle_deg * pi / 180
  
  corners <- data.frame(
    x = c(xleft, xright, xright, xleft),
    y = c(ybottom, ybottom, ytop, ytop)
  )
  
  rotated_corners <- corners %>%
    mutate(
      x_rot = center_x + (x - center_x) * cos(angle_rad) - (y - center_y) * sin(angle_rad),
      y_rot = center_y + (x - center_x) * sin(angle_rad) + (y - center_y) * cos(angle_rad)
    )
  
  # Draw rectangle base of the mound
  for (j in 1:4) {
    x1 <- rotated_corners$x_rot[j]
    y1 <- rotated_corners$y_rot[j]
    x2 <- rotated_corners$x_rot[ifelse(j == 4, 1, j + 1)]
    y2 <- rotated_corners$y_rot[ifelse(j == 4, 1, j + 1)]
    lines(c(x1, x2), c(y1, y2), col = "black", lwd = 1)
  }
  
  # Parallel line above rectangle
  x1_top <- rotated_corners$x_rot[3]
  y1_top <- rotated_corners$y_rot[3]
  x2_top <- rotated_corners$x_rot[4]
  y2_top <- rotated_corners$y_rot[4]
  
  dir_x <- x2_top - x1_top
  dir_y <- y2_top - y1_top
  dir_length <- sqrt(dir_x^2 + dir_y^2)
  norm_dir_x <- dir_x / dir_length
  norm_dir_y <- dir_y / dir_length
  
  extension <- 0.2
  y_offset <- 0.19
  
  extended_x1 <- x1_top - (extension * norm_dir_x)
  extended_y1 <- y1_top - (extension * norm_dir_y) + y_offset
  extended_x2 <- x2_top + (extension * norm_dir_x)
  extended_y2 <- y2_top + (extension * norm_dir_y) + y_offset
  
  lines(c(extended_x1, extended_x2), c(extended_y1, extended_y2), col = "black", lwd = 1)
  
  # Angled lines from ends of the parallel line
  angle_15_rad <- 194 * (pi / 180)
  edge_angle <- atan2(dir_y, dir_x)
  
  new_angle_right <- edge_angle - angle_15_rad
  angled_x2 <- extended_x1 + cos(new_angle_right)
  angled_y2 <- extended_y1 + sin(new_angle_right)
  lines(c(extended_x1, angled_x2), c(extended_y1, angled_y2), col = "black", lwd = 1)
  
  second_angle_deg <- 345
  angle_left_rad <- second_angle_deg * (pi / 180)
  new_angle_left <- edge_angle - angle_left_rad
  angled_x2_left <- extended_x2 + cos(new_angle_left)
  angled_y2_left <- extended_y2 + sin(new_angle_left)
  lines(c(extended_x2, angled_x2_left), c(extended_y2, angled_y2_left), col = "black", lwd = 1)
  
  # Oval arc to complete the mound top
  pt1 <- c(angled_x2, angled_y2)
  pt2 <- c(angled_x2_left, angled_y2_left)
  
  oval_center_x <- (pt1[1] + pt2[1]) / 2
  oval_center_y <- (pt1[2] + pt2[2]) / 2
  oval_width <- sqrt((pt2[1] - pt1[1])^2 + (pt2[2] - pt1[2])^2)
  oval_height <- 1  # Flat top of mound
  
  oval_angle_rad <- if (batter_stands == "Right") 359 * pi / 180 else 1 * pi / 180
  theta <- seq(pi, 2 * pi, length.out = 200)
  a <- oval_width / 2
  b <- oval_height / 2
  
  x_vals <- oval_center_x + a * cos(theta) * cos(oval_angle_rad) - b * sin(theta) * sin(oval_angle_rad)
  y_vals <- oval_center_y + a * cos(theta) * sin(oval_angle_rad) + b * sin(theta) * cos(oval_angle_rad)
  
  lines(x_vals, y_vals, col = "black", lwd = 1)
  
  
  # ---- DOT OFFSET BASED ON PITCH TYPE ----
  dot_offset <- if (!is.null(pitch_type) && pitch_type %in% c("Fastball", "TwoSeamFastball", "FourSeamFastball", "Sinker")) 0.15 else 0.15
  
  # ---- PLOT INDIVIDUAL PITCH TRAJECTORIES ----
  min_val <- min(filtered_data$x_swing_run_value)
  max_val <- max(filtered_data$x_swing_run_value)
  range_val <- max_val - min_val
  
  for (i in 1:nrow(filtered_data)) {
    pitch <- filtered_data[i, ]
    time_seq <- seq(0, pitch$ReactionTime, length.out = 100)
    x_traj <- pitch_equation(pitch$ax0, pitch$vx0, pitch$x0, time_seq)
    z_traj <- pitch_equation(pitch$az0, pitch$vz0, pitch$z0, time_seq)
    
    if (range_val > 0) {
      norm_value <- (pitch$x_swing_run_value - min_val) / range_val
      color_idx <- max(1, min(100, round(norm_value * 99) + 1))
      pitch_color <- color_scale(100)[color_idx]
    } else {
      pitch_color <- color_scale(100)[50]
    }
    
    rgb_values <- col2rgb(pitch_color)
    lines(x_traj, z_traj, col = rgb(rgb_values[1], rgb_values[2], rgb_values[3], maxColorValue = 255, alpha = 128), lwd = 1)
    points(pitch$x_react, pitch$z_react, col = pitch_color, pch = 16, cex = 1.2)
  }
  # ---- AVERAGE TRAJECTORY ----
  avg_ax0 <- mean(filtered_data$ax0)
  avg_vx0 <- mean(filtered_data$vx0)
  avg_x0  <- mean(filtered_data$x0)
  avg_az0 <- mean(filtered_data$az0)
  avg_vz0 <- mean(filtered_data$vz0)
  avg_z0  <- mean(filtered_data$z0)
  avg_reaction_time <- mean(filtered_data$ReactionTime)
  dot_time <- avg_reaction_time + dot_offset
  
  time_seq_full <- seq(0, avg_reaction_time + dot_offset, length.out = 200)
  avg_x_traj_full <- pitch_equation(avg_ax0, avg_vx0, avg_x0, time_seq_full)
  avg_z_traj_full <- pitch_equation(avg_az0, avg_vz0, avg_z0, time_seq_full)
  
  reaction_idx_avg <- which.min(abs(time_seq_full - avg_reaction_time))
  
  avg_x_main <- avg_x_traj_full[1:reaction_idx_avg]
  avg_z_main <- avg_z_traj_full[1:reaction_idx_avg]
  avg_x_tail <- avg_x_traj_full[reaction_idx_avg:length(avg_x_traj_full)]
  avg_z_tail <- avg_z_traj_full[reaction_idx_avg:length(avg_z_traj_full)]
  
  lines(avg_x_main, avg_z_main, col = rgb(0, 0, 0, alpha = 1), lwd = 1, lty = 2)
  lines(avg_x_tail, avg_z_tail, col = rgb(0, 0, 0, alpha = 0.4), lwd = 1, lty = 2)
  
  x_at_dot <- tail(avg_x_traj_full, 1)
  z_at_dot <- tail(avg_z_traj_full, 1)
  points(x_at_dot, z_at_dot, col = "black", pch = 16, cex = 1.5)
  
  # ---- EXTREME xRV TRAJECTORIES ----
  plot_extreme_trajectory <- function(pitch_row, color) {
    full_time_seq <- seq(0, pitch_row$ReactionTime + dot_offset, length.out = 200)
    x_traj <- pitch_equation(pitch_row$ax0, pitch_row$vx0, pitch_row$x0, full_time_seq)
    z_traj <- pitch_equation(pitch_row$az0, pitch_row$vz0, pitch_row$z0, full_time_seq)
    
    reaction_idx <- which.min(abs(full_time_seq - pitch_row$ReactionTime))
    x_main <- x_traj[1:reaction_idx]
    z_main <- z_traj[1:reaction_idx]
    x_tail <- x_traj[reaction_idx:length(x_traj)]
    z_tail <- z_traj[reaction_idx:length(z_traj)]
    
    rgb_vals <- col2rgb(color) / 255
    
    lines(x_main, z_main, col = rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = 1), lwd = 1.5)
    lines(x_tail, z_tail, col = rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = 0.4), lwd = 1.5)
    
    points(tail(x_traj, 1), tail(z_traj, 1), 
           col = rgb(rgb_vals[1], rgb_vals[2], rgb_vals[3], alpha = 0.4), 
           pch = 16, cex = 1.5)
  }
  
  # Use recent 50 for xRV extremes
  low_xRV_rows <- recent_50 %>% arrange(x_swing_run_value) %>% head(5)
  high_xRV_rows <- recent_50 %>% arrange(desc(x_swing_run_value)) %>% head(5)
  
  avg_low <- low_xRV_rows %>% summarise(
    ax0 = mean(ax0), vx0 = mean(vx0), x0 = mean(x0),
    az0 = mean(az0), vz0 = mean(vz0), z0 = mean(z0),
    ReactionTime = mean(ReactionTime)
  )
  
  avg_high <- high_xRV_rows %>% summarise(
    ax0 = mean(ax0), vx0 = mean(vx0), x0 = mean(x0),
    az0 = mean(az0), vz0 = mean(vz0), z0 = mean(z0),
    ReactionTime = mean(ReactionTime)
  )
  
  plot_extreme_trajectory(avg_low, "blue")
  plot_extreme_trajectory(avg_high, "red")
  
  # ---- BATTER IMAGE PLACEMENT ----
  xleft_adj <- ifelse(batter_stands == "Right", -9, 3)
  xright_adj <- ifelse(batter_stands == "Right", -3, 8)
  ybottom_adj <- -14.5
  ytop_adj <- 21.5
  
  # Always flip the image for RIGHT-handed batters, regardless of export mode
  # This ensures consistency between interactive and PDF output
  if (batter_stands == "Right") {
    batter_img <- batter_img[, ncol(batter_img):1, ]  # Flip horizontally
  }
  
  rasterImage(batter_img,
              xleft = xleft_adj, 
              ybottom = ybottom_adj, 
              xright = xright_adj, 
              ytop = ytop_adj,
              interpolate = FALSE)
  
  # ---- FINAL LEGEND ----
  color_legend <- color_scale(100)
  legend("topright", legend = c("Swing is Good", "Mid", "Swing is Bad"),
         fill = color_legend[c(100, 50, 1)], border = "black", bty = "n", cex = 0.5)
}

save_pitcher_plots <- function(pitcher_name, dataset) {
  # Get all pitch types thrown by this pitcher
  filtered_data <- dataset %>%
    filter(Pitcher == pitcher_name, !is.na(TaggedPitchType), !is.na(BatterSide))
  
  if (nrow(filtered_data) == 0) {
    stop("No data found for that pitcher.")
  }
  
  # Get all pitch types and both batter sides
  pitch_types <- unique(filtered_data$TaggedPitchType)
  batter_sides <- c("Left", "Right")
  
  # Format name: "Last, First" → "First_Last"
  name_parts <- unlist(strsplit(pitcher_name, ", "))
  formatted_name <- if (length(name_parts) == 2) paste(name_parts[2], name_parts[1], sep = "_") else pitcher_name
  
  for (pitch_type in pitch_types) {
    for (batter_side in batter_sides) {
      # Create filename and path
      file_name <- paste0(formatted_name, "_", pitch_type, "_vs_", batter_side, "Handed.pdf")
      file_path <- file.path(getwd(), file_name)
      
      # Save the plot with is_export flag set to TRUE
      pdf(file_path, width = 9, height = 6)
      tryCatch({
        par(mar = c(0, 0, 4, 0))
        horizontal_movement_chart(
          pitcher_name,
          dataset,
          pitch_type = pitch_type,
          batter_side = batter_side,
          is_export = TRUE  # Signal that we're in export mode
        )
      }, error = function(e) {
        message(paste("Could not create plot for", file_name, ":", e$message))
      })
      dev.off()
      
      message(paste("Saved:", file_name))
    }
  }
  
  message("All plots saved to:", getwd())
}

# Example usage:
horizontal_movement_chart("Meyer, Gavin", D1TM25, pitch_type = "Slider", batter_side = "Right")