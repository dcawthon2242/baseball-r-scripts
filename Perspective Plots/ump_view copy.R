library(ggplot2)
library(dplyr)
library(gganimate)
library(png)
library(grid)
library(httr) # for download.file()
library(ggimage) # for geom_image
library(jpeg) # for readJPEG()
library(gifski) # for gifski_renderer

# Function to download the image
download_image <- function(image_url, destfile) {
  GET(image_url, write_disk(destfile, overwrite = TRUE))
  file.exists(destfile) # Check if file was downloaded
}

# Function to create the animated plot with an image and background
ump_view <- function(player_name, dataset, pitch_types = NULL, output_gif = "/Users/a13105/Downloads/gurnea_plot.gif") {
  
  # Default URLs and background placement
  image_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c0/Baseball_%28crop%29_transparent.png/1200px-Baseball_%28crop%29_transparent.png"
  background_url <- "https://vafloc02.s3.amazonaws.com/isyn/images/f008/img-3653008-f.jpg"
  bg_xmin <- -10.6
  bg_xmax <- 9.55
  bg_ymin <- -10.5
  bg_ymax <- 21.4
  
  # Strike zone parameters
  strikezone_xmin <- -0.83
  strikezone_xmax <- 0.83
  strikezone_ymin <- 1.5
  strikezone_ymax <- 3.6
  
  pitch_equation <- function(a0, v0, p0, time) {
    # Calculate the position based on the equation of motion
    position = a0 * time^2 + v0 * time + p0
    return(position)
  }
  
  # Filter data for the specified player
  filtered_data <- dataset %>% filter(Pitcher == player_name)
  
  # If pitch types are provided, filter for them
  if (!is.null(pitch_types)) {
    filtered_data <- filtered_data %>% filter(TaggedPitchType %in% pitch_types)
  }
  
  # Remove missing or non-finite values in ZoneTime
  filtered_data <- filtered_data[is.finite(filtered_data$ZoneTime), ]
  
  # Group by TaggedPitchType and calculate means
  summarized_data <- filtered_data %>% 
    group_by(TaggedPitchType) %>%
    summarize(
      ax0 = mean(ax0, na.rm = TRUE),
      vx0 = mean(vx0, na.rm = TRUE),
      x0 = mean(x0, na.rm = TRUE),
      az0 = mean(az0, na.rm = TRUE),
      vz0 = mean(vz0, na.rm = TRUE),
      z0 = mean(z0, na.rm = TRUE),
      ZoneTime = mean(ZoneTime, na.rm = TRUE)
    )
  
  # Check if there's no data after filtering
  if (nrow(summarized_data) == 0) {
    stop("No data available for the specified pitch types.")
  }
  
  common_ZoneTime <- min(summarized_data$ZoneTime)
  
  # Generate the time sequence for plotting
  time_seq <- seq(0, common_ZoneTime, length.out = 100)
  
  # Create an empty data frame for storing image positions
  image_data <- data.frame()
  
  # Loop through pitch types and calculate trajectory points
  for (i in 1:nrow(summarized_data)) {
    pitch_type <- summarized_data$TaggedPitchType[i]
    
    # Calculate x and z positions
    x <- pitch_equation(summarized_data$ax0[i], summarized_data$vx0[i], summarized_data$x0[i], time_seq)
    z <- pitch_equation(summarized_data$az0[i], summarized_data$vz0[i], summarized_data$z0[i], time_seq)
    
    # Add image data for each time point
    image_data <- rbind(image_data, data.frame(
      Time = time_seq,
      X = x, # Use the same x for simplicity
      Z = z,  # Use the same z for simplicity
      Size = time_seq / max(time_seq) * 0.06, # Normalizing size (adjust factor for desired effect)
      PitchType = pitch_type # Add pitch type column for distinguishing different pitch types
    ))
  }
  
  # Define the image URL and download it
  img_path <- tempfile(fileext = ".png")
  if (download_image(image_url, img_path)) {
    img_path <- img_path
  } else {
    stop("Failed to download the image.")
  }
  
  # Download and prepare the background image
  bg_path <- tempfile(fileext = ".jpg")
  if (download_image(background_url, bg_path)) {
    bg_image <- rasterGrob(readJPEG(bg_path), width = unit(1, "npc"), height = unit(1, "npc"))
  } else {
    stop("Failed to download the background image.")
  }
  
  # Calculate the number of frames
  fps <- 60 # Frames per second
  nframes <- ceiling(common_ZoneTime * fps) # Total number of frames
  
  # Create an animated plot
  p <- ggplot() +
    annotation_custom(bg_image, xmin = bg_xmin, xmax = bg_xmax, ymin = bg_ymin, ymax = bg_ymax) +
    theme_void() +
    coord_cartesian(xlim = c(-4, 4), ylim = c(-1, 7)) +
    geom_rect(aes(xmin = strikezone_xmin, xmax = strikezone_xmax, ymin = strikezone_ymin, ymax = strikezone_ymax), fill = NA, color = "black") +
    transition_reveal(Time) +
    ease_aes('linear') +
    # Add the image to the plot with a dynamic size and color by pitch type
    geom_image(data = image_data,
               aes(x = X, y = Z, image = img_path, size = Size, color = PitchType),
               inherit.aes = FALSE) +
    scale_size_identity() + # Ensures the size is interpreted as is
    scale_color_manual(values = c("Slider" = "red", "Cutter" = "blue")) + # Adjust colors as needed
    theme(legend.position = "none") # Remove the legend
  
  # Save the animation as a GIF
  anim <- animate(p, nframes = nframes, fps = fps, renderer = gifski_renderer(output_gif))
  
  return(output_gif)
}

# Example call:
# To view multiple pitch types for a player with an image and custom background and save as a GIF:
output_gif_path <- ump_view("Goff, Dylan", CCLSzn, pitch_types = "Slider")
print(paste("GIF saved to:", output_gif_path))
