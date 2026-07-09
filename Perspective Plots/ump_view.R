# Load necessary libraries
library(ggplot2)
library(dplyr)
library(ggimage)
library(gifski)
library(grid)

# Define the function
ump_view <- function(date, inning, pa, pitcher, dataset) {
  
  # Image and background URLs
  image_url <- "https://upload.wikimedia.org/wikipedia/commons/thumb/c/c0/Baseball_%28crop%29_transparent.png/1200px-Baseball_%28crop%29_transparent.png"
  background_url <- "https://vafloc02.s3.amazonaws.com/isyn/images/f008/img-3653008-f.jpg"
  
  # Strike zone limits
  strikezone_xmin <- -0.83
  strikezone_xmax <- 0.83
  strikezone_ymin <- 1.5
  strikezone_ymax <- 3.6
  
  pitch_equation <- function(a0, v0, p0, time) {
    position = a0 * time^2 + v0 * time + p0
    return(position)
  }
  
  # Filter dataset for the selected pitcher, game, and at-bat
  filtered_data <- dataset %>% 
    filter(Date == date, Inning == inning, PAofInning == pa, Pitcher == pitcher) %>%
    arrange(PitchofPA)
  
  # If no data is found, print message and exit
  if (nrow(filtered_data) == 0) {
    print(paste("No data found for", pitcher, "on", date, "in Inning", inning, "PA", pa))
    return(NULL)
  }
  
  # Create data for animation
  image_data <- data.frame()
  
  for (i in 1:nrow(filtered_data)) {
    pitch <- filtered_data[i, ]
    time_seq <- seq(0, pitch$ZoneTime, length.out = 100)
    
    x <- pitch_equation(pitch$ax0, pitch$vx0, pitch$x0, time_seq)
    z <- pitch_equation(pitch$az0, pitch$vz0, pitch$z0, time_seq)
    
    image_data <- rbind(image_data, data.frame(
      Time = time_seq,  # Offset each pitch
      X = x,
      Z = z,
      PitchType = pitch$TaggedPitchType,
      pitch_size = 0.0005,  # Fixed size for testing
      PitchofPA = pitch$PitchofPA
    ))
  }
  
  # Download baseball image
  img_path <- tempfile(fileext = ".png")
  if (download_image(image_url, img_path)) {
    img_path <- img_path
  } else {
    stop("Failed to download the image.")
  }
  
  # Download and set background image
  bg_path <- tempfile(fileext = ".jpg")
  if (download_image(background_url, bg_path)) {
    bg_image <- rasterGrob(readJPEG(bg_path), width = unit(1, "npc"), height = unit(1, "npc"))
  } else {
    stop("Failed to download the background image.")
  }
  
  fps <- 60
  nframes <- ceiling(max(image_data$Time) * fps)
  
  # Animated plot
  p <- ggplot() +
    annotation_custom(bg_image, xmin = -10.6, xmax = 9.55, ymin = -10.5, ymax = 21.4) +
    theme_void() +
    coord_cartesian(xlim = c(-4, 4), ylim = c(-1, 7)) +
    geom_rect(aes(xmin = strikezone_xmin, xmax = strikezone_xmax, ymin = strikezone_ymin, ymax = strikezone_ymax), fill = NA, color = "black") +
    transition_reveal(Time) +
    ease_aes('linear') +
    geom_image(data = image_data, aes(x = X, y = Z, image = img_path, size = pitch_size)) +  # Use fixed size
    geom_text(data = image_data, aes(x = 0, y = 6.5, label = PitchType, group = PitchofPA), size = 6, color = "black", fontface = "bold") +
    scale_color_manual(values = c("Slider" = "red", "Cutter" = "blue", "Fastball" = "green", "Changeup" = "purple")) +
    theme(legend.position = "none")
  
  # Render and display animation in RStudio Viewer pane
  anim <- animate(p, nframes = nframes, fps = fps, renderer = gifski_renderer())
  
  print(anim)  # Show animation in RStudio Viewer pane
  return(anim)
}

# Example Call
ump_view("2024-02-18", 8, 1, "Volchko, Joey", D1TM24)