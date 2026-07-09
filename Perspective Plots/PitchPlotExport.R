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
      
      # Save the plot
      pdf(file_path, width = 9, height = 6)
      tryCatch({
        par(mar = c(0, 0, 4, 0))
        horizontal_movement_chart(
          pitcher_name,
          dataset,
          pitch_type = pitch_type,
          batter_side = batter_side
        )
      }, error = function(e) {
        message(paste("Could not create plot for", file_name, ":", e$message))
      })
      dev.off()
      
      # Clear export flag
      rm("IS_EXPORT_MODE", envir = .GlobalEnv)
      
      message(paste("Saved:", file_name))
    }
  }
  
  message("All plots saved to:", getwd())
}
