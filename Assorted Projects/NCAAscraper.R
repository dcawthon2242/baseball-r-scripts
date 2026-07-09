# Load the required package
library(baseballr)

# Define the range of years and division
years <- 2013:2024
division <- 1

# Initialize an empty list to store the results
ncaa_data_list <- list()

# Loop through each year
for (year in years) {
  cat("Processing year:", year, "\n")
  
  # Get the list of teams for the given year and division
  team_info <- ncaa_teams(year = year, division = division)
  
  # Loop through each team and get the school ID
  for (team_name in team_info$team_names) {
    cat("Processing team:", team_name, "in year:", year, "\n")
    
    # Use ncaa_school_id_lu to find the school ID for the team
    school_info <- ncaa_school_id_lu(school = team_name)
    
    # Check if school_info is not NULL and has rows
    if (!is.null(school_info) && nrow(school_info) > 0) {
      # Extract the school ID and print it
      school_id <- school_info$school_id[1] # Assuming we take the first match
      cat("Found school ID:", school_id, "for team:", team_name, "\n")
      
      # Scrape team player stats using the school ID
      try({
        player_stats <- ncaa_team_player_stats(
          team_id = school_id, 
          year = year, 
          stat_type = "batting"
        )
        
        # Check if player_stats is not NULL or empty
        if (!is.null(player_stats) && nrow(player_stats) > 0) {
          # Add the data to the list, appending the year and team for reference
          player_stats$year <- year
          player_stats$team_name <- team_name
          ncaa_data_list[[paste0(team_name, "_", year)]] <- player_stats
          
          cat("Data for", team_name, "in year", year, "scraped successfully.\n")
        } else {
          cat("No data for", team_name, "in year", year, "\n")
        }
      }, silent = TRUE) # Ignore errors and continue
    } else {
      cat("No school ID found for", team_name, "in year", year, "\n")
    }
  }
}

# Combine all the data into one large data frame
ncaa_combined_data <- do.call(rbind, ncaa_data_list)

# Save the data to a CSV file
if (!is.null(ncaa_combined_data) && nrow(ncaa_combined_data) > 0) {
  write.csv(ncaa_combined_data, "ncaa_team_player_batting_stats_2013_2024.csv", row.names = FALSE)
  cat("Data saved to ncaa_team_player_batting_stats_2013_2024.csv\n")
} else {
  cat("No data was scraped.\n")
}

cat("Data scraping complete!\n")
