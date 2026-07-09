# Install and load the required packages
install.packages("Rcpp")
install.packages("dplyr")
library(Rcpp)
library(dplyr)

# Source the C++ code for calculating the pitch trajectory distance
cppCode <- '
#include <Rcpp.h>
#include <cmath>
using namespace Rcpp;

// [[Rcpp::export]]
double trajectory_distance(NumericVector x1, NumericVector y1, NumericVector z1,
                           NumericVector x0, NumericVector y0, NumericVector z0,
                           double reaction_time) {
  // Determine the length of the shorter trajectory
  std::size_t len = std::min(x1.size(), x0.size());
  
  double total_distance = 0.0;
  double delta_time = reaction_time / len;

  for (std::size_t i = 0; i < len; ++i) {
    double dx = x1[i] - x0[i];
    double dy = y1[i] - y0[i];
    double dz = z1[i] - z0[i];
    total_distance += std::sqrt(dx * dx + dy * dy + dz * dz) * delta_time;
  }

  return total_distance;
}
'

# Compile the C++ code
sourceCpp(code = cppCode)


# Ensure trajectory columns are numeric lists
pbp_2022_2024$x_trajectory <- lapply(pbp_2022_2024$x_trajectory, as.numeric)
pbp_2022_2024$y_trajectory <- lapply(pbp_2022_2024$y_trajectory, as.numeric)
pbp_2022_2024$z_trajectory <- lapply(pbp_2022_2024$z_trajectory, as.numeric)

# Grouping and initializing
pbp_2022_2024 <- pbp_2022_2024 %>%
  group_by(game_pk, at_bat_number) %>%
  arrange(pitch_number) %>%
  mutate(pitch_trajectory_distance = NA_real_)

# Initialize counter
row_counter <- 0

# Loop through each group (at-bat)
for (grp in unique(pbp_2022_2024$game_pk)) {
  at_bats <- pbp_2022_2024 %>% filter(game_pk == grp)
  
  for (ab in unique(at_bats$at_bat_number)) {
    pitches <- at_bats %>% filter(at_bat_number == ab)
    
    if (nrow(pitches) < 2) next  # Skip if less than 2 pitches
    
    for (i in 2:nrow(pitches)) {
      # Current and previous pitch trajectories
      x1 <- unlist(pitches$x_trajectory[[i]])
      y1 <- unlist(pitches$y_trajectory[[i]])
      z1 <- unlist(pitches$z_trajectory[[i]])
      
      x0 <- unlist(pitches$x_trajectory[[i - 1]])
      y0 <- unlist(pitches$y_trajectory[[i - 1]])
      z0 <- unlist(pitches$z_trajectory[[i - 1]])
      
      # Reaction zone bounds
      reaction_time1 <- pitches$reaction_zone[i]
      reaction_time0 <- pitches$reaction_zone[i - 1]
      
      # Use the shorter reaction_zone as common time
      common_reaction_time <- min(reaction_time1, reaction_time0)
      
      # Calculate distance using the Rcpp function
      total_distance <- trajectory_distance(x1, y1, z1, x0, y0, z0, common_reaction_time)
      
      # Assign result
      pbp_2022_2024$pitch_trajectory_distance[
        pbp_2022_2024$game_pk == grp &
          pbp_2022_2024$at_bat_number == ab &
          pbp_2022_2024$pitch_number == pitches$pitch_number[i]
      ] <- total_distance
      
      # Increment counter
      row_counter <- row_counter + 1
      if (row_counter %% 1000 == 0) {
        print(paste("Processed", row_counter, "rows..."))
      }
    }
  }
}

print("Processing complete!")