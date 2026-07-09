#==============================================================================
# FINAL SCRIPT v20 (Using Pre-Calculated Columns)
#==============================================================================

# Step 1: Load Necessary Libraries
cat("Loading libraries...\n")
library(dplyr)
library(parallel)
library(progressr)
library(progress)

# --- Configure the progress bar handler ---
handlers(global = TRUE)
handlers("progress")

#==============================================================================
# Step 2: Initial Data Preparation
#==============================================================================
# This script assumes that the input dataframe 'pbp_2022_2024' already contains
# the 'trajectories' and 'chase_above_expected' columns.

cat("Performing initial data preparation...\n")

# Arrange data by pitch sequence for pairing
pbp_2022_2024 <- pbp_2022_2024 %>%
  arrange(game_pk, at_bat_number, pitch_number)

#==============================================================================
# Step 3: Optimize Trajectory Weights against `chase_above_expected`
#==============================================================================
cat("\n--- Starting Trajectory Weight Optimization ---\n")

# --- 3a: Identify pitch pairs to process ---
indices_to_process <- which(
  pbp_2022_2024$game_pk == lag(pbp_2022_2024$game_pk) &
    pbp_2022_2024$at_bat_number == lag(pbp_2022_2024$at_bat_number)
)

# --- 3b: Define function to get distance components ---
calculate_trajectory_components <- function(pitch1_data, pitch2_data, tof1, tof2, h = 0.01, n_points = 50) {
  tryCatch({
    t_pitch1 <- seq(0, tof1, length.out = nrow(pitch1_data)); t_pitch2 <- seq(0, tof2, length.out = nrow(pitch2_data))
    max_tof <- max(tof1, tof2, na.rm = TRUE); t_common <- seq(0, max_tof, length.out = n_points)
    delta_t <- t_common[2] - t_common[1]
    regress_pitch <- function(t_regress, t_data, pitch_coords, h_bw) {
      time_diff_matrix <- outer(t_regress, t_data, "-"); weights_matrix <- exp(-0.5 * (time_diff_matrix / h_bw)^2)
      sum_of_weights <- rowSums(weights_matrix, na.rm = TRUE); sum_of_weights[sum_of_weights == 0] <- 1
      regressed_x <- (weights_matrix %*% pitch_coords$x) / sum_of_weights
      regressed_y <- (weights_matrix %*% pitch_coords$y) / sum_of_weights
      regressed_z <- (weights_matrix %*% pitch_coords$z) / sum_of_weights
      return(data.frame(x = regressed_x, y = regressed_y, z = regressed_z))
    }
    coords1 <- regress_pitch(t_common, t_pitch1, pitch1_data, h); coords2 <- regress_pitch(t_common, t_pitch2, pitch2_data, h)
    ix2 <- sum(delta_t * (coords1$x - coords2$x)^2, na.rm = TRUE)
    iy2 <- sum(delta_t * (coords1$y - coords2$y)^2, na.rm = TRUE)
    iz2 <- sum(delta_t * (coords1$z - coords2$z)^2, na.rm = TRUE)
    return(c(ix2 = ix2, iy2 = iy2, iz2 = iz2))
  }, error = function(e) { return(c(ix2 = NA, iy2 = NA, iz2 = NA)) })
}

# --- 3c: Run parallel calculation to get trajectory components ---
cat("Calculating trajectory components in parallel...\n")
cl <- makeCluster(detectCores() - 1)
clusterExport(cl, varlist=c("calculate_trajectory_components"), envir=environment())

# Prepare lists for parallel processing
pitch1_list <- pbp_2022_2024$trajectories[indices_to_process - 1]
pitch2_list <- pbp_2022_2024$trajectories[indices_to_process]
tof1_vec <- pbp_2022_2024$time_of_flight[indices_to_process - 1]
tof2_vec <- pbp_2022_2024$time_of_flight[indices_to_process]

with_progress({
  results_list <- parLapply(cl, 1:length(pitch1_list), function(i) {
    calculate_trajectory_components(pitch1_list[[i]], pitch2_list[[i]], tof1_vec[i], tof2_vec[i])
  })
})
stopCluster(cl)

# --- 3d: Assemble data for modeling ---
cat("\n--- Assembling data for final optimization ---\n")
components_df <- as.data.frame(do.call(rbind, results_list))

current_pitch_data <- pbp_2022_2024[indices_to_process, ] %>%
  select(chase_above_expected, plate_x, plate_z)

previous_pitch_data <- pbp_2022_2024[indices_to_process - 1, ] %>%
  select(prev_plate_x = plate_x, prev_plate_z = plate_z)

model_data <- cbind(
  current_pitch_data,
  previous_pitch_data,
  components_df
) %>%
  na.omit()

# --- 3e: Define the objective function ---
calculate_correlation_vs_chase <- function(weights, data) {
  w_x <- weights[1]; w_y <- weights[2]; w_z <- weights[3]
  weighted_trajectory_dist <- sqrt(w_x * data$ix2 + w_y * data$iy2 + w_z * data$iz2)
  euclidean_dist <- sqrt((data$plate_x - data$prev_plate_x)^2 + (data$plate_z - data$prev_plate_z)^2)
  euclidean_dist[euclidean_dist == 0] <- 0.0005
  ratio <- weighted_trajectory_dist / euclidean_dist
  correlation <- cor(ratio, data$chase_above_expected, method = "spearman")
  return(-correlation) # Return negative because optim() minimizes
}

# --- 3f: Run final optimization ---
cat(sprintf("Running optimization on %d observations...\n", nrow(model_data)))

if (nrow(model_data) > 0) {
  optimization_result <- optim(
    par = c(1, 1, 1),
    fn = calculate_correlation_vs_chase,
    data = model_data,
    method = "L-BFGS-B",
    lower = c(0, 0, 0)
  )
  
  optimal_weights <- optimization_result$par
  cat("\n--- Trajectory Optimization vs. Chase Complete ---\n")
  cat(sprintf("Optimal Weight for Trajectory X: %f\n", optimal_weights[1]))
  cat(sprintf("Optimal Weight for Trajectory Y: %f\n", optimal_weights[2]))
  cat(sprintf("Optimal Weight for Trajectory Z: %f\n", optimal_weights[3]))
  cat(sprintf("Maximum Correlation with chase_above_expected: %f\n", -optimization_result$value))
  
  # --- 3g: Calculate final column with optimal weights ---
  cat("Calculating final 'weighted_trajectory_distance' column...\n")
  final_weighted_dist <- sqrt(optimal_weights[1] * components_df$ix2 + optimal_weights[2] * components_df$iy2 + optimal_weights[3] * components_df$iz2)
  pbp_2022_2024$weighted_trajectory_distance <- NA_real_
  pbp_2022_2024$weighted_trajectory_distance[indices_to_process] <- final_weighted_dist
  
} else {
  cat("Skipping optimization because no valid data was available.\n")
}

cat("\nProcessing complete.\n")