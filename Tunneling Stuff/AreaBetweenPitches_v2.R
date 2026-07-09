#==============================================================================
# FINAL SCRIPT v17 (Weighted Trajectory Optimization)
#==============================================================================

# Step 1: Load Necessary Libraries
library(dplyr)
library(jsonlite)
library(parallel)
library(progressr)
library(progress)

# --- Configure the progress bar handler ---
handlers(global = TRUE)
handlers("progress")


# Step 2: Set Up the Parallel Processing Cluster
no_cores <- detectCores() - 1
cat(sprintf("Detected %d cores. Creating a cluster with %d workers...\n", no_cores + 1, no_cores))
cl <- makeCluster(no_cores)
cat("Cluster created successfully.\n")


# ✅ --- Step 3: Modified Function to Calculate Distance Components ---
# This function now returns the integrated squared differences for x, y, and z.
calculate_trajectory_components <- function(pitch1_data, pitch2_data, tof1, tof2, h = 0.01, n_points = 200) {
  tryCatch({
    if (!is.data.frame(pitch1_data) || !is.data.frame(pitch2_data) || nrow(pitch1_data) == 0 || nrow(pitch2_data) == 0) {
      return(c(ix2 = NA_real_, iy2 = NA_real_, iz2 = NA_real_))
    }
    t_pitch1 <- seq(0, tof1, length.out = nrow(pitch1_data))
    t_pitch2 <- seq(0, tof2, length.out = nrow(pitch2_data))
    max_tof <- max(tof1, tof2, na.rm = TRUE)
    if (is.na(max_tof) || !is.finite(max_tof)) return(c(ix2 = NA_real_, iy2 = NA_real_, iz2 = NA_real_))
    t_common <- seq(0, max_tof, length.out = n_points)
    delta_t <- t_common[2] - t_common[1]
    
    regress_pitch <- function(t_regress, t_data, pitch_coords, h_bw) {
      time_diff_matrix <- outer(t_regress, t_data, "-")
      weights_matrix <- exp(-0.5 * (time_diff_matrix / h_bw)^2)
      sum_of_weights <- rowSums(weights_matrix, na.rm = TRUE)
      sum_of_weights[sum_of_weights == 0] <- 1
      if (ncol(weights_matrix) != nrow(pitch_coords)) return(data.frame(x=NA, y=NA, z=NA))
      regressed_x <- (weights_matrix %*% pitch_coords$x) / sum_of_weights
      regressed_y <- (weights_matrix %*% pitch_coords$y) / sum_of_weights
      regressed_z <- (weights_matrix %*% pitch_coords$z) / sum_of_weights
      return(data.frame(x = regressed_x, y = regressed_y, z = regressed_z))
    }
    
    coords1 <- regress_pitch(t_common, t_pitch1, pitch1_data, h)
    coords2 <- regress_pitch(t_common, t_pitch2, pitch2_data, h)
    
    # Calculate the integrated squared differences
    ix2 <- sum(delta_t * (coords1$x - coords2$x)^2, na.rm = TRUE)
    iy2 <- sum(delta_t * (coords1$y - coords2$y)^2, na.rm = TRUE)
    iz2 <- sum(delta_t * (coords1$z - coords2$z)^2, na.rm = TRUE)
    
    return(c(ix2 = ix2, iy2 = iy2, iz2 = iz2))
  }, error = function(e) {
    return(c(ix2 = NA_real_, iy2 = NA_real_, iz2 = NA_real_))
  })
}


# Step 4: Prepare Data
cat("Preparing data for parallel processing...\n")
pbp_2022_2024$trajectory_df <- NULL
pbp_2022_2024$trajectory_df <- pbp_2022_2024$trajectories
pbp_2022_2024$trajectory_df <- lapply(pbp_2022_2024$trajectory_df, as.data.frame)
pbp_2022_2024 <- pbp_2022_2024 %>%
  arrange(game_pk, at_bat_number, pitch_number)

indices_to_process <- which(
  pbp_2022_2024$game_pk == lag(pbp_2022_2024$game_pk) &
    pbp_2022_2024$at_bat_number == lag(pbp_2022_2024$at_bat_number)
)
pitch1_list <- pbp_2022_2024$trajectory_df[indices_to_process - 1]
pitch2_list <- pbp_2022_2024$trajectory_df[indices_to_process]
tof1_vec <- pbp_2022_2024$time_of_flight[indices_to_process - 1]
tof2_vec <- pbp_2022_2024$time_of_flight[indices_to_process]
cat(sprintf("Found %d pitch pairs to process.\n", length(indices_to_process)))


# Step 5: Run Parallel Computation to get Distance Components
cat("Exporting component calculation function to parallel workers...\n")
clusterExport(cl, varlist=c("calculate_trajectory_components"), envir=environment())
total_items <- length(indices_to_process)
chunks_of_indices <- split(1:total_items, ceiling(seq_along(1:total_items) / ceiling(total_items/100)))
results_list <- list() 
cat(sprintf("Data split into %d chunks. Starting computation...\n", length(chunks_of_indices)))
with_progress({
  p <- progressr::progressor(steps = length(chunks_of_indices))
  for (i in 1:length(chunks_of_indices)) {
    current_indices <- chunks_of_indices[[i]]
    args_for_chunk <- lapply(current_indices, function(idx) {
      list(p1 = pitch1_list[[idx]], p2 = pitch2_list[[idx]], t1 = tof1_vec[idx], t2 = tof2_vec[idx])
    })
    chunk_results <- parLapply(cl, args_for_chunk, function(arg) {
      calculate_trajectory_components(pitch1_data = arg$p1, pitch2_data = arg$p2, tof1 = arg$t1, tof2 = arg$t2)
    })
    results_list <- c(results_list, chunk_results)
    p() 
  }
})
stopCluster(cl)
cat("\nCluster shut down.\n")


# Step 6: Assemble Data and Run Optimization
cat("\n--- Starting Optimization for Trajectory Weights ---\n")

# --- 6a: Assemble components into a data frame for modeling ---
cat("Assembling component data for optimization...\n")
components_df <- as.data.frame(do.call(rbind, results_list))

# --- 6b: Prepare data for the model ---
# Get plate locations for Euclidean distance and join with components
plate_locations <- pbp_2022_2024[indices_to_process, ] %>%
  mutate(
    delta_x_loc = plate_x - lag(plate_x),
    delta_z_loc = plate_z - lag(plate_z)
  ) %>%
  select(swing_decision_rv, delta_x_loc, delta_z_loc)

# Combine into a single model data frame
model_data <- cbind(components_df, plate_locations) %>% na.omit()
cat(sprintf("Running optimization on %d complete observations.\n", nrow(model_data)))

# --- 6c: Define the Objective Function ---
calculate_correlation_trajectory <- function(weights, data) {
  w_x <- weights[1]
  w_y <- weights[2]
  w_z <- weights[3]
  
  # Calculate the weighted trajectory distance from the pre-calculated components
  weighted_trajectory_dist <- sqrt(w_x * data$ix2 + w_y * data$iy2 + w_z * data$iz2)
  
  # Calculate the simple Euclidean distance at the plate
  euclidean_dist <- sqrt(data$delta_x_loc^2 + data$delta_z_loc^2)
  euclidean_dist[euclidean_dist == 0] <- 0.0005
  
  # Calculate the final ratio
  ratio <- weighted_trajectory_dist / euclidean_dist
  
  correlation <- cor(ratio, data$swing_decision_rv, method = "spearman")
  
  return(-correlation)
}

# --- 6d: Run the Optimization ---
cat("Finding optimal weights for trajectory x, y, and z... (This may take a moment)\n")
optimization_result <- optim(
  par = c(1, 1, 1),
  fn = calculate_correlation_trajectory,
  data = model_data,
  method = "L-BFGS-B",
  lower = c(0, 0, 0)
)

# --- 6e: Display Optimization Results ---
optimal_weights <- optimization_result$par
max_correlation <- -optimization_result$value

cat("\n--- Trajectory Optimization Complete ---\n")
cat(sprintf("Optimal Weight for Trajectory X: %f\n", optimal_weights[1]))
cat(sprintf("Optimal Weight for Trajectory Y: %f\n", optimal_weights[2]))
cat(sprintf("Optimal Weight for Trajectory Z: %f\n", optimal_weights[3]))
cat(sprintf("Maximum Non-Linear Correlation Achieved: %f\n", max_correlation))


# Step 7: Calculate Final Columns with Optimal Weights
cat("\nCalculating final distance and ratio columns using optimal weights...\n")

# Calculate the final weighted trajectory distance for ALL processed pairs
final_weighted_dist <- sqrt(optimal_weights[1] * components_df$ix2 + optimal_weights[2] * components_df$iy2 + optimal_weights[3] * components_df$iz2)

# Assign the final weighted distance to the main data frame
pbp_2022_2024$weighted_trajectory_distance <- NA_real_
pbp_2022_2024$weighted_trajectory_distance[indices_to_process] <- final_weighted_dist

# Calculate the final ratio
plate_x1 <- pbp_2022_2024$plate_x[indices_to_process - 1]
plate_z1 <- pbp_2022_2024$plate_z[indices_to_process - 1]
plate_x2 <- pbp_2022_2024$plate_x[indices_to_process]
plate_z2 <- pbp_2022_2024$plate_z[indices_to_process]
final_euclidean_dist <- sqrt((plate_x1 - plate_x2)^2 + (plate_z1 - plate_z2)^2)
final_euclidean_dist[final_euclidean_dist == 0] <- 0.0005

pbp_2022_2024$final_ratio <- NA_real_
pbp_2022_2024$final_ratio[indices_to_process] <- pbp_2022_2024$weighted_trajectory_distance[indices_to_process] / final_euclidean_dist


# Step 8: View Your Final Results
cat("\nHere's a preview of the final results:\n")
print(head(select(pbp_2022_2024, game_pk, at_bat_number, pitch_number, weighted_trajectory_distance, final_ratio)))
cat("\nSummary of the final weighted trajectory distance:\n")
summary(pbp_2022_2024$weighted_trajectory_distance)
cat("\nSummary of the final ratio:\n")
summary(pbp_2022_2024$final_ratio)

cat("\nProcessing complete.\n")