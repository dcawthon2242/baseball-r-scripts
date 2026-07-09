# ==============================================================================
# FINDING MEAN DELTA_RUN_EXPECTANCY BY COUNT AND EVENT
# ============================================================================================================================================================


pbp_2022_2024$decision <- with(pbp_2022_2024, 
                               ifelse(description %in% c("ball", "blocked_ball", "pitchout"), "ball",
                                      ifelse(description == "hit_into_play", "hit_into_play",
                                             ifelse(description %in% c("swinging_strike", "foul_tip", "missed_bunt", "swinging_strike_blocked", "bunt_foul_tip"), "swinging_strike",
                                                    ifelse(description == "called_strike", "called_strike",
                                                           ifelse(description == "hit_by_pitch", "hit_by_pitch",
                                                                  ifelse(description %in% c("foul", "foul_bunt", "foul_pitchout"), "foul", NA)))))))


pbp_2022_2024$event <- with(pbp_2022_2024,
                            ifelse(events %in% "strikeout", "strikeout",
                                   ifelse(events %in% c("field_out", "force_out", "grounded_into_double_play", "fielders_choice_out", "fielders_choice", "double_play", "sac_fly", "sac_fly_double_play"), "field_out",
                                          ifelse(events == "hit_by_pitch", "hit_by_pitch",
                                                 ifelse(events == "double", "double",
                                                        ifelse(events == "single", "single",
                                                               ifelse(events == "triple", "triple",
                                                                      ifelse(events == "home_run", "home_run",
                                                                             ifelse(events == "walk", "walk",
                                                                                    ifelse(events == "wild_pitch", "wild_pitch", NA))))))))))
pbp_2022_2024$swing <- ifelse(pbp_2022_2024$decision %in% c("swinging_strike", "hit_into_play", "foul"), 1, 0)

pbp_2022_2024 <- pbp_2022_2024 %>%
  mutate(decision = ifelse(decision == "hit_into_play", event, decision)) %>%
  filter(!is.na(decision))

run_expectancy_output <- pbp_2022_2024 %>%
  group_by(decision, balls, strikes) %>%
  summarise(
    avg_delta_run_exp = mean(delta_run_exp, na.rm = TRUE),
    count = n()  # Count of rows for each group
  ) %>%
  mutate(count = paste(balls, strikes, sep = "-")) %>%
  ungroup()

ggplot(data = run_expectancy_output) +
  geom_bar(mapping = aes(x = count, y = avg_delta_run_exp), stat = "identity") +
  facet_wrap(~ decision) +
  labs(x = "Balls-Strikes Count", y = "Average Delta Run Expectancy", title = "Average Delta Run Expectancy by Count and Decision") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggplot(data = run_expectancy_output, aes(x=x, y=y) ) +
  geom_hex() +
  theme_bw()

pbp_2022_2024 <- pbp_2022_2024 %>%
  left_join(run_expectancy_output, by = c("decision" = "decision", "balls", "strikes")) %>%
  rename(avg_delta_run_exp_mapped = avg_delta_run_exp)

pbp_2022_2024_swing <- filter(pbp_2022_2024, swing == 1)
pbp_2022_2024_take <- filter(pbp_2022_2024, swing == 0)

#===============================================================================
# DATA PREP
#===============================================================================
# Creating vertical acceleration adjusted for arm angle
model <- lm(az ~ arm_angle, data = pbp_2022_2024)

pbp_2022_2024 <- pbp_2022_2024 %>%
  mutate(az_AA = az - predict(model, newdata = pbp_2022_2024) + mean(az))

# Creating horizontal acceleration adjusted for arm angle
model <- lm(ax ~ arm_angle, data = pbp_2022_2024)

pbp_2022_2024 <- pbp_2022_2024 %>%
  mutate(ax_AA = ax - predict(model, newdata = pbp_2022_2024) + mean(ax))

#===============================================================================
# PITCH xRV MODEL
#===============================================================================
# Prepare features for training
one_hot_pbp_2022_2024 <- pbp_2022_2024 %>%
  select(release_speed, release_spin_rate, spin_axis, release_pos_x, release_pos_z, az, ax, release_pos_x, avg_delta_run_exp_mapped)

# Prepare features for full dataset
one_hot_pbp_2022_2024_full <- pbp_2022_2024 %>%
  select(release_speed, release_spin_rate, spin_axis, release_pos_x, release_pos_z, az, ax)

# Identify columns present in training dataset but not in full dataset
training_columns <- colnames(one_hot_pbp_2022_2024 %>% select(-avg_delta_run_exp_mapped))
missing_columns <- setdiff(training_columns, colnames(one_hot_pbp_2022_2024_full))

# Add missing columns to full dataset with 0 values if necessary
for (col in missing_columns) {
  one_hot_pbp_2022_2024_full[[col]] <- 0
}

# Ensure columns are in the same order
one_hot_pbp_2022_2024_full <- one_hot_pbp_2022_2024_full[, training_columns]

# Prepare training data
X_train <- one_hot_pbp_2022_2024 %>% select(-avg_delta_run_exp_mapped)
y_train <- one_hot_pbp_2022_2024$avg_delta_run_exp_mapped

# Convert to matrix format
X_train_matrix <- as.matrix(X_train)
X_full_matrix <- as.matrix(one_hot_pbp_2022_2024_full)

# Create LightGBM dataset for training
lgb_train <- lgb.Dataset(data = X_train_matrix, label = y_train)

# Modified Bayesian Optimization Function
mean_delta_objective <- function(num_leaves, max_depth, learning_rate) {
  # Prepare parameters
  params <- list(
    objective = "regression",
    metric = "rmse",  # Using RMSE for regression
    num_leaves = round(num_leaves),
    max_depth = round(max_depth),
    learning_rate = learning_rate,
    min_data_in_leaf = 30,  # Fixed value
    feature_pre_filter = FALSE,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 1
  )
  
  # Perform cross-validation
  cv_results <- lgb.cv(
    params = params,
    data = lgb_train,
    nrounds = 1000,
    nfold = 5,
    early_stopping_rounds = 50,
    verbose = -1
  )
  
  # Return best score (minimize RMSE)
  best_score <- min(cv_results$best_score)
  return(list(Score = -best_score))  # Negative because optimizer maximizes
}

# Updated parameter search bounds
bounds <- list(
  num_leaves = c(20L, 100L),
  max_depth = c(3L, 15L),
  learning_rate = c(0.01, 0.3)
)

# Perform Bayesian Optimization
opt_results <- BayesianOptimization(
  FUN = mean_delta_objective,
  bounds = bounds,
  init_points = 5,
  n_iter = 10,
  acq = "ucb"
)

# Extract best parameters
best_params <- list(
  objective = "regression",
  metric = "rmse",
  num_leaves = round(opt_results$Best_Par["num_leaves"]),
  max_depth = round(opt_results$Best_Par["max_depth"]),
  learning_rate = opt_results$Best_Par["learning_rate"],
  min_data_in_leaf = 30,  # Fixed value
  feature_pre_filter = FALSE,
  feature_fraction = 0.8,
  bagging_fraction = 0.8,
  bagging_freq = 1
)

# Train final model with optimized parameters
final_model <- lgb.train(
  params = best_params,
  data = lgb_train,
  nrounds = 1000,
  verbose = 0
)

# Make predictions on full dataset
pred_probs <- predict(final_model, X_full_matrix)

# Add predictions to the original dataset
pbp_2022_2024$xRV_2 <- pred_probs

# Feature Importance Plot
importance_matrix <- lgb.importance(final_model, percentage = TRUE)
importance_matrix <- importance_matrix %>%
  rename(Feature = Feature, Importance = Gain)

ggplot(data = importance_matrix, aes(x = reorder(Feature, Importance), y = Importance)) +
  geom_bar(stat = "identity", fill = "blue") +
  coord_flip() +
  labs(
    title = "Feature Importance",
    x = "Features",
    y = "Importance (Gain)"
  ) +
  theme_minimal()

leaderboard <- pbp_2022_2024 %>%
                 group_by(player_name, game_year, pitch_type) %>%
                 summarise(
                               total_pitches = n(),
                              avg_release_speed = mean(release_speed, na.rm = TRUE),
                             avg_release_pos_z = mean(release_pos_z, na.rm = TRUE),
                              avg_release_pos_x = mean(release_pos_x, na.rm = TRUE),
                               avg_pfx_x = mean(pfx_x, na.rm = TRUE),
                               avg_pfx_z = mean(pfx_z, na.rm = TRUE),
                               xRV_per_100 = sum(xRV_2, na.rm = TRUE) / total_pitches * 100
                          ) %>%
                 filter(total_pitches >= 50) %>%  # Filter pitchers with at least 50 pitches
                 mutate(
                               pitch_grade = 50 + 10 * (-xRV_per_100 - mean(-xRV_per_100, na.rm = TRUE)) / sd(-xRV_per_100, na.rm = TRUE)
                           ) %>%
                ungroup()
 rmse_v2 <- rmse(pbp_2022_2024$xRV_2, pbp_2022_2024$avg_delta_run_exp_mapped)
#===============================================================================
#Pitch Grade Model v2
#===============================================================================

# Load necessary libraries
library(dplyr)
library(lightgbm)
library(rBayesianOptimization)

# Combine specified pitch types
pbp_2022_2024 <- pbp_2022_2024 %>%
  mutate(pitch_type = case_when(
    pitch_type %in% c("CU", "KC") ~ "CU",
    pitch_type %in% c("SL", "SV") ~ "SL",
    pitch_type %in% c("SC", "CH") ~ "CH",
    pitch_type %in% c("FA", "FF") ~ "FF",
    TRUE ~ pitch_type
  ))

# Define pitch types for models
pitch_types <- c("FF", "CU", "CH", "SL", "SI", "FC", "ST", "FS")

# Function to train and predict for each pitch type
train_predict_pitch_type <- function(pitch_type) {
  # Filter data for the current pitch type
  data_pitch <- pbp_2022_2024 %>% filter(pitch_type == !!pitch_type)
  
  # Prepare features for training
  one_hot_pbp <- data_pitch %>%
    select(release_speed, release_spin_rate, spin_axis, release_pos_x, release_pos_z, pfx_z, pfx_x, avg_delta_run_exp_mapped)
  
  X_train <- one_hot_pbp %>% select(-avg_delta_run_exp_mapped)
  y_train <- one_hot_pbp$avg_delta_run_exp_mapped
  
  # Convert to matrix format
  X_train_matrix <- as.matrix(X_train)
  
  # Create LightGBM dataset for training
  lgb_train <- lgb.Dataset(data = X_train_matrix, label = y_train)
  
  # Perform Bayesian Optimization
  opt_results <- BayesianOptimization(
    FUN = function(num_leaves, max_depth, learning_rate) {
      params <- list(
        objective = "regression",
        metric = "rmse",
        num_leaves = round(num_leaves),
        max_depth = round(max_depth),
        learning_rate = learning_rate,
        min_data_in_leaf = 30,
        feature_pre_filter = FALSE,
        feature_fraction = 0.8,
        bagging_fraction = 0.8,
        bagging_freq = 1
      )
      
      cv_results <- lgb.cv(
        params = params,
        data = lgb_train,
        nrounds = 1000,
        nfold = 5,
        early_stopping_rounds = 50,
        verbose = -1
      )
      
      best_score <- min(cv_results$best_score)
      return(list(Score = -best_score))
    },
    bounds = list(
      num_leaves = c(20L, 100L),
      max_depth = c(3L, 15L),
      learning_rate = c(0.01, 0.3)
    ),
    init_points = 5,
    n_iter = 10,
    acq = "ucb"
  )
  
  # Extract best parameters
  best_params <- list(
    objective = "regression",
    metric = "rmse",
    num_leaves = round(opt_results$Best_Par["num_leaves"]),
    max_depth = round(opt_results$Best_Par["max_depth"]),
    learning_rate = opt_results$Best_Par["learning_rate"],
    min_data_in_leaf = 30,
    feature_pre_filter = FALSE,
    feature_fraction = 0.8,
    bagging_fraction = 0.8,
    bagging_freq = 1
  )
  
  # Train final model with optimized parameters
  final_model <- lgb.train(
    params = best_params,
    data = lgb_train,
    nrounds = 1000,
    verbose = 0
  )
  
  # Make predictions
  data_pitch$xRV_3 <- predict(final_model, X_train_matrix)
  
  return(data_pitch)
}

# Apply the function to each pitch type and combine results
results <- lapply(pitch_types, train_predict_pitch_type)
combined_results <- bind_rows(results)

# Add predictions back to the original dataset
pbp_2022_2024 <- pbp_2022_2024 %>%
  left_join(combined_results %>% select(row_index, xRV_3), by = "row_index")

# Calculate RMSE for the combined model
rmse_v3 <- rmse(pbp_2022_2024$xRV_3, pbp_2022_2024$avg_delta_run_exp_mapped)

# Load necessary library
library(dplyr)

# Calculate leaderboard with pitch_grade compared to the mean and sd of individual pitch types
leaderboard <- pbp_2022_2024 %>%
  group_by(player_name, game_year, pitch_type) %>%
  summarise(
    total_pitches = n(),
    avg_release_speed = mean(release_speed, na.rm = TRUE),
    avg_release_pos_z = mean(release_pos_z, na.rm = TRUE),
    avg_release_pos_x = mean(release_pos_x, na.rm = TRUE),
    avg_pfx_x = mean(pfx_x, na.rm = TRUE),
    avg_pfx_z = mean(pfx_z, na.rm = TRUE),
    xRV_per_100 = sum(xRV_3, na.rm = TRUE) / total_pitches * 100
  ) %>%
  filter(total_pitches >= 50) %>%  # Filter pitchers with at least 50 pitches
  group_by(pitch_type) %>%  # Group by pitch type for mean and sd calculations
  mutate(
    pitch_grade = 100 + 5 * (-xRV_per_100 - mean(-xRV_per_100, na.rm = TRUE)) / sd(-xRV_per_100, na.rm = TRUE)
  ) %>%
  ungroup()


# Print RMSE
print(paste("RMSE for the combined model:", rmse_v3))




tunnel_leaderboard <- pbp_2022_2024 %>%
       group_by(player_name, game_year) %>%
       summarise(
             total_pitches = n(),
             avg_pLocRatio = mean(path_to_location_ratio, na.rm = TRUE),
             pitcher_id = mean(pitcher, na.rm = TRUE)
         ) %>%
       filter(total_pitches >= 98) %>% # Filter pitchers with at least 50 pitches
       filter(game_year != 2022) %>%
       ungroup()


