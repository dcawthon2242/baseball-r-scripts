trajectory_one_hot <- pbp_2024

# Define the parameters for the quadratic equation
trajectory_one_hot <- trajectory_one_hot %>%
  mutate(
    a = 0.5 * ay,  # Coefficient for t^2
    b = vy0,       # Coefficient for t
    c = release_extension - 60.5,  # Constant term
    
    # Calculate the discriminant
    discriminant = b^2 - 4 * a * c,
    
    # Calculate time_of_flight using the quadratic formula
    time_of_flight = ifelse(discriminant >= 0,
                            (-b - sqrt(discriminant)) / (2 * a) * -1, 
                            NA)
  )

# Filter the dataset to keep only rows with odd row_index values
trajectory_one_hot <- trajectory_one_hot %>%
  mutate(row_index = row_number()) %>%
  filter(row_index %% 2 != 0) %>%
  select(-row_index)

trajectory_one_hot <- trajectory_one_hot %>%
  mutate(reaction_zone = time_of_flight - .175) %>%
  arrange(game_pk, at_bat_number, pitch_number)

#Remove eephuses, pitchouts, etc.
trajectory_one_hot <- trajectory_one_hot[!trajectory_one_hot$pitch_type %in% c("EP", "NA", "PO", "CS", NA), ]
#Remove NA columns
trajectory_one_hot <- trajectory_one_hot %>% select(-spin_dir, -break_length_deprecated, -tfs_deprecated, -tfs_zulu_deprecated, -umpire, -sv_id)
#Remove NA values in remaining columns
trajectory_one_hot <- trajectory_one_hot %>% filter(!is.na(release_pos_x), !is.na(release_pos_z), !is.na(player_name), !is.na(batter), !is.na(pitcher), !is.na(description), !is.na(zone), !is.na(des), !is.na(game_type), !is.na(stand), !is.na(p_throws), !is.na(home_team), !is.na(away_team), !is.na(type), !is.na(balls), !is.na(strikes), !is.na(game_year), !is.na(pfx_x), !is.na(pfx_z), !is.na(plate_x), !is.na(plate_z), !is.na(outs_when_up), !is.na(inning), !is.na(inning_topbot), !is.na(fielder_2), !is.na(vx0), !is.na(vy0), !is.na(vz0), !is.na(ax), !is.na(ay), !is.na(az), !is.na(sz_top), !is.na(sz_bot), !is.na(effective_speed), !is.na(release_spin_rate), !is.na(release_extension), !is.na(game_pk), !is.na(pitcher_1), !is.na(fielder_2_1), !is.na(fielder_3), !is.na(fielder_4), !is.na(fielder_5), !is.na(fielder_6), !is.na(fielder_7), !is.na(fielder_8), !is.na(fielder_9), !is.na(release_pos_y), !is.na(at_bat_number), !is.na(pitch_number), !is.na(pitch_number), !is.na(pitch_number), !is.na(home_score), !is.na(away_score), !is.na(bat_score), !is.na(fld_score), !is.na(post_away_score), !is.na(post_home_score), !is.na(post_bat_score), !is.na(post_fld_score), !is.na(if_fielding_alignment), !is.na(of_fielding_alignment), !is.na(spin_axis), !is.na(delta_home_win_exp), !is.na(delta_run_exp), !is.na(a), !is.na(b), !is.na(c), !is.na(discriminant), !is.na(time_of_flight), !is.na(reaction_zone)) 

trajectory_one_hot <- tail(trajectory_one_hot, n = 25000)

#Convert pfx_x to inches
trajectory_one_hot$pfx_x <- trajectory_one_hot$pfx_x *12

#Convert pfx_x to inches
trajectory_one_hot$pfx_z <- trajectory_one_hot$pfx_z *12

#Create row_index column
trajectory_one_hot$row_index <- 1:nrow(trajectory_one_hot)



filtered_trajectory_one_hot <- trajectory_one_hot %>% select(p_throws, pitch_type, release_speed, release_pos_x, release_pos_z, plate_x, plate_z, az)

#Make numbers to remerge model with
trajectory_one_hot_remaining <- trajectory_one_hot %>% select(-p_throws, -release_speed, -pfx_x, -pfx_z, -release_pos_x, -release_pos_z, -release_extension, -plate_x, -plate_z)

#Convert pitcher throws to 1 and 0 (R=1, L=0)
filtered_trajectory_one_hot <- filtered_trajectory_one_hot %>%
  mutate(p_throws = ifelse(p_throws == "R", 1, 0))

# Generate one-hot encoding matrix for pitch_type
pitch_type_onehot_matrix <- model.matrix(~ pitch_type - 1, filtered_trajectory_one_hot)

# Optionally, bind the one-hot matrix back to the original data frame
filtered_trajectory_one_hot <- cbind(filtered_trajectory_one_hot, pitch_type_onehot_matrix)

filtered_trajectory_one_hot <- filtered_trajectory_one_hot %>% select(-pitch_type)

sample_split <- sample.split(Y = filtered_trajectory_one_hot$az, SplitRatio = .25)
train_set <- subset(x = filtered_trajectory_one_hot, sample_split == TRUE)
test_set <- subset(x = filtered_trajectory_one_hot, sample_split == FALSE)

# Create factor levels based on the entire dataset (train + test combined)
az_levels <- levels(as.factor(filtered_trajectory_one_hot$az))

# Convert az to factor based on the levels from the entire dataset
y_train <- as.integer(factor(train_set$az, levels = az_levels)) - 1
y_test <- as.integer(factor(test_set$az, levels = az_levels)) - 1
X_train <- train_set %>% select(-az)
X_test <- test_set %>% select(-az)
# Recalculate the number of unique classes
num_class <- length(az_levels)

# Proceed with the model creation
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

xgb_params <- list(
  booster = "gbtree",
  eta = 0.1,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = num_class
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 100,
  verbose = 1
)
xgb_model
# 1. Make predictions (predict probabilities for each class)
# Make predictions on the test set
# Reshape the prediction matrix to have num_class columns and nrow = number of test samples
pred_matrix <- matrix(preds, ncol = num_class, byrow = TRUE)

# Extract the predicted class (highest probability for each row)
pred_labels <- max.col(pred_matrix) - 1  # Subtract 1 to align with label indexing

# Calculate RMSE
rmse <- sqrt(mean((y_test - pred_labels)^2))
print(paste("RMSE:", rmse))


# Calculate log loss
library(Metrics)
log_loss <- logLoss(y_test, pred_matrix)
print(paste("Log Loss:", log_loss))









