#===============================================================================
#BALLS IN PLAY EXIT VELOCITY
#===============================================================================

# Filter the data for the specified pitch calls
filtered_pbp_2022_2024 <- filter(pbp_2022_2024, description == "hit_into_play")

# Remove rows with NA in launch_speed
filtered_pbp_2022_2024 <- filtered_pbp_2022_2024 %>%
  filter(!is.na(launch_speed))

# Create the one-hot encoded dataset
one_hot_pbp_2022_2024 <- filtered_pbp_2022_2024 %>%
  select(pitch_type, balls, strikes, p_throws, stand, release_speed, pfx_z, pfx_x, release_pos_x, release_pos_z, release_extension, plate_x, plate_z, row_index, arm_angle, LIVAA_AA, HAA, launch_speed)

# Create another subset with the remaining columns
one_hot_pbp_2022_2024_remaining <- filtered_pbp_2022_2024 %>%
  select(-balls, -strikes, -p_throws, -stand, -release_speed, -pfx_x, -pfx_z, -release_pos_x, -release_pos_z, -release_extension, -plate_x, -plate_z, -arm_angle, -LIVAA_AA, -HAA, -launch_speed)

# Convert batter stance to 1 and 0 (R=1, L=0)
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  mutate(stand = ifelse(stand == "R", 1, 0))

# Convert pitcher throws to 1 and 0 (R=1, L=0)
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  mutate(p_throws = ifelse(p_throws == "R", 1, 0))


# Generate one-hot encoding matrix for pitch_type
pitch_type_onehot_matrix <- model.matrix(~ pitch_type - 1, one_hot_pbp_2022_2024)

# Bind the one-hot matrix back to the original data frame
one_hot_pbp_2022_2024 <- cbind(one_hot_pbp_2022_2024, pitch_type_onehot_matrix)

# Remove the original pitch_type column and prepare the data
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>% select(-pitch_type)

# Split the dataset into train and test sets
sample_split <- sample.split(Y = one_hot_pbp_2022_2024$launch_speed, SplitRatio = .25)
train_set <- subset(x = one_hot_pbp_2022_2024, sample_split == TRUE)
test_set <- subset(x = one_hot_pbp_2022_2024, sample_split == FALSE)

# Prepare the training and testing sets
y_train <- train_set$launch_speed
y_test <- test_set$launch_speed
X_train <- train_set %>% select(-launch_speed)
X_test <- test_set %>% select(-launch_speed)

# Convert to xgboost matrix
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

#xgb_grid <- expand.grid(
#nrounds = c(1000, 2000),
#eta = c(0.01, 0.1),
#max_depth = c(4, 6, 8),
#gamma = c(0, 1, 5),
#colsample_bytree = c(0.6, 0.8, 1),
#min_child_weight = c(1, 3),
#subsample = c(0.6, 0.8, 1)
#)

#train_control <- trainControl(
#method = "cv",
#number = 5,  # 5-fold cross-validation
#verboseIter = TRUE
#)

#xgb_tuned_model <- train(
#x = X_train,  # training data
#y = y_train,  # target variable
#method = "xgbTree",
#trControl = train_control,
#tuneGrid = xgb_grid,
#metric = "RMSE"
#)

# XGBoost parameters
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 6,
  gamma = 0,
  subsample = 0.6,
  colsample_bytree = 0.8,
  objective = "reg:squarederror",
  eval_metric = "rmse"  # Use logloss for binary
)

# Train the model
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 1000,
  verbose = 1
)



# Add probabilities to datasets
train_set$xEV <- predict(xgb_model, newdata = xgb_train)
test_set$xEV <- predict(xgb_model, newdata = xgb_test)

# Calculate RMSE manually to ensure it's being computed correctly
rmse_value <- sqrt(mean((y_test - test_set$xEV)^2, na.rm = TRUE))

# Print the RMSE
cat("RMSE for xEV:", rmse_value, "\n")

# Combine test and train sets
one_hot_pbp_2022_2024 <- rbind(test_set, train_set)

# Remove pitch_type one-hot columns
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  select(-starts_with("pitch_type"))



# Sort and re-arrange columns
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  arrange(row_index)

# Remove pitch_type one-hot columns
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  select(-starts_with("pitch_type"))

# Group back with remaining columns
pbp_2022_2024_in_play <- cbind(one_hot_pbp_2022_2024, one_hot_pbp_2022_2024_remaining)

pbp_2022_2024_in_play <- pbp_2022_2024_in_play[ , -13]



#===============================================================================
#BALLS IN PLAY LAUNCH ANGLE
#===============================================================================

# Filter the data for the specified pitch calls
filtered_pbp_2022_2024 <- pbp_2022_2024_in_play


# Create the one-hot encoded dataset
one_hot_pbp_2022_2024 <- filtered_pbp_2022_2024 %>%
  select(pitch_type, balls, strikes, p_throws, stand, release_speed, release_pos_x, release_pos_z, release_extension, plate_x, plate_z, row_index, arm_angle, az, ax, LIVAA_AA, HAA, launch_angle)

# Create another subset with the remaining columns
one_hot_pbp_2022_2024_remaining <- filtered_pbp_2022_2024 %>%
  select(-balls, -strikes, -p_throws, -stand, -release_speed, -release_pos_x, -release_pos_z, -release_extension, -plate_x, -plate_z, -HAA, -LIVAA_AA, -arm_angle, -az, -ax, -launch_angle)

# Convert batter stance to 1 and 0 (R=1, L=0)
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  mutate(stand = ifelse(stand == "R", 1, 0))

# Convert pitcher throws to 1 and 0 (R=1, L=0)
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  mutate(p_throws = ifelse(p_throws == "R", 1, 0))


# Generate one-hot encoding matrix for pitch_type
pitch_type_onehot_matrix <- model.matrix(~ pitch_type - 1, one_hot_pbp_2022_2024)

# Bind the one-hot matrix back to the original data frame
one_hot_pbp_2022_2024 <- cbind(one_hot_pbp_2022_2024, pitch_type_onehot_matrix)

# Remove the original pitch_type column and prepare the data
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>% select(-pitch_type)

set.seed(2242)

# Split the dataset into train and test sets
sample_split <- sample.split(Y = one_hot_pbp_2022_2024$launch_angle, SplitRatio = .25)
train_set <- subset(x = one_hot_pbp_2022_2024, sample_split == TRUE)
test_set <- subset(x = one_hot_pbp_2022_2024, sample_split == FALSE)

# Prepare the training and testing sets
y_train <- train_set$launch_angle
y_test <- test_set$launch_angle
X_train <- train_set %>% select(-launch_angle)
X_test <- test_set %>% select(-launch_angle)

# Convert to xgboost matrix
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

#xgb_grid <- expand.grid(
#nrounds = c(1000, 2000),
#eta = c(0.01, 0.1),
#max_depth = c(4, 6, 8),
#gamma = c(0, 1, 5),
#colsample_bytree = c(0.6, 0.8, 1),
#min_child_weight = c(1, 3),
#subsample = c(0.6, 0.8, 1)
#)

#train_control <- trainControl(
#method = "cv",
#number = 5,  # 5-fold cross-validation
#verboseIter = TRUE
#)

#xgb_tuned_model <- train(
#x = X_train,  # training data
#y = y_train,  # target variable
#method = "xgbTree",
#trControl = train_control,
#tuneGrid = xgb_grid,
#metric = "RMSE"
#)

# XGBoost parameters
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 4,
  gamma = 5,
  subsample = 0.8,
  colsample_bytree = 0.6,
  lambda = 0.5,
  alpha = 0.1,
  min_child_weight = 1,
  objective = "reg:squarederror",
  eval_metric = "rmse"  # Use logloss for binary
)

# Train the model
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 1000,
  verbose = 1
)





# Add probabilities to datasets
train_set$xLA <- predict(xgb_model, newdata = xgb_train)
test_set$xLA <- predict(xgb_model, newdata = xgb_test)

# Calculate RMSE manually to ensure it's being computed correctly
rmse_value <- sqrt(mean((y_test - test_set$xLA)^2, na.rm = TRUE))

# Print the RMSE
cat("RMSE for xLA:", rmse_value, "\n")

# Combine test and train sets
one_hot_pbp_2022_2024 <- rbind(test_set, train_set)

# Sort and re-arrange columns
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  arrange(row_index)

# Remove pitch_type one-hot columns
one_hot_pbp_2022_2024 <- one_hot_pbp_2022_2024 %>%
  select(-starts_with("pitch_type"))

pbp_2022_2024_in_play <- one_hot_pbp_2022_2024


# Group back with remaining columns
pbp_2022_2024_in_play <- cbind(one_hot_pbp_2022_2024, one_hot_pbp_2022_2024_remaining)

pbp_2022_2024_in_play <- pbp_2022_2024_in_play[, -11]

# Calculate residuals
pbp_2022_2024_in_play <- pbp_2022_2024_in_play %>%
  mutate(residual = launch_angle - xLA)


# Create scatter plot with regression line
ggplot(pbp_2022_2024_in_play, aes(x = launch_angle, y = xLA)) +
  geom_point(alpha = 0.5, color = "blue") +  # Scatter plot of launch angle vs. xLA
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Linear regression line
  labs(
    title = "Correlation between Actual Launch Angle and Predicted xLA",
    x = "Actual Launch Angle",
    y = "Predicted Launch Angle (xLA)"
  ) +
  theme_minimal()  # Use a clean, minimal theme
