# Split the one-hot encoded dataset into training and testing sets
set.seed(42)
sample_split <- sample.split(Y = pbp_2022_2024_one_hot$swing, SplitRatio = 0.7)
train_set_one_hot <- subset(x = pbp_2022_2024_one_hot, sample_split == TRUE)
test_set_one_hot <- subset(x = pbp_2022_2024_one_hot, sample_split == FALSE)

# Prepare labels
y_train <- train_set_one_hot$swing
y_test <- test_set_one_hot$swing

# Ensure data matrices are correct (excluding the label column)
X_train <- train_set_one_hot %>% select(-swing)
X_test <- test_set_one_hot %>% select(-swing)

# Create DMatrix for training and testing using the one-hot encoded features
xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# Set up parameters for XGBoost
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(unique(y_train))  # Number of classes based on the training labels
)

# Train the XGBoost model
xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 5000,
  verbose = 1
)

# Ensure the new data has the same features as the training data
trained_features <- colnames(as.matrix(X_train))

# Ensure the new_data has the same columns in the same order
new_data_one_hot <- model.matrix(~ . - 1, data = new_data)
new_data_one_hot <- new_data_one_hot[, trained_features, drop = FALSE]

# Convert the new data to an xgb.DMatrix
xgb_new_data <- xgb.DMatrix(data = as.matrix(new_data_one_hot))

# Predict on the new data using the trained model
new_data_preds <- predict(xgb_model, xgb_new_data)

# Convert predicted probabilities to class predictions (if binary classification)
new_data_class_preds <- apply(matrix(new_data_preds, ncol = 2, byrow = TRUE), 1, which.max) - 1

print(new_data_class_preds)

