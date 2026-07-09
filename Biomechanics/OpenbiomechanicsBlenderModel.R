# Install and load necessary libraries
install.packages("xgboost")
library(xgboost)

# Load your dataset into a data frame
# Replace 'your_dataset.csv' with the actual file path or URL of your dataset
df <- read.csv('your_dataset.csv')

# Assuming your dataset has features (X) and target variable (y)
# Replace c('feature_column1', 'feature_column2', 'feature_column3') with the actual column names for your features
X <- df[, c('feature_column1', 'feature_column2', 'feature_column3')]
y <- df$BaseballSavant_Run_Value

# Split the data into training and testing sets
set.seed(42)
split_index <- createDataPartition(y, p = 0.8, list = FALSE)
X_train <- X[split_index, ]
y_train <- y[split_index]
X_test <- X[-split_index, ]
y_test <- y[-split_index]

# Define the XGBoost model
model <- xgboost(data = as.matrix(X_train), label = y_train, nrounds = 10,
                 objective = 'reg:squarederror', colsample_bytree = 0.3, eta = 0.1,
                 max_depth = 5, alpha = 10)

# Make predictions on the test set
y_pred <- predict(model, as.matrix(X_test))

# Evaluate the model
mse <- mean((y_test - y_pred)^2)
print(paste('Mean Squared Error:', mse))

# Now you can use the trained model to make predictions on new data
# For example, if you have a new pitch data in a data frame called 'new_data'
# Replace 'new_data' with the actual data frame containing your new pitch data
new_predictions <- predict(model, as.matrix(new_data))
print(new_predictions)
