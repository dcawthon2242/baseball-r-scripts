# Damon Cawthon xRV/100 model

# Function to modify values
fix_values <- function(x) {
  # Check if the value ends with "-"
  if (substr(x, nchar(x), nchar(x)) == "-") {
    # Remove the "-" and add it in front
    x <- paste0("-", substr(x, 1, nchar(x) - 1))
  }
  return(as.numeric(x))
}

# Apply  function to 2022 rhp hmov
pitch_model_rhp_2022$hmov <- sapply(pitch_model_rhp_2022$hmov, fix_values)
# Apply  function to 2022 lhp hmov
pitch_model_lhp_2022$hmov <- sapply(pitch_model_lhp_2022$hmov, fix_values)
# Apply  function to 2023 rhp hmov
pitch_model_rhp_2023$hmov <- sapply(pitch_model_rhp_2023$hmov, fix_values)
# Apply  function to 2023 lhp hmov
pitch_model_lhp_2023$hmov <- sapply(pitch_model_lhp_2023$hmov, fix_values)
# Combine years and handedness
pitch_model_data <- rbind(pitch_model_rhp_2023, pitch_model_lhp_2023, pitch_model_rhp_2022, pitch_model_lhp_2022)
# randomize data
set.seed(2242)
pitch_model_data <- pitch_model_data[sample(1:nrow(pitch_model_data)), ]
# change throws to numeric
pitch_model_data$throws <- ifelse(pitch_model_data$throws == "Right", 1, 0) 
# combine year and pitcher into one column for one-hot encode
pitch_model_data$yearpitcher <- paste(pitch_model_data$year, pitch_model_data$pitcher, sep = " ")
# remove redundant columns
pitch_model_data <- pitch_model_data %>%
  select(yearpitcher, throws, pitch_type, pitch_mph, IVB, hmov, vrel_point, hrel_point, extension, rv_per_hundred)

# setting universal params
params <- list(
  objective = "reg:squarederror",
  eval_metric = "rmse"
)
# FOUR-SEAM FASTBALL

# Separating model data into pitch type FF
FF_model_data <- filter(pitch_model_data, pitch_type == "FF")
# Create a vector for labels
FF_model_labels <- FF_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
FF_model_data <- FF_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
FF_pitchers <- model.matrix(~yearpitcher-1, FF_model_data)
# Combine model data and pitcher matrix
FF_model_data <- cbind(FF_pitchers, FF_model_data)
# Make sure all data is numeric
FF_model_numeric <- FF_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
FF_model_matrix <- as.matrix(FF_model_numeric)
# Training sample calculated by size of model matrix * .75 (75/25 train/test split)
# training data created
FF_train_data <- FF_model_matrix[1:FFTrainingSampleNumber, ]
# training labels created
FF_train_labels <- FF_model_labels[1:FFTrainingSampleNumber]
# test data created
FF_test_data <- FF_model_matrix[-(1:FFTrainingSampleNumber), ]
# test labels created
FF_test_labels <- FF_model_labels[-(1:FFTrainingSampleNumber)]
# insert training data/labels
FF_dtrain <- xgb.DMatrix(data = FF_train_data, label = FF_train_labels)
# insert testing data/labels
FF_dtest <- xgb.DMatrix(data = FF_test_data, label = FF_test_labels)
# FF Model
FFmodel <- xgb.train(
  max.depth = 2,
  data = FF_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = FF_dtrain, test = FF_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 2.787918

# SINKER

# Separating model data into pitch type SI
SI_model_data <- filter(pitch_model_data, pitch_type == "SI")
# Create a vector for labels
SI_model_labels <- SI_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
SI_model_data <- SI_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
SI_pitchers <- model.matrix(~yearpitcher-1, SI_model_data)
# Combine model data and pitcher matrix
SI_model_data <- cbind(SI_pitchers, SI_model_data)
# Make sure all data is numeric
SI_model_numeric <- SI_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
SI_model_matrix <- as.matrix(SI_model_numeric)
# Training sample calculated by size of model matrix * .75 (75/25 train/test split)
# training data created
SI_train_data <- SI_model_matrix[1 : SITrainingSampleNumber, ]
# training labels created
SI_train_labels <- SI_model_labels[1 : SITrainingSampleNumber]
# test data created
SI_test_data <- SI_model_matrix[-(1:SITrainingSampleNumber), ]
# test labels created
SI_test_labels <- SI_model_labels[-(1:SITrainingSampleNumber)]
# insert training data/labels
SI_dtrain <- xgb.DMatrix(data = SI_train_data, label = SI_train_labels)
# insert testing data/labels
SI_dtest <- xgb.DMatrix(data = SI_test_data, label = SI_test_labels)
# SI Model
SImodel <- xgb.train(
  gamma = 2,
  max.depth = 2,
  data = SI_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = SI_dtrain, test = SI_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 8.621095

# SPLITTER

# Separating model data into pitch type FS
FS_model_data <- filter(pitch_model_data, pitch_type == "FS")
# Create a vector for labels
FS_model_labels <- FS_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
FS_model_data <- FS_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
FS_pitchers <- model.matrix(~yearpitcher-1, FS_model_data)
# Combine model data and pitcher matrix
FS_model_data <- cbind(FS_pitchers, FS_model_data)
# Make sure all data is numeric
FS_model_numeric <- FS_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
FS_model_matrix <- as.matrix(FS_model_numeric)
# Create training sample number with 75/25 train/test split
FSTrainingSampleNumber <- round(length(FS_model_labels)* .75)
# training data created
FS_train_data <- FS_model_matrix[1 : FSTrainingSampleNumber, ]
# training labels created
FS_train_labels <- FS_model_labels[1 : FSTrainingSampleNumber]
# test data created
FS_test_data <- FS_model_matrix[-(1:FSTrainingSampleNumber), ]
# test labels created
FS_test_labels <- FS_model_labels[-(1:FSTrainingSampleNumber)]
# insert training data/labels
FS_dtrain <- xgb.DMatrix(data = FS_train_data, label = FS_train_labels)
# insert testing data/labels
FS_dtest <- xgb.DMatrix(data = FS_test_data, label = FS_test_labels)
# FS Model
FSmodel <- xgb.train(
  max.depth = 4,
  gamma = 0,
  data = FS_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = FS_dtrain, test = FS_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 5.915692

# CUTTER

# Separating model data into pitch type FC
FC_model_data <- filter(pitch_model_data, pitch_type == "FC")
# Create a vector for labels
FC_model_labels <- FC_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
FC_model_data <- FC_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
FC_pitchers <- model.matrix(~yearpitcher-1, FC_model_data)
# Combine model data and pitcher matrix
FC_model_data <- cbind(FC_pitchers, FC_model_data)
# Make sure all data is numeric
FC_model_numeric <- FC_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
FC_model_matrix <- as.matrix(FC_model_numeric)
# Create training sample number with 75/25 train/test split
FCTrainingSampleNumber <- round(length(FC_model_labels)* .75)
# training data created
FC_train_data <- FC_model_matrix[1 : FCTrainingSampleNumber, ]
# training labels created
FC_train_labels <- FC_model_labels[1 : FCTrainingSampleNumber]
# test data created
FC_test_data <- FC_model_matrix[-(1:FCTrainingSampleNumber), ]
# test labels created
FC_test_labels <- FC_model_labels[-(1:FCTrainingSampleNumber)]
# insert training data/labels
FC_dtrain <- xgb.DMatrix(data = FC_train_data, label = FC_train_labels)
# insert testing data/labels
FC_dtest <- xgb.DMatrix(data = FC_test_data, label = FC_test_labels)
# FC Model
FCmodel <- xgb.train(
  gamma = 2,
  data = FC_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = FC_dtrain, test = FC_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 3.900546

# SLIDER

# Separating model data into pitch type SL
SL_model_data <- filter(pitch_model_data, pitch_type == "SL")
# Create a vector for labels
SL_model_labels <- SL_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
SL_model_data <- SL_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
SL_pitchers <- model.matrix(~yearpitcher-1, SL_model_data)
# Combine model data and pitcher matrix
SL_model_data <- cbind(SL_pitchers, SL_model_data)
# Make sure all data is numeric
SL_model_numeric <- SL_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
SL_model_matrix <- as.matrix(SL_model_numeric)
# Create training sample number with 75/25 train/test split
SLTrainingSampleNumber <- round(length(SL_model_labels)* .75)
# training data created
SL_train_data <- SL_model_matrix[1 : SLTrainingSampleNumber, ]
# training labels created
SL_train_labels <- SL_model_labels[1 : SLTrainingSampleNumber]
# test data created
SL_test_data <- SL_model_matrix[-(1:SLTrainingSampleNumber), ]
# test labels created
SL_test_labels <- SL_model_labels[-(1:SLTrainingSampleNumber)]
# insert training data/labels
SL_dtrain <- xgb.DMatrix(data = SL_train_data, label = SL_train_labels)
# insert testing data/labels
SL_dtest <- xgb.DMatrix(data = SL_test_data, label = SL_test_labels)
# SL Model
SLmodel <- xgb.train(
  max.depth = 2,
  gamma = 0,
  data = SL_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = SL_dtrain, test = SL_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 3.693106

# SWEEPER

# Separating model data into pitch type SW
SW_model_data <- filter(pitch_model_data, pitch_type == "SW")
# Create a vector for labels
SW_model_labels <- SW_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
SW_model_data <- SW_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
SW_pitchers <- model.matrix(~yearpitcher-1, SW_model_data)
# Combine model data and pitcher matrix
SW_model_data <- cbind(SW_pitchers, SW_model_data)
# Make sure all data is numeric
SW_model_numeric <- SW_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
SW_model_matrix <- as.matrix(SW_model_numeric)
# Create training sample number with 75/25 train/test split
SWTrainingSampleNumber <- round(length(SW_model_labels)* .75)
# training data created
SW_train_data <- SW_model_matrix[1 : SWTrainingSampleNumber, ]
# training labels created
SW_train_labels <- SW_model_labels[1 : SWTrainingSampleNumber]
# test data created
SW_test_data <- SW_model_matrix[-(1:SWTrainingSampleNumber), ]
# test labels created
SW_test_labels <- SW_model_labels[-(1:SWTrainingSampleNumber)]
# insert training data/labels
SW_dtrain <- xgb.DMatrix(data = SW_train_data, label = SW_train_labels)
# insert testing data/labels
SW_dtest <- xgb.DMatrix(data = SW_test_data, label = SW_test_labels)
# SW Model
SWmodel <- xgb.train(
  max.depth = 4,
  gamma = 1.8,
  data = SW_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = SW_dtrain, test = SW_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 2.782963

# CURVEBALL

# Separating model data into pitch type CU
CU_model_data <- filter(pitch_model_data, pitch_type == "CU")
# Create a vector for labels
CU_model_labels <- CU_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
CU_model_data <- CU_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
CU_pitchers <- model.matrix(~yearpitcher-1, CU_model_data)
# Combine model data and pitcher matrix
CU_model_data <- cbind(CU_pitchers, CU_model_data)
# Make sure all data is numeric
CU_model_numeric <- CU_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
CU_model_matrix <- as.matrix(CU_model_numeric)
# Create training sample number with 75/25 train/test split
CUTrainingSampleNumber <- round(length(CU_model_labels)* .75)
# training data created
CU_train_data <- CU_model_matrix[1 : CUTrainingSampleNumber, ]
# training labels created
CU_train_labels <- CU_model_labels[1 : CUTrainingSampleNumber]
# test data created
CU_test_data <- CU_model_matrix[-(1:CUTrainingSampleNumber), ]
# test labels created
CU_test_labels <- CU_model_labels[-(1:CUTrainingSampleNumber)]
# insert training data/labels
CU_dtrain <- xgb.DMatrix(data = CU_train_data, label = CU_train_labels)
# insert testing data/labels
CU_dtest <- xgb.DMatrix(data = CU_test_data, label = CU_test_labels)
# CU Model
CUmodel <- xgb.train(
  max.depth = 5,
  gamma = 0,
  data = CU_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = CU_dtrain, test = CU_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 3.184252

# CHANGEUP

# Separating model data into pitch type CU
CH_model_data <- filter(pitch_model_data, pitch_type == "CH")
# Create a vector for labels
CH_model_labels <- CH_model_data %>%
  pull(rv_per_hundred)
# Remove redundant info
CH_model_data <- CH_model_data %>%
  select(-rv_per_hundred, -pitch_type)
# Create a one-hot matrix for pitchers and years
CH_pitchers <- model.matrix(~yearpitcher-1, CH_model_data)
# Combine model data and pitcher matrix
CH_model_data <- cbind(CH_pitchers, CH_model_data)
# Make sure all data is numeric
CH_model_numeric <- CH_model_data %>%
  select_if(is.numeric)
# Finalize model matrix
CH_model_matrix <- as.matrix(CH_model_numeric)
# Training sample calculated by size of model matrix * .75 (75/25 train/test split)
# training data created
CH_train_data <- CH_model_matrix[1 : CHTrainingSampleNumber, ]
# training labels created
CH_train_labels <- CH_model_labels[1 : CHTrainingSampleNumber]
# test data created
CH_test_data <- CH_model_matrix[-(1:CHTrainingSampleNumber), ]
# test labels created
CH_test_labels <- CH_model_labels[-(1:CHTrainingSampleNumber)]
# insert training data/labels
CH_dtrain <- xgb.DMatrix(data = CH_train_data, label = CH_train_labels)
# insert testing data/labels
CH_dtest <- xgb.DMatrix(data = CH_test_data, label = CH_test_labels)
# CH Model
CHmodel <- xgb.train(
  max.depth = 3,
  gamma = 0,
  data = CH_dtrain,
  params = params,
  nrounds = 100, 
  watchlist = list(train = CH_dtrain, test = CH_dtest),
  early_stopping_rounds = 10
)
# Test RMSE: 6.165607

# Fastball prediction table

# Finding Fastball Predicted Values
FF_pred = predict(FFmodel, FF_model_matrix)
FF_data__with_predictions <- as.data.frame(FF_model_matrix)
FF_data <- filter(pitch_model_data, pitch_type == "FF")
# Adding predicted values to old table
FF_data$predictions <- FF_pred

# Finding Sinker Predicted Values
SI_pred = predict(SImodel, SI_model_matrix)
SI_data__with_predictions <- as.data.frame(SI_model_matrix)
SI_data <- filter(pitch_model_data, pitch_type == "SI")
# Adding predicted values to old table
SI_data$predictions <- SI_pred

# Finding Splitter Predicted Values
FS_pred = predict(FSmodel, FS_model_matrix)
FS_data__with_predictions <- as.data.frame(FS_model_matrix)
FS_data <- filter(pitch_model_data, pitch_type == "FS")
# Adding predicted values to old table
FS_data$predictions <- FS_pred

# Finding Cutter Predicted Values
FC_pred = predict(FCmodel, FC_model_matrix)
FC_data__with_predictions <- as.data.frame(FC_model_matrix)
FC_data <- filter(pitch_model_data, pitch_type == "FC")
# Adding predicted values to old table
FC_data$predictions <- FC_pred

# Finding Slider Predicted Values
SL_pred = predict(SLmodel, SL_model_matrix)
SL_data__with_predictions <- as.data.frame(SL_model_matrix)
SL_data <- filter(pitch_model_data, pitch_type == "SL")
# Adding predicted values to old table
SL_data$predictions <- SL_pred

# Finding Sweeper Predicted Values
SW_pred = predict(SWmodel, SW_model_matrix)
SW_data__with_predictions <- as.data.frame(SW_model_matrix)
SW_data <- filter(pitch_model_data, pitch_type == "SW")
# Adding predicted values to old table
SW_data$predictions <- SW_pred

# Finding Curveball Predicted Values
CU_pred = predict(CUmodel, CU_model_matrix)
CU_data__with_predictions <- as.data.frame(CU_model_matrix)
CU_data <- filter(pitch_model_data, pitch_type == "CU")
# Adding predicted values to old table
CU_data$predictions <- CU_pred

# Finding ChangeUp Predicted Values
CH_pred = predict(CHmodel, CH_model_matrix)
CH_data__with_predictions <- as.data.frame(CH_model_matrix)
CH_data <- filter(pitch_model_data, pitch_type == "CH")
# Adding predicted values to old table
CH_data$predictions <- CH_pred





# DATASET CHECK
# Check for missing values
#if (any(is.na(FF_test_data)) | any(is.na(FF_test_labels))) {
  #stop("Data contains missing values. Please handle them before creating xgb.DMatrix.")}

# Check all values are numeric
#if (!all(sapply(FF_test_data, is.numeric)) | !is.numeric(FF_test_labels)) {
  #stop("Data types are not numeric. Convert the data to numeric before creating xgb.DMatrix.")}
# Check for infinite values
#if (any(is.infinite(FF_test_data)) | any(is.infinite(FF_test_labels))) {
  #stop("Data contains infinite values. Please handle them before creating xgb.DMatrix.")}