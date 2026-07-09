D1TM_Combined <- D1TM_Combined %>%
  mutate(is_called_strike = ifelse(PitchCall == "StrikeCalled", 1, 0))


library(lightgbm)
library(dplyr)

# Step 1: Create binary target
D1TM_Combined <- D1TM_Combined %>%
  mutate(is_called_strike = ifelse(PitchCall == "StrikeCalled", 1, 0))

# Step 2: Filter out rows with missing relevant values
model_data <- D1TM_Combined %>%
  filter(!is.na(BatterSide), !is.na(PlateLocHeight), !is.na(PlateLocSide), !is.na(is_called_strike))

# Step 3: Encode BatterSide
model_data <- model_data %>%
  mutate(BatterSideEncoded = as.integer(as.factor(BatterSide)))

# Step 4: Prepare data
features <- c("BatterSideEncoded", "PlateLocHeight", "PlateLocSide")
label <- model_data$is_called_strike
dtrain <- lgb.Dataset(data = as.matrix(model_data[, features]), label = label)

# Step 5: Train model
params <- list(
  objective = "binary",
  metric = "binary_logloss",
  learning_rate = 0.05,
  num_leaves = 31
)

model <- lgb.train(
  params = params,
  data = dtrain,
  nrounds = 100
)

# Step 6: Prepare full dataset for prediction
D1TM_Combined <- D1TM_Combined %>%
  mutate(
    BatterSideEncoded = as.integer(as.factor(BatterSide)),
    PlateLocHeight = as.numeric(PlateLocHeight),
    PlateLocSide = as.numeric(PlateLocSide)
  )

# Step 7: Predict xCalledStrike
predict_rows <- which(!is.na(D1TM_Combined$BatterSideEncoded) & !is.na(D1TM_Combined$PlateLocHeight) & !is.na(D1TM_Combined$PlateLocSide))
predict_matrix <- as.matrix(D1TM_Combined[predict_rows, c("BatterSideEncoded", "PlateLocHeight", "PlateLocSide")])
D1TM_Combined$xCalledStrike <- NA
D1TM_Combined$xCalledStrike[predict_rows] <- predict(model, predict_matrix)

D1TM_Combined <- D1TM_Combined %>%
  mutate(
    distance_from_center = sqrt((PlateLocSide)^2 + (PlateLocHeight - 2.55)^2),
    LocationClassification = case_when(
      !is.na(distance_from_center) & distance_from_center <= 0.42 ~ "Meatball",
      xCalledStrike > 0.05 ~ "Edge-Expand",
      xCalledStrike <= 0.05 ~ "Waste",
      TRUE ~ NA_character_
    )
  ) %>%
  select(-distance_from_center)  # Optional: remove helper column


