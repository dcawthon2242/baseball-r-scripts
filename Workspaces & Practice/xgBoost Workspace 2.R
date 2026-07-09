library(xgboost)
library(tidyverse)

colic <- horse

# set a random seed & shuffle data frame
set.seed(1234)
colic <- colic[sample(1:nrow(colic)), ]
# print the first few rows of our dataframe
head(colic)
colic$lived <- colic$outcome == "lived"
# get the subset of the dataframe that doesn't have labels about the target info
colic_OutcomeRemoved <- colic %>%
  select(-outcome)
# get lived labels

# convert "Age" to a numeric variable using one-hot encoding
# select only numeric columns & remove rows with NA values
colic_numeric <- colic_OutcomeRemoved %>%
  select(age, rectal_temp, pulse, respiratory_rate, packed_cell_volume, total_protein, abdomo_protein,
         lesion_1, lesion_2, lesion_3, lived) %>% # select only numeric columns
  # and sparsely tracked info
  na.omit() # remove na values
age <- model.matrix(~age-1, colic_numeric)
colic_labels <- colic_numeric %>%
  select(lived) %>%
  is.na() %>%
  magrittr::not()
colic_numeric <- colic_numeric %>%
  select(-age, -lived)

colic_numeric <- cbind(age, colic_numeric)

colic_matrix <- data.matrix(colic_numeric)

colicNumberofTrainingSamples <- round(length(colic_labels) * .8)

colictrain_data <- colic_matrix[1:colicNumberofTrainingSamples, ]
colictrain_labels <- colic_matrix[1:colicNumberofTrainingSamples]

colictest_data <- colic_matrix[1:colicNumberofTrainingSamples, ]
colictest_labels <- colic_matrix[1:colicNumberofTrainingSamples]

colicdtrain <- xgb.DMatrix(data = colictrain_data, label = colictrain_labels)
colicdtest <- xgb.DMatrix(data = colictest_data, label = colictest_labels)

model <- xgboost(data = colicdtrain, nround = 2, objective = "binary:logistic")

pred <- predict(model, colicdtest)
err <- mean(as.numeric(pred < 0.5) != colictest_labels)
print(paste("test-error = ", err))









