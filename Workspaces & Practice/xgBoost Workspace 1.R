library(xgboost)
diseaseInfo <- Outbreak_240817

cols(
  .default = col_character(),
  Id = col_double(),
  latitude = col_double(),
  longitude = col_double(),
  sumAtRisk = col_double(),
  sumCases = col_double(),
  sumDeaths = col_double(),
  sumDestroyed = col_double(),
  sumSlaughtered = col_double(),
  humansAge = col_double(),
  humansAffected = col_double(),
  humansDeaths = col_double()
)
# set a random seed & shuffle data frame
set.seed(1234)
diseaseInfo <- diseaseInfo[sample(1:nrow(diseaseInfo)), ]
# print the first few rows of our dataframe
head(diseaseInfo)
# get the subset of the data frame that doesn't have labels about humans affected with disease
diseaseInfo_humansRemoved <- diseaseInfo %>% 
  select(-starts_with("human"))
# get a boolean vector of training labels
diseaseLabels <- diseaseInfo %>%
  select(humansAffected) %>% # get the column with the number of humans affected
  # switch TRUE and FALSE (using function from magrittr package)
  is.na() %>%
  magrittr::not()
# check out the first few lines
head(diseaseLabels) # of our target variable
head(diseaseInfo$humansAffected) # of the original column
# select just the numeric columns
diseaseInfo_numeric <- diseaseInfo_humansRemoved %>%
  select(-Id) %>% # the case id shouldn't contain useful information
  select(-c(longitude, latitude)) %>% # location data is also in country data
  select_if(is.numeric) # select remaining numeric columns
# make sure that our dataframe is all numeric
str(diseaseInfo_numeric)
# check out the first few rows of the country column
head(diseaseInfo$country)
# one-hot matrix for just the first few rows of the "country" column
model.matrix(~country-1, head(diseaseInfo))
# convert categorical factor into one-hot encoding
region <- model.matrix(~country-1, diseaseInfo)
# some of the species
head(diseaseInfo$speciesDescription)
# add a boolean column to our numeric dataframe
# indicating whether a species is domestic
diseaseInfo_numeric$is_domestic <- str_detect(
  diseaseInfo$speciesDescription, "domestic"
)
# get a list of all the species by getting the last
speciesList <- diseaseInfo$speciesDescription %>%
  str_replace("[[:punct:]]", "") %>% # remove punctuation (some rows have parenthesis)
  str_extract("[a-z]*$") # extract the last word in each row
# convert our list into a dataframe...
speciesList <- tibble(species = speciesList)
# and convert to a matrix using 1 hot encoding
options(na.action = 'na.pass') # don't drop NA values!
species <- model.matrix(~species-1, speciesList)
# add our one-hot encoded variable and convert the dataframe into a matrix
diseaseInfo_numeric <- cbind(diseaseInfo_numeric,
                             region, species)
diseaseInfo_matrix <- data.matrix(diseaseInfo_numeric)
# get the numb 70/30 training test split
numberOfTrainingSamples <- round(length(diseaseLabels) * .7)
# training data
train_data <- diseaseInfo_matrix[1:numberOfTrainingSamples, ]
train_labels <- diseaseLabels[1:numberOfTrainingSamples]
# testing data
test_data <- diseaseInfo_matrix[-(1:numberOfTrainingSamples), ]
test_labels <- diseaseLabels[-(1:numberOfTrainingSamples)]
# put our testing and training data into two separate DMatrixs objects
dtrain <- xgb.DMatrix(data = train_data, label = train_labels)
dtest <- xgb.DMatrix(data = test_data, label = test_labels)

#train a model using our training data
model <- xgboost(data = dtrain, # the data
                 nround = 2, # max number of boosting iterations
                 objective = "binary:logistic") # the objective function
# generate predictions for our held-out testing data
pred <- predict(model, dtest)
# get and print the classification error
err <- mean(as.numeric(pred > 0.5) != test_labels)
print(paste("test-error=", err))







































