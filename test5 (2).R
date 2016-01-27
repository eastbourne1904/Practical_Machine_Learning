# http://rzarev.github.io/pml_project/writeup.html
# could not get to work

if (!file.exists("pml-training.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv") 
} 

if (!file.exists("pml-testing.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv") } 

library(ggplot2)
library(scales)
library(dplyr)
library(caret)
library(e1071)
library(randomForest)
library(lubridate)

all_data <-
  read.csv('pml-training.csv', row.names = 1, stringsAsFactors = FALSE,
           na.strings = c("NA", "", "#DIV/0!")) %>%
  mutate(cvtd_timestamp = mdy_hm(cvtd_timestamp),
         user_name      = as.factor(user_name),
         new_window     = as.factor(new_window),
         classe         = as.factor(classe))

testing_data <-
  read.csv('pml-testing.csv', row.names = 1, stringsAsFactors = FALSE,
           na.strings = c("NA", "", "#DIV/0!")) %>%
  mutate(cvtd_timestamp = mdy_hm(cvtd_timestamp),
         user_name      = as.factor(user_name),
         new_window     = as.factor(new_window))

train_percentage <- .6

set.seed(3251)
train_index <- createDataPartition(y = all_data$classe,
                                   p = train_percentage,
                                   list = FALSE)
train_set      <- all_data[train_index, ]
validation_set <- all_data[-train_index, ]

set.seed(1837)

accuracy_rf <- numeric(5)
for (j in 1:5) {
  # Splits into 10 folds.
  folds <- createFolds(y = train_set$classe, k = 10, list = FALSE)
  correct <- logical(nrow(train_set))
  for (i in 1:10) {
    # Split into training and validation sets
    # =======================================
    train_subset <- train_set[folds != i, ]
    test_subset  <- train_set[folds == i, ]
    
    # Pre-processing
    # ==============
    
    # Remove features with missing values
    missing <- is.na(train_subset)
    keep_columns <- names(which(colSums(missing) == 0))
    train_subset <- train_subset[, keep_columns]
    test_subset  <- test_subset[, keep_columns]
    
    # Fit the model
    # =============
    model <- randomForest(classe ~ ., data = train_subset)
    
    # Record which guesses are correct
    # ================================
    predictions <- predict(model, newdata = test_subset)
    correct[folds == i] <- (predictions == test_subset$classe)
  }
  # Accuracy for the j-th iteration
  accuracy_rf[j] <- mean(correct)
}

set.seed(23756)

# Remove missing variables
missing              <- is.na(train_set)
keep_columns         <- names(which(colSums(missing) == 0))
train_processed      <- train_set[, keep_columns]
validation_processed <- validation_set[, keep_columns]

# Fit the model
# =============
model <- randomForest(classe ~ ., data = train_processed)

# Record which guesses are correct
# ================================
predictions <- predict(model, newdata = validation_processed)
cm <- confusionMatrix(data = predictions, reference = validation_processed$classe)