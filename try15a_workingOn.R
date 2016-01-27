#library(AppliedPredictiveModeling)
library(caret)
library(rattle)
library(rpart.plot)
library(randomForest)

# Download data.
url_raw_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
file_dest_training <- "pml-training.csv"
#download.file(url=url_raw_training, destfile=file_dest_training, method="curl")
url_raw_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
file_dest_testing <- "pml-testing.csv"
#download.file(url=url_raw_testing, destfile=file_dest_testing, method="curl")

# Import the data treating empty values as NA.
pmlTrainingData <- read.csv(file_dest_training, na.strings=c("NA",""), header=TRUE)
pmlTrainingData.columnNames <- colnames(pmlTrainingData)
pmlTestingData <- read.csv(file_dest_testing, na.strings=c("NA",""), header=TRUE)
pmlTestingData.columnNames <- colnames(pmlTestingData)

# Verify that the column names (excluding classe and problem_id) are identical in the training and test set.
all.equal(pmlTrainingData.columnNames[1:length(pmlTrainingData.columnNames)-1], pmlTestingData.columnNames[1:length(pmlTrainingData.columnNames)-1])

# Count the number of non-NAs in each col.
nonNAs <- function(x) {
  as.vector(apply(x, 2, function(x) length(which(!is.na(x)))))
}

# Build vector of missing data or NA columns to drop.
columnCount <- nonNAs(pmlTrainingData)
dropColumns <- c()
for (cnt in 1:length(columnCount)) {
  if (columnCount[cnt] < nrow(pmlTrainingData)) {
    dropColumns <- c(dropColumns, pmlTrainingData.columnNames[cnt])
  }
}

# Drop NA data and the first 7 columns as they're unnecessary for predicting.
pmlTrainingData <- pmlTrainingData[,!(names(pmlTrainingData) %in% dropColumns)]
pmlTrainingData <- pmlTrainingData[,8:length(colnames(pmlTrainingData))]

pmlTestingData <- pmlTestingData[,!(names(pmlTestingData) %in% dropColumns)]
pmlTestingData <- pmlTestingData[,8:length(colnames(pmlTestingData))]

# Show remaining columns.
colnames(pmlTrainingData)
colnames(pmlTestingData)

nzv <- nearZeroVar(pmlTrainingData, saveMetrics=TRUE)
nzv

# Divide the given training set into 4 roughly equal sets.
#set.seed(1979)
#partition.id <- createDataPartition(y=df_training$classe, p=0.25, list=FALSE)
#partition.df1 <- df_training[partition.id,]
#df_remainder <- df_training[-partition.id,]

#set.seed(1979)
#partition.id <- createDataPartition(y=df_remainder$classe, p=0.33, list=FALSE)
#partition.df2 <- df_remainder[partition.id,]
#df_remainder <- df_remainder[-partition.id,]

#set.seed(1979)
#partition.id <- createDataPartition(y=df_remainder$classe, p=0.5, list=FALSE)
#partition.df3 <- df_remainder[partition.id,]

#partition.df4 <- df_remainder[-partition.id,]

#-----------------------------------------------------------------------------------------

# Divide each of these 4 sets into training (60%) and test (40%) sets.
set.seed(1979)
inTrain <- createDataPartition(y=pmlTrainingData$classe, p=0.6, list=FALSE)
partition.df_training <- pmlTrainingData[inTrain,]
partition.df_testing <- pmlTrainingData[-inTrain,]

#set.seed(1979)
#inTrain <- createDataPartition(y=partition.df1$classe, p=0.6, list=FALSE)
#partition.df_training1 <- partition.df1[inTrain,]
#partition.df_testing1 <- partition.df1[-inTrain,]

#set.seed(1979)
#inTrain <- createDataPartition(y=partition.df2$classe, p=0.6, list=FALSE)
#partition.df_training2 <- partition.df2[inTrain,]
#partition.df_testing2 <- partition.df2[-inTrain,]

#set.seed(1979)
#inTrain <- createDataPartition(y=partition.df3$classe, p=0.6, list=FALSE)
#partition.df_training3 <- partition.df3[inTrain,]
#partition.df_testing3 <- partition.df3[-inTrain,]

#set.seed(1979)
#inTrain <- createDataPartition(y=partition.df4$classe, p=0.6, list=FALSE)
#partition.df_training4 <- partition.df4[inTrain,]
#partition.df_testing4 <- partition.df4[-inTrain,]

#-----------------------------------------------------------------------------------------

# Train on training set 1 of 4 with no extra features.
set.seed(1979)
testModel <- train(partition.df_training$classe ~ ., data = partition.df_training, method="rpart")
print(testModel, digits=3)

print(testModel$finalModel, digits=3)

fancyRpartPlot(testModel$finalModel)

#set.seed(1979)
#testModel <- train(partition.df_training1$classe ~ ., data = partition.df_training1, method="rpart")
#print(testModel, digits=3)

#print(testModel$finalModel, digits=3)

#fancyRpartPlot(testModel$finalModel)

#-----------------------------------------------------------------------------------------

# Run against testing set 1 of 4 with no extra features.
#predictions <- predict(testModel, newdata=partition.df_testing1)
#print(confusionMatrix(predictions, partition.df_testing1$classe), digits=4)
predictions <- predict(testModel, newdata=partition.df_testing)
print(confusionMatrix(predictions, partition.df_testing$classe), digits=4)

# Train on training set 1 of 4 with only preprocessing.
#set.seed(1979)
#testModel <- train(partition.df_training1$classe ~ .,  preProcess=c("center", "scale"), data = partition.df_training1, method="rpart")
#print(testModel, digits=3)
set.seed(1979)
testModel <- train(partition.df_training$classe ~ .,  preProcess=c("center", "scale"), data = partition.df_training, method="rpart")
print(testModel, digits=3)

# Train on training set 1 of 4 with only cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training1$classe ~ .,  trControl=trainControl(method = "cv", number = 4), data = partition.df_training1, method="rpart")
#print(testModel, digits=3)
set.seed(1979)
testModel <- train(partition.df_training$classe ~ .,  trControl=trainControl(method = "cv", number = 4), data = partition.df_training, method="rpart")
print(testModel, digits=3)

# Train on training set 1 of 4 with both preprocessing and cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training1$classe ~ .,  preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data = partition.df_training1, method="rpart")
#print(testModel, digits=3)
set.seed(1979)
testModel <- train(partition.df_training$classe ~ .,  preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data = partition.df_training, method="rpart")
print(testModel, digits=3)

# Run against testing set 1 of 4 with both preprocessing and cross validation.
#predictions <- predict(testModel, newdata=partition.df_testing1)
#print(confusionMatrix(predictions, partition.df_testing1$classe), digits=4)
predictions <- predict(testModel, newdata=partition.df_testing)
print(confusionMatrix(predictions, partition.df_testing$classe), digits=4)

# Train on training set 1 of 4 with only cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training1$classe ~ ., method="rf", trControl=trainControl(method = "cv", number = 4), data=partition.df_training1)
#print(testModel, digits=3)
set.seed(1979)
testModel <- train(partition.df_training1$classe ~ ., method="rf", trControl=trainControl(method = "cv", number = 4), data=partition.df_training1)
print(testModel, digits=3)

# Run against testing set 1 of 4.
#predictions <- predict(testModel, newdata=partition.df_testing1)
#print(confusionMatrix(predictions, partition.df_testing1$classe), digits=4)
predictions <- predict(testModel, newdata=partition.df_testing)
print(confusionMatrix(predictions, partition.df_testing$classe), digits=4)

# Run against 20 testing set provided by Professor Leek.
print(predict(testModel, newdata=pmlTestingData))

# Train on training set 1 of 4 with only both preprocessing and cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training1$classe ~ ., method="rf", preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data=partition.df_training1)
#print(testModel, digits=3)
set.seed(1979)
testModel <- train(partition.df_training$classe ~ ., method="rf", preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data=partition.df_training)
print(testModel, digits=3)

# Run against testing set 1 of 4.
#predictions <- predict(testModel, newdata=partition.df_testing1)
#print(confusionMatrix(predictions, partition.df_testing1$classe), digits=4)
predictions <- predict(testModel, newdata=partition.df_testing)
print(confusionMatrix(predictions, partition.df_testing$classe), digits=4)

# Run against 20 testing set provided by Professor Leek.
print(predict(testModel, newdata=pmlTestingData))

# Train on training set 2 of 4 with only cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training2$classe ~ ., method="rf", preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data=partition.df_training2)
#print(testModel, digits=3)

# Run against testing set 2 of 4.
#predictions <- predict(testModel, newdata=partition.df_testing2)
#print(confusionMatrix(predictions, partition.df_testing2$classe), digits=4)

# Run against 20 testing set provided by Professor Leek.
#print(predict(testModel, newdata=pmlTestingData))

# Train on training set 3 of 4 with only cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training3$classe ~ ., method="rf", preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data=partition.df_training3)
#print(testModel, digits=3)

# Run against testing set 3 of 4.
#predictions <- predict(testModel, newdata=partition.df_testing3)
#print(confusionMatrix(predictions, partition.df_testing3$classe), digits=4)

# Run against 20 testing set provided by Professor Leek.
#print(predict(testModel, newdata=pmlTestingData))

# Train on training set 4 of 4 with only cross validation.
#set.seed(1979)
#testModel <- train(partition.df_training4$classe ~ ., method="rf", preProcess=c("center", "scale"), trControl=trainControl(method = "cv", number = 4), data=partition.df_training4)
#print(testModel, digits=3)

# Run against testing set 4 of 4.
#predictions <- predict(testModel, newdata=partition.df_testing4)
#print(confusionMatrix(predictions, partition.df_testing4$classe), digits=4)

# Run against 20 testing set provided by Professor Leek.
print(predict(testModel, newdata=pmlTestingData))
