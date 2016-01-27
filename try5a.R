library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

set.seed(12345)

if (!file.exists("pml-training.csv")) { 
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",  
                destfile = "pml-training.csv") 
} 

if (!file.exists("pml-testing.csv")) { 
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",  
                destfile = "pml-testing.csv") 
} 

pmlTrainingSet <- read.csv("pml-training.csv", header = TRUE, sep =  ",",na.strings=c('NA','','#DIV/0!'))
pmlTestingSet <- read.csv("pml-testing.csv", header = TRUE, sep =  ",",na.strings=c('NA','','#DIV/0!'))

# ---------------------good---------------------

# Convert columns 8 to end minus the classe column to numeric 
for(i in c(8:ncol(pmlTrainingSet)-1)){ 
  pmlTrainingSet[,i]=as.numeric(as.character((pmlTrainingSet[,i])))
}
for(i in c(8:ncol(pmlTestingSet)-1)){ 
  pmlTestingSet[,i]=as.numeric(as.character((pmlTestingSet[,i])))
}

inTrain <- createDataPartition(y=pmlTrainingSet$classe, p=0.6, list=FALSE )
pmlTrainingData <- pmlTrainingSet[inTrain, ]
pmlTestingData <- pmlTrainingSet[-inTrain, ]
dim(pmlTrainingData)
dim(pmlTestingData)

nsv <- nearZeroVar(pmlTrainingData, saveMetrics=TRUE)

nsvVars <- names(pmlTrainingData) %in% c("new_window", "kurtosis_roll_belt", "kurtosis_picth_belt",
                                         "kurtosis_yaw_belt", "skewness_roll_belt", "skewness_roll_belt.1", "skewness_yaw_belt",
                                         "max_yaw_belt", "min_yaw_belt", "amplitude_yaw_belt", "avg_roll_arm", "stddev_roll_arm",
                                         "var_roll_arm", "avg_pitch_arm", "stddev_pitch_arm", "var_pitch_arm", "avg_yaw_arm",
                                         "stddev_yaw_arm", "var_yaw_arm", "kurtosis_roll_arm", "kurtosis_picth_arm",
                                         "kurtosis_yaw_arm", "skewness_roll_arm", "skewness_pitch_arm", "skewness_yaw_arm",
                                         "max_roll_arm", "min_roll_arm", "min_pitch_arm", "amplitude_roll_arm", "amplitude_pitch_arm",
                                         "kurtosis_roll_dumbbell", "kurtosis_picth_dumbbell", "kurtosis_yaw_dumbbell", "skewness_roll_dumbbell",
                                         "skewness_pitch_dumbbell", "skewness_yaw_dumbbell", "max_yaw_dumbbell", "min_yaw_dumbbell",
                                         "amplitude_yaw_dumbbell", "kurtosis_roll_forearm", "kurtosis_picth_forearm", "kurtosis_yaw_forearm",
                                         "skewness_roll_forearm", "skewness_pitch_forearm", "skewness_yaw_forearm", "max_roll_forearm",
                                         "max_yaw_forearm", "min_roll_forearm", "min_yaw_forearm", "amplitude_roll_forearm",
                                         "amplitude_yaw_forearm", "avg_roll_forearm", "stddev_roll_forearm", "var_roll_forearm",
                                         "avg_pitch_forearm", "stddev_pitch_forearm", "var_pitch_forearm", "avg_yaw_forearm",
                                         "stddev_yaw_forearm", "var_yaw_forearm")

pmlTrainingData <- pmlTrainingData[!nsvVars]

#To check the new N?? of observations
dim(pmlTrainingData)

pmlTrainingData <- pmlTrainingData[c(-1)]

pmlTrainingData_Subset <- pmlTrainingData #creating another subset to iterate in loop
for(i in 1:length(pmlTrainingData)) { #for every column in the training dataset
  if( sum( is.na( pmlTrainingData[, i] ) ) /nrow(pmlTrainingData) >= .6 ) { #if n?? NAs > 60% of total observations
    for(j in 1:length(pmlTrainingData_Subset)) {
      if( length( grep(names(pmlTrainingData[i]), names(pmlTrainingData_Subset)[j]) ) ==1)  { #if the columns are the same:
        pmlTrainingData_Subset <- pmlTrainingData_Subset[ , -j] #Remove that column
      }   
    } 
  }
}
#To check the new N?? of observations
dim(pmlTrainingData_Subset)

pmlTrainingData <- pmlTrainingData_Subset
rm(pmlTrainingData_Subset)

pmlTrainingData_check <- colnames(pmlTrainingData)
pmlTrainingData_removeLastColumn <- colnames(pmlTrainingData[, -58]) #already with classe column removed

pmlTestingData <- pmlTestingData[pmlTrainingData_check]
testing <- testing[pmlTrainingData_removeLastColumn]

#To check the new N?? of observations
dim(pmlTestingData)

#To check the new N?? of observations
dim(testing)

for (i in 1:length(testing) ) {
  for(j in 1:length(pmlTrainingData)) {
    if( length( grep(names(pmlTrainingData[i]), names(testing)[j]) ) ==1)  {
      class(testing[j]) <- class(pmlTrainingData[i])
    }      
  }      
}
#And to make sure Coertion really worked, simple smart ass technique:
testing <- rbind(pmlTrainingData[2, -58] , testing) #note row 2 does not mean anything, this will be removed right.. now:
testing <- testing[-1,]

modFitA1 <- rpart(classe ~ ., data=pmlTrainingData, method="class")

fancyRpartPlot(modFitA1)

predictionsA1 <- predict(modFitA1, pmlTestingData, type = "class")

confusionMatrix(predictionsA1, pmlTestingData$classe)

modFitB1 <- randomForest(classe ~. , data=pmlTrainingData)

predictionsB1 <- predict(modFitB1, pmlTestingData, type = "class")

confusionMatrix(predictionsB1, pmlTestingData$classe)

predictionsB2 <- predict(modFitB1, testing, type = "class")

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsB2)