# Import data
if (!file.exists("pml-training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv")
}

# set libraries
library(ggplot2)
library(dplyr)
library(caret)

# Read data, while dealing with missing or error data
pmlTraining <- read.csv('pml-training.csv', na.strings = c("NA", "", "#DIV/0!"))
pmlTesting <-  read.csv('pml-testing.csv', na.strings = c("NA", "", "#DIV/0!")) 

# Set seed so we can reproduce
set.seed(9523)

# Create Data Partition to estimate the out-of-sample error
dataPartitions <- createDataPartition(y=pmlTraining$classe, times = 1, p=0.7, list=FALSE)

pmlTraining_subSet <- pmlTraining[dataPartitions, ]  # Subset
pmlTraining_validation <- pmlTraining[-dataPartitions, ]  # Validation set

# Remove variables with nearly zero variance
nzv <- nearZeroVar(pmlTraining_subSet, saveMetrics= TRUE)
#nzv[nzv$nzv,][1:10,]
#dim(pmlTraining)
nzv <- nearZeroVar(pmlTraining_subSet)
filteredDescr <- pmlTraining_subSet[, -nzv]
#dim(filteredDescr)
