library(caret)
library(rpart)
library(rpart.plot)
library(RColorBrewer)
library(rattle)
library(randomForest)

set.seed(12345)

# Load data. Import if necessary 

if (!file.exists("pml-training.csv")) { 
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv",  
                destfile = "pml-training.csv") 
} 
if (!file.exists("pml-testing.csv")) { 
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv",  
                destfile = "pml-testing.csv") 
}

# Convert NA,NULL,#DIV/0! when reading in the data 

pmlTrainingSet <- read.csv("pml-training.csv", header = TRUE, sep =  ",",na.strings=c('NA','','#DIV/0!')) 
pmlTestingSet <- read.csv("pml-testing.csv", header = TRUE, sep =  ",",na.strings=c('NA','','#DIV/0!'))

for(i in c(8:ncol(pmlTrainingSet)-1)){ 
  pmlTrainingSet[,i]=as.numeric(as.character((pmlTrainingSet[,i])))
} 

pmlTraining_dataSet <- colnames(pmlTrainingSet[colSums(is.na(pmlTrainingSet)) == 0])[-(1:7)] 
pmlTraining_ModelDataSet <- pmlTrainingSet[pmlTraining_dataSet] 
colnames(pmlTraining_ModelDataSet) 

inTrain <- createDataPartition(y=pmlTrainingSet$classe, p=0.6, list=FALSE ) 
training <- pmlTraining_ModelDataSet[inTrain,] 
testing <- pmlTraining_ModelDataSet[-inTrain,] 
dim(training) 
dim(testing)