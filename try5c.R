require(caret) 
require(rpart)
require(rattle)

pmlTrainingSet <- read.csv("pml-training.csv", header = TRUE, sep = ",",na.strings=c('NA','','#DIV/0!'))
pmlTestingSet <- read.csv("pml-testing.csv", header = TRUE, sep = ",",na.strings=c('NA','','#DIV/0!'))

inTrain <- createDataPartition(y=pmlTraining$classe,p=0.7,list=FALSE)
training <- pmlTraining[inTrain,]
testing <- pmlTraining[-inTrain,]
dim(training);dim(testing)

modFit <- train(classe ~ .,method="rpart",data=training)
print(modFit$finalModel)

plot(modFit$finalModel,uniform=TRUE, main="Classification Tree")
text(modFit$finalModel,use.n=TRUE,all=TRUE,cex=.8)

fancyRpartPlot(modFit$finalModel)   # shows a fancier version of plain rpart plot

predict(modFit,newdata=testing)    # predict new values
