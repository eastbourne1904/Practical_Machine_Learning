# Could not get to work

require(data.table)

if (!file.exists("pml-training.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv") 
} 

if (!file.exists("pml-testing.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv") } 

isAnyMissing <- sapply(DTest, function (x) any(is.na(x) | x == ""))
isPredictor <- !isAnyMissing & grepl("belt|[^(fore)]arm|dumbbell|forearm", names(isAnyMissing))
predCandidates <- names(isAnyMissing)[isPredictor]
predCandidates

varToInclude <- c("classe", predCandidates)
D <- D[, varToInclude, with=FALSE]
dim(D)

names(D)

D <- D[, classe := factor(D[, classe])]
D[, .N, classe]

require(caret)

seed <- as.numeric(as.Date("2014-10-26"))
set.seed(seed)
inTrain <- createDataPartition(D$classe, p=0.6)
DTrain <- D[inTrain[[1]]]
DProbe <- D[-inTrain[[1]]]

X <- DTrain[, predCandidates, with=FALSE]
preProc <- preProcess(X)
preProc

XCS <- predict(preProc, X)
DTrainCS <- data.table(data.frame(classe = DTrain[, classe], XCS))

X <- DProbe[, predCandidates, with=FALSE]
XCS <- predict(preProc, X)
DProbeCS <- data.table(data.frame(classe = DProbe[, classe], XCS))

nzv <- nearZeroVar(DTrainCS, saveMetrics=TRUE)
if (any(nzv$nzv)) nzv else message("No variables with near zero variance")

require(parallel)
require(doParallel)

cl <- makeCluster(detectCores() - 1)
registerDoParallel(cl)

ctrl <- trainControl(classProbs=TRUE,
                     savePredictions=TRUE,
                     allowParallel=TRUE)

method <- "rf"
system.time(trainingModel <- train(classe ~ ., data=DTrainCS, method=method))

stopCluster(cl)

trainingModel

hat <- predict(trainingModel, DTrainCS)
confusionMatrix(hat, DTrain[, classe])

hat <- predict(trainingModel, DProbeCS)
confusionMatrix(hat, DProbeCS[, classe])

varImp(trainingModel)

trainingModel$finalModel

save(trainingModel, file="trainingModel.RData")

load(file="trainingModel.RData", verbose=TRUE)

DTestCS <- predict(preProc, DTest[, predCandidates, with=FALSE])
hat <- predict(trainingModel, DTestCS)
DTest <- cbind(hat , DTest)
subset(DTest, select=names(DTest)[grep("belt|[^(fore)]arm|dumbbell|forearm", names(DTest), invert=TRUE)])

