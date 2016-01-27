library(caret) 

ptrain <- read.csv("pml-training.csv") 
ptest <- read.csv("pml-testing.csv") 


set.seed(10) 
inTrain <- createDataPartition(y=ptrain$classe, p=0.7, list=F) 
ptrain1 <- ptrain[inTrain, ] 
ptrain2 <- ptrain[-inTrain, ] 

nzv <- nearZeroVar(ptrain1) 
ptrain1 <- ptrain1[, -nzv] 
ptrain2 <- ptrain2[, -nzv] 

mostlyNA <- sapply(ptrain1, function(x) mean(is.na(x))) > 0.95 
ptrain1 <- ptrain1[, mostlyNA==F] 
ptrain2 <- ptrain2[, mostlyNA==F] 

ptrain1 <- ptrain1[, -(1:5)] 
ptrain2 <- ptrain2[, -(1:5)] 

fitControl <- trainControl(method="cv", number=3, verboseIter=F) 
fit <- train(classe ~ ., data=ptrain1, method="rf", trControl=fitControl) 
fit$finalModel 
preds <- predict(fit, newdata=ptrain2) 



######
# Answer:
######
#> fit

#Random Forest 

#13737 samples
#53 predictor
#5 classes: 'A', 'B', 'C', 'D', 'E' 

#No pre-processing
#Resampling: Cross-Validated (3 fold) 
#Summary of sample sizes: 9158, 9159, 9157 
#Resampling results across tuning parameters:
  
#  mtry  Accuracy   Kappa      Accuracy SD   Kappa SD    
#2    0.9906092  0.9881209  0.0006572157  0.0008322066
#27    0.9957052  0.9945676  0.0014531077  0.0018376808
#53    0.9935944  0.9918974  0.0029475015  0.0037278121

#Accuracy was used to select the optimal model using  the largest value.
#The final value used for the model was mtry = 27. 

#> preds
# Lots of A thru E's
# Levels: A B C D E