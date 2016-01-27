# https://rstudio-pubs-static.s3.amazonaws.com/66713_2ae121fc81a844ce9d5d8ef91900717f.html
# could not get to work

if (!file.exists("pml-training.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv") 
} 

if (!file.exists("pml-testing.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv") } 


a=read.csv('pml-training.csv',na.strings=c('','NA'))
b=a[,!apply(a,2,function(x) any(is.na(x)) )]
c=b[,-c(1:7)]

#install.packages('randomForest')
library('randomForest')
#install.packages('caret')
library('caret')
#install.packages('e1071')
library('e1071')

subGrps=createDataPartition(y=c$classe, p=0.6, list=FALSE)
subTraining=c[subGrps,]
subTesting=c[-subGrps, ]
dim(subTraining);dim(subTesting)

model=randomForest(classe~., data=subTraining, method='class')
pred=predict(model,subTesting, type='class')
z=confusionMatrix(pred,subTesting$classe)
save(z,file='test.RData')

load('test.RData')
z$table

z$overall[1]

d=read.csv('pml-testing.csv',na.strings=c('','NA'))
e=d[,!apply(d,2,function(x) any(is.na(x)) )]
f=e[,-c(1:7)]

predicted=predict(model,f,type='class')
save(predicted,file='predicted.RData')

#setwd('C:/users/a/Desktop')
load('predicted.RData')
p