# Data

if (!file.exists("pml-training.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv") 
} 

if (!file.exists("pml-testing.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv") } 

# libraries
library(caret)
   # Loading required package: lattice
   # Loading required package: ggplot2
library(e1071)
library(randomForest)
 
# Some preprocessing (cleaning)
## PreProcess
   # 1. Define your error rate (.75)
   # 2. Split data into Training and Testing (validation optional) - This has already been done at the onset
   pmlTraining <- read.csv('pml-training.csv', row.names = 1, stringsAsFactors = FALSE, na.strings = c("NA", "", "#DIV/0!")) 
   #pmlTesting <- read.csv('pml-testing.csv', row.names = 1, stringsAsFactors = FALSE, na.strings = c("NA", "", "#DIV/0!"))

   # 3. On the training set pick features (cross validation - what is most important to use)
   # Smaller dataset (get rid of rows we don't need for prediction)
   ## [1] "user_name"                "raw_timestamp_part_1"     "raw_timestamp_part_2"     "cvtd_timestamp"          
   ## [5] "new_window"               "num_window"
   pmlTraining_subSet <- pmlTraining[,-c(1:7)]
   
   # Get rid of near zero variables
   nzv <- nearZeroVar(pmlTraining_subSet)
   # freqRatio percentUnique zeroVar   nzv
   # kurtosis_roll_belt        2.000000    2.01304658   FALSE FALSE
   # kurtosis_picth_belt       1.333333    1.60534094   FALSE FALSE
   # kurtosis_yaw_belt         0.000000    0.00000000    TRUE  TRUE
   pmlTraining_subSet.new <- pmlTraining_subSet[, -nzv]
   # min_yaw_arm amplitude_yaw_arm roll_dumbbell pitch_dumbbell yaw_dumbbell kurtosis_roll_dumbbell kurtosis_picth_dumbbell
   # 1              NA                NA    13.0521746    -70.4940037  -84.8739389                     NA                      NA
   # 2              NA                NA    13.1307396    -70.6375051  -84.7106471                     NA  
   
   # Get rid of variables that are NA
   NAs <- sapply(pmlTraining_subSet.new, function(x) mean(is.na(x))) > 0.95
   pmlTraining_subSet.new2 <- pmlTraining_subSet.new[, NAs==FALSE] 
   # accel_arm_x accel_arm_y accel_arm_z magnet_arm_x magnet_arm_y magnet_arm_z roll_dumbbell pitch_dumbbell yaw_dumbbell
   # 1            -288         109        -123         -368          337          516    13.0521746    -70.4940037  -84.8739389
   # 2            -290         110        -125         -369          337          513    13.1307396    -70.6375051  -84.7106471

## Data splitting
   # createData Partition  (we don't need to do this because our training and testing are already separate above)
      #inTrain <- createDataPartion(y=pmlTraining_subSet$classe,p=.75, list=FALSE)
      #training <- pmlTraining_subSet[inTrain,]
      #testing <- pmlTraining_subSet[-inTrain,]
      #dim(training)
   # createResample
   # create Timeslices

## Training/testing functions
   # 4. On the training set pick prediction function (use cross validation)
   # train
   set.seed(32343)
   # instruct train to use 3-fold CV to select optimal tuning parameters 
      # package e1071 is required
      # package randomForest is required
   fitControl <- trainControl(method="cv", number=3, verboseIter=F) 
   modelFit <- train(classe ~ ., data=pmlTraining_subSet.new2, method="rf", trControl=fitControl) 
   prediction1 <- predict(modelFit,data=pmlTraining)
   prediction1
   # predict
   
## Model comparison
   # confusionMatrix
   # 5. If no validation apply 1 time to test set
   # 6. If validation apply to test set and refine, apply 1 time to validation
