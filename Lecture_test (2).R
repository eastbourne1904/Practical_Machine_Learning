# Data

if (!file.exists("pml-training.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv") 
} 
pmlTraining <- read.csv('pml-training.csv', row.names = 1, stringsAsFactors = FALSE, na.strings = c("NA", "", "#DIV/0!")) 
if (!file.exists("pml-testing.csv")) {     
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv") } 
read.csv('pml-testing.csv', row.names = 1, stringsAsFactors = FALSE, na.strings = c("NA", "", "#DIV/0!")) 

# Smaller dataset (get rid of rows we don't need for prediction)
## [1] "user_name"                "raw_timestamp_part_1"     "raw_timestamp_part_2"     "cvtd_timestamp"          
## [5] "new_window"               "num_window"
pmlTraining_subSet <- pmlTraining[,-c(1:7)]

# Build a predictor (In sample and out of sample error)
rule1 <- function(x){
  prediction <- rep(NA,length(x))
  prediction[x > 2.8] <- "gt28"
  prediction[x <= 2.8] <- "lte28"
  return(prediction)
}
# to get error predictions for classe vs pitch_belt...
table(rule1(pmlTraining_subSet$classe),pmlTraining_subSet$pitch_belt)

# 1. Define your error rate 
# 2. Split data into Training and Testing (validation optional) - This has already been done at the onset
# 3. On the training set pick features (cross validation - what is most important to use)
# 4. On the training set pick prediction function (use cross validation)
# 5. If no validation apply 1 time to test set
# 6. If validation apply to test set and refine, apply 1 time to validation
