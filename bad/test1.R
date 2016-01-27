if (!file.exists("pml-training.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv", 
                destfile = "pml-training.csv")
}
if (!file.exists("pml-testing.csv")) {
  download.file("http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv", 
                destfile = "pml-testing.csv")
}
pmlTraining <- read.csv("pml-training.csv", header = TRUE, sep = ",",na.strings=c("#DIV/0!"))
pmlTesting <- read.csv("pml-testing.csv", header = TRUE, sep = ",",na.strings=c("#DIV/0!"))
