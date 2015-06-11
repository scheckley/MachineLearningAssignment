#assignment
rm(list = ls()) #clear all
library(curl)
library(caret)
library(rpart)
library(rpart.plot)
library(randomForest)
library(gplots)
library(rattle)
library(RColorBrewer)
#obtain the data

testing_url <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
training_url <-"https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"

if (!file.exists("pml-testing.csv")) {
  download.file(testing_url, destfile="pml-testing.csv", method="curl")
}

if (!file.exists("pml-training.csv")) {
  download.file(training_url, destfile="pml-training.csv", method="curl")
}

#load the data
data.testing <- read.csv("pml-testing.csv",na.strings=c("NA", "#DIV/0!"))
data.training <- read.csv("pml-training.csv",na.strings=c("NA", "#DIV/0!"))
#View(data.training)

#remove missing values
data.testing <- data.testing[, colSums(is.na(data.testing)) == 0] 
data.training <- data.training[, colSums(is.na(data.training)) == 0] 

#remove unnecessary columns
data.testing <- data.testing[ , !names(data.testing) %in% c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window","num_window")]
data.training <- data.training[ , !names(data.training) %in% c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window","num_window")]

#some exploratory data analysis
data4map <- cor(data.training[, names(data.training) != "classe"])
my_palette <- colorRampPalette(c("darkblue", "blue", "white", "darkred", "red"))(n = 1000)
heatmap.2(data4map,
          col = my_palette,
          dendrogram="both",
          density.info = "none",
          trace="none",
          margins = c(5,10),
          key=TRUE,
          scale="none"
          )
#looks reasonably random.

#pca
tmp = preProcess(data.training[,-53], 
                   method=c("BoxCox", "center", 
                            "scale", "pca"))
tmp
#tmp.pca = predict(tmp, data.training[,-53])
#pca reveals 25 variables explain 95% of the variability in the data

#prep data for model training
inTrain <- createDataPartition(y = data.training$classe, p = 0.7, list = FALSE)
data4training <- data.training[inTrain, ]
data4testing <- data.training[-inTrain, ]

#auxillary model settings and cross validation
library(doMC)
registerDoMC(cores = detectCores(logical=FALSE))
set.seed(8418)
trControl <- trainControl(method = "cv", number = 10, allowParallel = TRUE, verboseIter = FALSE)

#fit using random forest
modfit <- train(classe ~ ., data=data4training, method="rf", trControl=trControl, prox=FALSE)
modfit
modfit.test <- predict(modfit, newdata = data4testing)
cfMatrix <- confusionMatrix(data4testing$classe, modfit.test)
cfMatrix

missClassfiers = function(values, model) {
  sum(model != values)/length(values)
}
#missclassification error
out.sample.error = missClassifiers(data4testing$classe, modfit.test)
out.sample.error

#final predictions on test data set
data.testing$problem_id <- NULL #don't need this column
test.result <- predict(modfit, data.testing)
test.result
fancyRpartPlot(modfit$finalModel) 
varImp(modfit)

#function to write text file containing each individual model prediction
pml_write_files <- function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("answers/problem_id_",i,".txt")
    write.table(x[i], file=filename, quote=FALSE,
                row.names=FALSE, col.names=FALSE)
  }
}
answers <- as.character(test.result)
pml_write_files(test.result)
