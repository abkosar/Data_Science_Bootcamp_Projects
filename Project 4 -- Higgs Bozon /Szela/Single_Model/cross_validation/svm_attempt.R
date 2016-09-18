library(e1071)
library(caret)
setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation")

#Reading in training data.
train = as.data.frame(read.csv("training.csv"))
test = as.data.frame(read.csv("test.csv"))

labels = train$Label

#Releveling b and s such that b = 0 and s = 1, so that we can do logistic regression on them.
train$Label = as.numeric(train$Label) - 1

#Create a 5 fold partition to select 10% of the training data.
set.seed(0)
#idx = createDataPartition(train.cv$Label, p=.80, list=FALSE, times = 5)
idx = createFolds(train$Label, k = 20, list = TRUE, returnTrain = FALSE)
idx = as.data.frame(idx)

train$Label = NULL
train$Weight = NULL
train$EventId = NULL

#Taking only 10% of the training data so the model trains faster.
train.label = labels[idx[[1]]]
train.subset = train[idx[[1]],]

ptm <- proc.time()
svm_model = svm(train.subset, train.label, type = "C-classification", kernel = "linear",
                probability = TRUE)
summary(svm_model)
proc.time() - ptm

#user  system elapsed 4%
#50.814   0.287  51.089 

#user  system elapsed 5%
#98.119   0.338  98.453

#user  system elapsed 10%
#586.726   3.056 612.326 
