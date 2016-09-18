library(xgboost)
library(caret)
library(nnet)

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model")

#Reading in training and testing data.
train = as.data.frame(read.csv("training.csv"))
test = as.data.frame(read.csv("test.csv"))

#Releveling b and s such that b = 0 and s = 1, so that we can do logistic regression on them.
levels(train$Label) = c(0, 1)
train$Label = as.numeric(train$Label) - 1

#countNAs <- function(x) {
#  return( sum(x == -999.0) )
#}
#train$nNA <- apply(train, 1, FUN=countNAs)
#test$nNA <- apply(test, 1, FUN=countNAs)

#train$PRI_jet_num = as.factor(train$PRI_jet_num)
#test$PRI_jet_num = as.factor(test$PRI_jet_num)


sum(train$Label) / length(train$Label)

#Creating a vector for our target variable.
train.y <- as.vector(train$Label)
train$Label = NULL

#Setting the evaluation metric for the xgboost to AUC to begin with, although others may be better
eval_met = "ams"

#Removing the training weight and training and testing eventID.
#Don't currently know what to do with the weight, so removing it preliminarily.
#Need to remove the training and testing eventID so that we don't train and predict on them.
train.ID <- train$EventId
train$EventId = NULL

test.ID <- test$EventId
test$EventId <- NULL

train.weight <- train$Weight
train$Weight = NULL

#train.NA_to_0 = train
#test.NA_to_0 = test
#train.NA_to_0[train.NA_to_0 == -999.0] <- 0
#test.NA_to_0[test.NA_to_0 == -999.0] <- 0
#train$sum = rowSums(train.NA_to_0)
#test$sum = rowSums(test.NA_to_0)

sumwpos <- sum(train.weight * (train.y==1.0))
sumwneg <- sum(train.weight * (train.y==0.0))
print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))

train$TARGET = train.y
train$TARGET = NULL

#Remove constant features if there are any.  (Might arise due to number of jets being split into 
#different groups along with the NA's
cat("\n## Removing the constants features.\n")
for (f in names(train)) {
  if (length(unique(train[[f]])) == 1) {
    cat(f, "is constant in train. We delete it.\n")
    train[[f]] <- NULL
    test[[f]] <- NULL
  }
}

train.nn <- nnet(x = train, y = train.y, weights = train.weight, linout = T, maxit = 10, size = 100,
                 MaxNWts = 250001)



