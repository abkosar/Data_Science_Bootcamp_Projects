library(xgboost)
library(caret)
library(VIM)
setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model")

#Reading in training and testing data.
train = as.data.frame(read.csv("training.csv"))
test = as.data.frame(read.csv("test.csv"))

train[train == -999.0] <- NA

summary.aggr(train, bars = F, combined = T)

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
eval_met = "auc"

#Removing the training weight and training and testing eventID.
#Don't currently know what to do with the weight, so removing it preliminarily.
#Need to remove the training and testing eventID so that we don't train and predict on them.
train.ID <- train$EventId
train$EventId = NULL

test.ID <- test$EventId
test$EventId <- NULL

train.weight <- train$Weight
train$Weight = NULL

train.NA_to_0 = train
test.NA_to_0 = test
train.NA_to_0[train.NA_to_0 == -999.0] <- 0
test.NA_to_0[test.NA_to_0 == -999.0] <- 0
train$sum = rowSums(train.NA_to_0)
test$sum = rowSums(test.NA_to_0)

sumwpos <- sum(train.weight * (train.y==1.0))
sumwneg <- sum(train.weight * (train.y==0.0))
print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))

train$TARGET = train.y

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

#Preparing for the training of the xgboost model.
train.model <- sparse.model.matrix(TARGET ~ ., data = as.data.frame(train))

length(train.y)


dtrain <- xgb.DMatrix(data = train.model, label = train.y, missing = -999, weight = train.weight)
watchlist <- list(train=dtrain)

set.seed(1234)
param <- list(  objective           = "binary:logitraw", 
                "scale_pos_weight" = sumwneg / sumwpos,
                booster             = "gbtree",
                eval_metric         = "ams@0.15",
                eta                 = 0.1,
                max_depth           = 6
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 250,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)

#Creating a sparse model matrix so that we can predict using the xgboost model we just trained.
test$TARGET <- -1
test.model <- sparse.model.matrix(TARGET ~ ., data = test)

preds <- predict(clf, test.model)

#Putting back in eventid's
submission = data.frame(EventId = test.ID, predictions = preds)

#Sorting the predictions from lowest to highest.
submission_sorted = submission[order(submission$predictions), ]
submission_sorted$RankOrder = 1:550000

threshold = 550000 - as.integer(0.15 * length(test[,1]))

submission_sorted$Class = ifelse(submission_sorted$RankOrder <= threshold, "b", "s")
save_predictions = submission_sorted$predictions
submission_sorted$predictions = NULL

write.csv(submission_sorted, "submission_11.csv", row.names = F)

#Checking feature importance
importance_matrix <- xgb.importance(dimnames(train.model)[[2]], model = clf)
xgb.plot.importance(importance_matrix)
