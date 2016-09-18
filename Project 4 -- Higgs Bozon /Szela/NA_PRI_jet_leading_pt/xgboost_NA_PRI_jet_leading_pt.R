#Loading important packages.
library(xgboost)
library(caret)

setwd("~/GitHub-kszela24/higgs-bozon/Szela/NA_PRI_jet_leading_pt")

#Reading in training and testing data.
train = as.data.frame(read.csv("train_NA_PRI_jet_leading_pt.csv"))
test = as.data.frame(read.csv("test_NA_PRI_jet_leading_pt.csv"))

#Releveling b and s such that b = 0 and s = 1, so that we can do logistic regression on them.
levels(train$Label) = c(0, 1)
train$Label = as.numeric(train$Label) - 1

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


dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)

#Creating the params and training the model.
#Initial parameters are copied from a Santander Kaggle script, going to do a grid search eventually
#with cross validation to find the best parameters.
#We are using a binary:logistic objective since this is a binary classification problem.
#We are using gbtree because it generally provides better results than gblinear.
#Changed from 560 nrounds to 200 nrounds because the AUC seemed to level off at that stage and the 
#model may likely be overfitting (all preliminary).
#Set seed to 1234 for reproducibility (DONT FORGET TO DO THIS FOR YOUR OWN MODELS).
set.seed(1234)
param <- list(  objective           = "binary:logistic", 
                booster             = "gbtree",
                eval_metric         = eval_met,
                eta                 = 0.02,
                max_depth           = 5,
                subsample           = 0.6815,
                colsample_bytree    = 0.701
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 200,
                    nfolds              = 5,
                    verbose             = 1,
                    watchlist           = watchlist,
                    maximize            = FALSE
)


#Creating a sparse model matrix so that we can predict using the xgboost model we just trained.
test$Label <- -1
test <- sparse.model.matrix(Label ~ ., data = test)

preds <- predict(clf, test)

#Initial validation that we get roughly the same proportion of s's and b's in both the training
# and the test sets.  Looks decent, but we can still use this data to find
# which features seem to be important at least for predicting either a signal or not.  An idea
# is to tweak the threshold for what determines a signal to be the same as the prior probability
# of s or b in order to achieve a similar proportion.  Just a thought in case we find out that 
# they should be roughly similar.
preds.num.s = length(preds[preds > 0.5])
preds.num.b = length(preds[preds <= 0.5])

preds.num.s / (preds.num.b + preds.num.s)
#First run, no tuning = 26.8%

train.y.s = length(train.y[train.y == 1])
train.y.b = length(train.y[train.y == 0])

train.y.s / (train.y.s + train.y.b)
#Prior probability = 32.4%

#Checking feature importance
importance_matrix <- xgb.importance(dimnames(train.model)[[2]], model = clf)
xgb.plot.importance(importance_matrix)
