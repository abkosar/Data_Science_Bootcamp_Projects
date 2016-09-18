library(xgboost)
library(caret)
library(doMC)
registerDoMC(3)

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation")
the_seed = 0
#Reading in training data.
train.cv = as.data.frame(read.csv("training.csv"))
test.bagging = as.data.frame(read.csv("test.csv"))

train.cv = train.cv[, -c(17, 20, 22, 27, 30)]
test.bagging = test.bagging[, -c(17, 20, 22, 27, 30)]

labels = train.cv$Label

#Releveling b and s such that b = 0 and s = 1, so that we can do logistic regression on them.
levels(train.cv$Label) = c(0, 1)
train.cv$Label = as.numeric(train.cv$Label) - 1

#Create 2 fold partition.
set.seed(the_seed)
#idx = createDataPartition(train.cv$Label, p=.80, list=FALSE, times = 5)
idx = createFolds(train.cv$Label, k = 5, list = TRUE, returnTrain = FALSE)
idx = as.data.frame(idx)


foreach(i=1:length(idx)) %dopar% {
  train.label = labels[-idx[[i]]]
  test.label = labels[idx[[i]]]
  
  train = train.cv[-idx[[i]],]
  test = train.cv[idx[[i]],]
  
  #Separate columns we don't want to train on.
  train.y <- as.vector(train$Label)
  train$Label = NULL
  
  test.y <- as.vector(test$Label)
  test$Label = NULL
  
  train.ID = train$EventId
  train$EventId = NULL
  
  test.ID <- test$EventId
  test$EventId = NULL
  
  test.bagging.ID <- test.bagging$EventId
  test.bagging$EventId = NULL
  
  train.weight = train$Weight
  train$Weight = NULL
  
  test.weight = test$Weight
  test$Weight = NULL
  
  sumwpos <- sum(train.weight * (train.y==1.0))
  sumwneg <- sum(train.weight * (train.y==0.0))
  print(paste("weight statistics: wpos=", sumwpos, "wneg=", sumwneg, "ratio=", sumwneg / sumwpos))
  
  train$TARGET = train.y
  
  #Preparing for the training of the xgboost model.
  train.model <- sparse.model.matrix(TARGET ~ ., data = as.data.frame(train))
  
  dtrain <- xgb.DMatrix(data = train.model, label = train.y, missing = -999, weight = train.weight)
  watchlist <- list(train=dtrain)
  
  set.seed(the_seed)
  param <- list(  objective           = "binary:logistic", 
                  "scale_pos_weight" = sumwneg / sumwpos,
                  booster             = "gbtree",
                  eval_metric         = "ams@0.15",
                  eta                 = 0.1,
                  max_depth           = 6
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 220,
                      verbose             = 1,
                      watchlist           = watchlist,
                      maximize            = FALSE
  )
  
  #Creating a sparse model matrix so that we can predict using the xgboost model we just trained.
  test$TARGET <- -1
  test.model <- sparse.model.matrix(TARGET ~ ., data = test)
  
  test.bagging$TARGET <- -1
  test.bagging.model <- sparse.model.matrix(TARGET ~ ., data = test.bagging)
  
  preds <- predict(clf, test.model)
  preds.bagging <- predict(clf, test.bagging.model)
  
  
  #Putting back in eventid's
  submission = data.frame(EventId = test.ID, predictions = preds)
  submission_bagged = data.frame(EventId = test.bagging.ID, predictions = preds.bagging)
  
  #Sorting the predictions from lowest to highest.
  submission_sorted = submission[order(submission$predictions), ]
  submission_sorted$RankOrder = 1:length(test.weight)
  
  threshold = length(test.weight) - as.integer(0.15 * length(test.weight))
  
  submission_sorted$Class = ifelse(submission_sorted$RankOrder <= threshold, "b", "s")
  save_predictions = submission_sorted$predictions
  submission_sorted$predictions = NULL
  
  solution.df = data.frame(EventId = test.ID, Label = test.label, Weight = test.weight)
  
  for_stack_test = paste0("stack_", i, "_", the_seed, "_test.csv")
  for_stack_train = paste0("stack_", i, "_", the_seed, "_train.csv")
  sub_file = paste0("cv_", i, "_", the_seed, ".csv")
  sol_file = paste0("solution_cv_", i, "_", the_seed, ".csv")
  
  write.csv(submission_bagged, for_stack_test, row.names = F)
  write.csv(submission, for_stack_train, row.names = F)
  write.csv(submission_sorted, sub_file, row.names = F)
  write.csv(solution.df, sol_file, row.names = F)
}

train.cv.extra = rbind(as.data.frame(read.csv(paste0("stack_1_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_2_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_3_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_4_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_5_", the_seed, "_train.csv"))))

test.preds1 = as.data.frame(read.csv(paste0("stack_1_", the_seed, "_test.csv")))
test.preds2 = as.data.frame(read.csv(paste0("stack_2_", the_seed, "_test.csv")))
test.preds3 = as.data.frame(read.csv(paste0("stack_3_", the_seed, "_test.csv")))
test.preds4 = as.data.frame(read.csv(paste0("stack_4_", the_seed, "_test.csv")))
test.preds5 = as.data.frame(read.csv(paste0("stack_5_", the_seed, "_test.csv")))

test.preds.avg = (test.preds1$predictions + test.preds2$predictions + test.preds3$predictions + 
  test.preds4$predictions + test.preds5$predictions) / 5

train.cv.extra = train.cv.extra[order(train.cv.extra$EventId), ]$predictions

write.csv(train.cv.extra, paste0("predictions_stacked_", the_seed, "_train.csv"), row.names = F)

#Reading in training and testing data.
train = as.data.frame(read.csv("training.csv"))
test = as.data.frame(read.csv("test.csv"))

train = train[, -c(17, 20, 22, 27, 30)]
test = test[, -c(17, 20, 22, 27, 30)]

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

#Removing the training weight and training and testing eventID.
#Don't currently know what to do with the weight, so removing it preliminarily.
#Need to remove the training and testing eventID so that we don't train and predict on them.
train.ID <- train$EventId
train$EventId = NULL

test.ID <- test$EventId
test$EventId <- NULL

train.weight <- train$Weight
train$Weight = NULL

train$bagged <- train.cv.extra
test$bagged <- test.preds.avg

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

set.seed(the_seed)
param <- list(  objective           = "binary:logitraw", 
                "scale_pos_weight" = sumwneg / sumwpos,
                booster             = "gbtree",
                eval_metric         = "ams@0.15",
                eta                 = 0.1,
                max_depth           = 6
)

clf <- xgb.train(   params              = param, 
                    data                = dtrain, 
                    nrounds             = 220,
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

write.csv(submission, paste0("predictions_stacked_", the_seed, "_test.csv"), row.names = F)
write.csv(submission_sorted, paste0("submission_stacked_", the_seed, ".csv"), row.names = F)