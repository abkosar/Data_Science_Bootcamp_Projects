library(xgboost)
library(caret)
library(doMC)
registerDoMC(2)

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation")

seeds = c(0, 1111, 1234)

#Reading in training data.
train.cv = as.data.frame(read.csv("training.csv"))
test.bagging = as.data.frame(read.csv("test.csv"))


train.cv = train.cv[, -c(17, 20, 22, 27, 29, 30)]
test.bagging = test.bagging[, -c(17, 20, 22, 27, 29, 30)]

labels = train.cv$Label

#Releveling b and s such that b = 0 and s = 1, so that we can do logistic regression on them.
levels(train.cv$Label) = c(0, 1)
train.cv$Label = as.numeric(train.cv$Label) - 1

proc = proc.time()
for (the_seed in seeds) {
  #Create 2 fold partition.
  set.seed(the_seed)
  #idx = createDataPartition(train.cv$Label, p=.80, list=FALSE, times = 5)
  idx = createFolds(train.cv$Label, k = 2, list = TRUE, returnTrain = FALSE)
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
    
    #for_stack_test = paste0("stack_", i, "_", the_seed,"_test.csv")
    for_stack_train = paste0("cv_", i, "_", the_seed,".csv")
    #sub_file = paste0("cv_", i, "_0.csv")
    sol_file = paste0("solution_cv_", i, "_", the_seed,".csv")
    
    #write.csv(submission_bagged, for_stack_test, row.names = F)
    write.csv(submission_sorted, for_stack_train, row.names = F)
    #write.csv(submission_sorted, sub_file, row.names = F)
    write.csv(solution.df, sol_file, row.names = F)
  }
}
proc.time() - proc