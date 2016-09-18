library(VIM)
library(mice)



setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 4")


train = read.csv("training.csv")
str(train)
#View(train)

#train2 = train

train[train==-999]=NA
#View(train2)

#str(train2)

summary(train)

which(colSums(is.na(train)) != 0 )

which(rowSums(is.na(train)) != 0 )

md.pattern(train)

aggr(train)

colSums(is.na(train))

#zeyu example

dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
dfTrain[dfTrain==-999.0] <- NA
dfTest[dfTest==-999.0] <- NA
testId = dfTest$EventId

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]



#random forests

library(randomForest)
library(dplyr)
library(caret)
library(pROC)

#Releveling b and s such that b = 0 and s = 1, so that we can do logistic regression on them.
#levels(dfTrain$Label) = c(0, 1)
#dfTrain$Label = as.numeric(dfTrain$Label) - 1




jetnum0  = filter(dfTrain,PRI_jet_num == '0')

jetnum1  = filter(dfTrain,PRI_jet_num == '1')

jetnum23  = filter(dfTrain,PRI_jet_num == '2'|PRI_jet_num == '3')


which(colSums(is.na(jetnum0)) != 0 )

which(colSums(is.na(jetnum1)) != 0 )

which(colSums(is.na(jetnum23)) != 0 )

aggr(jetnum0)

aggr(jetnum1)

aggr(jetnum23)

aggr(dfTrain, combined = TRUE,)





testnum0  = filter(dfTest,PRI_jet_num == '0')

testnum1  = filter(dfTest,PRI_jet_num == '1')

testnum23  = filter(dfTest,PRI_jet_num == '2'|PRI_jet_num == '3')

testId0 = testnum0$EventId

testId1 = testnum1$EventId

testId23 = testnum23$EventId

#trainnum0 =sample(1:nrow(jetnum0), 7*nrow(jetnum0)/10) #Training indices.

#trainnum1 =sample(1:nrow(jetnum1), 7*nrow(jetnum1)/10)

#trainnum23 =sample(1:nrow(jetnum23), 7*nrow(jetnum23)/10)

#initial random forest full data
set.seed(0)
rf = randomForest(Label ~ ., data = dfTrain[,-c(1,32)], importance = TRUE)
rf

#OOB error rate 16.04%

varImpPlot(rf)

#finding appropriate threshold using zeyu's code
rfTrainPred <- predict(rf, newdata=train, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels, rfTrainPred[,2])
plot(auc, print.thres=TRUE)

# From the graph, we can tell the best threshold is 0.496
threshold <- 0.496

rfTestPred <- predict(rf, newdata=test, type="prob")

predicted <- rep("b",550000)
predicted[rfTestPred[,2]>=threshold] <- "s"
weightRank = rank(rfTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)
write.csv(submission, "rf_submission.csv", row.names=FALSE)


#seperating the NA's and training them seperately


#initial random forest jet number 0


set.seed(0)
rf1 = randomForest(Label ~ ., data = jetnum0[,-c(1,32)], importance = TRUE)
rf1

varImpPlot(rf1)

table(predict(rf1, jetnum0[-trainnum0,], type = 'class'),jetnum0[-trainnum0,]$Label)

#initial random forest jet number 1

set.seed(0)
rf2 = randomForest(Label ~ ., data = jetnum1[,-c(1,32)], importance = TRUE)
rf2

varImpPlot(rf2)

table(predict(rf2, jetnum1[-trainnum1,], type = 'class'),jetnum1[-trainnum1,]$Label)

#initial random forest jet number 2-3
      
set.seed(0)
rf3 = randomForest(Label ~ ., data = jetnum23[,-c(1,32)], importance = TRUE)
rf3
      
varImpPlot(rf3)
      
table(predict(rf3, jetnum23[-trainnum23,], type = 'class'),jetnum23[-trainnum23,]$Label)


#selecting the best tree from all the variables

library(doParallel) 
registerDoParallel(cores = 4)



set.seed(0)
oob.err = numeric(30)
for (mtry in 1:30) {
  fit = randomForest(Label ~ ., data = dfTrain[,-c(1,32)], mtry = mtry)
  oob.err[mtry] = fit$err.rate[500]
  cat("We're performing iteration", mtry, "\n")
}





#Visualizing the OOB error.
plot(1:30, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.boston)
varImpPlot(rf.boston)




#zeyu example of random forest
# Windows users please follow the instruction on stackoverflow: http://stackoverflow.com/a/24655923

ctrl = trainControl(method = "repeatedcv",number = 5, #should be 5 or 10
                    summaryFunction = AMS_summary)

rfGrid <-  expand.grid(mtry = c(3,6,9))

m_rf = train(x=train, y=labels, 
             method="rf", weights=weight, 
             verbose=TRUE, trControl=ctrl, metric="AMS")



















set.seed(0)
rf = randomForest(Label ~ ., data = train2, importance = TRUE)
rf

#The MSE and percent variance explained are based on out-of-bag estimates,
#yielding unbiased error estimates. The model reports that mtry = 4, which is
#the number of variables randomly chosen at each split. Since we have 13 overall
#variables, we could try all 13 possible values of mtry. We will do so, record
#the results, and make a plot.

#Varying the number of variables used at each step of the random forest procedure.
set.seed(0)
oob.err = numeric(30)
for (mtry in 1:30) {
  fit = randomForest(medv ~ ., data = Boston[train, ], mtry = mtry)
  oob.err[mtry] = fit$mse[500]
  cat("We're performing iteration", mtry, "\n")
}

#Visualizing the OOB error.
plot(1:13, oob.err, pch = 16, type = "b",
     xlab = "Variables Considered at Each Split",
     ylab = "OOB Mean Squared Error",
     main = "Random Forest OOB Error Rates\nby # of Variables")

#Can visualize a variable importance plot.
importance(rf.boston)
varImpPlot(rf.boston)
