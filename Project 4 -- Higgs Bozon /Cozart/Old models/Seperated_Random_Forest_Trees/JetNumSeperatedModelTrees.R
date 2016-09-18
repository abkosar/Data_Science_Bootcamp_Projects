setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 4")

dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)

# Convert PRI_jet_num to factor as instructed on the website.
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)
str(dfTrain)

labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]

library(randomForest)
library(dplyr)
library(caret)
library(pROC)

jetnum0  = filter(dfTrain,PRI_jet_num == '0')

jetnum1  = filter(dfTrain,PRI_jet_num == '1')

jetnum23  = filter(dfTrain,PRI_jet_num == '2'|PRI_jet_num == '3')

labels0 <- jetnum0$Label

labels1 <- jetnum1$Label

labels23 <- jetnum23$Label

train0 <- jetnum0[, -c(1,32,33)]

train1 <- jetnum1[, -c(1,32,33)]

train23 <- jetnum23[, -c(1,32,33)]

testnum0  = filter(dfTest,PRI_jet_num == '0')

testnum1  = filter(dfTest,PRI_jet_num == '1')

testnum23  = filter(dfTest,PRI_jet_num == '2'|PRI_jet_num == '3')

testId0 = testnum0$EventId

testId1 = testnum1$EventId

testId23 = testnum23$EventId

#initial random forest jet number 0

set.seed(0)
rf1 = randomForest(Label ~ ., data = jetnum0[,-c(1,32)], importance = TRUE,ntree=1000)
rf1

#OOB error 14.87

varImpPlot(rf1)

#finding threshold based on accuracy
rf1TrainPred <- predict(rf1, newdata=train0, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels0, rf1TrainPred[,2])
plot(auc, print.thres=TRUE)


#use this threshold to make predictions
threshold0 <- 0.492

rf1TestPred <- predict(rf1, newdata=testnum0[,-1], type="prob")

predicted0 <- rep("b",220156)
predicted0[rf1TestPred[,2]>=threshold0] <- "s"
weightRank0 = rf1TestPred[,2]

submission0 = data.frame(EventId = testId0, RankOrder = weightRank0, Class = predicted0)


#initial random forest jet number 1

set.seed(0)
rf2 = randomForest(Label ~ ., data = jetnum1[,-c(1,32)], importance = TRUE,ntree=1000)
rf2


#OOB error 18.41%

varImpPlot(rf2)

#use this threshold to make predictions
threshold1 <- 0.485

rf2TestPred <- predict(rf2, newdata=testnum1[,-1], type="prob")

predicted1 <- rep("b",169716)
predicted1[rf2TestPred[,2]>=threshold1] <- "s"
weightRank1 = rf2TestPred[,2]

submission1 = data.frame(EventId = testId1, RankOrder = weightRank1, Class = predicted1)


#initial random forest jet number 2-3

set.seed(0)
rf3 = randomForest(Label ~ ., data = jetnum23[,-c(1,32)], importance = TRUE,ntree=1000)
rf3

#OOB error 14.99%

varImpPlot(rf3)

#finding threshold based on accuracy
rf2TrainPred <- predict(rf2, newdata=train1, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels1, rf2TrainPred[,2])
plot(auc, print.thres=TRUE)

#finding threshold based on accuracy
rf3TrainPred <- predict(rf3, newdata=train23, type="prob")

labels <- ifelse(labels=='s', 1, 0)
auc = roc(labels23, rf3TrainPred[,2])
plot(auc, print.thres=TRUE)


#use this threshold to make predictions
threshold23 <- 0.490

rf3TestPred <- predict(rf3, newdata=testnum23[,-1], type="prob")

predicted23 <- rep("b",160128)
predicted23[rf3TestPred[,2]>=threshold23] <- "s"
weightRank23 = rf3TestPred[,2]

submission23 = data.frame(EventId = testId23, RankOrder = weightRank23, Class = predicted23)


#combine the 3 models and rank/reorder

submission = rbind(submission0,submission1,submission23)

submission$RankOrder = rank( submission$RankOrder,ties.method= "random")

write.csv(submission, "rfjetnumtree_submission2.csv", row.names=FALSE)

#reorder by rank
submission = submission[order(submission[,2]),]


#make the top 15% of rankorder s.

submission = mutate(submission, Class = ifelse(RankOrder>467500,"s","b"))

write.csv(submission, "rfjetnumtree_submission.csv", row.names=FALSE)