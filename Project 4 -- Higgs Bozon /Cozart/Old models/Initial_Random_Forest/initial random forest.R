#Loading important packages.
library(randomForest)
library(dplyr)


setwd("C:/Users/Hayes/Desktop/BDS 005/Projects/Project 4")

#data preperation
dfTrain <- read.csv('training.csv', header=T)
dfTest <- read.csv('test.csv', header=T)
testId = dfTest$EventId

# Convert PRI_jet_num to factor
dfTrain$PRI_jet_num <- as.factor(dfTrain$PRI_jet_num)
dfTest$PRI_jet_num <- as.factor(dfTest$PRI_jet_num)

str(dfTrain)

weight <- dfTrain$Weight
labels <- dfTrain$Label

train <- dfTrain[, -c(1,32,33)]
test <- dfTest[,-1]


#random forest



#initial random forest full data
set.seed(0)
rf = randomForest(Label ~ ., data = dfTrain[,-c(1,32)], importance = TRUE)
rf


#variable importance plot
varImpPlot(rf)

#creating submission file

rfTestPred <- predict(rf, newdata=test, type="prob")

predicted <- rep("b",550000)
weightRank = rank(rfTestPred[,2], ties.method= "random")

submission = data.frame(EventId = testId, RankOrder = weightRank, Class = predicted)

write.csv(submission, "rf_submission2.csv", row.names=FALSE)

#reorder by rank
submission = submission[order(submission[,2]),]

#make the top 15% of rankorder s.

submission = mutate(submission, Class = ifelse(RankOrder>467500,"s","b"))

write.csv(submission, "rf_submission.csv", row.names=FALSE)



