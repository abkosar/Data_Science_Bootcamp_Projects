library(xgboost)
library(caret)

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Split_NAs/Combining")

#Reading in predictions
preds_0_na = as.data.frame(read.csv("predictions_0_NA.csv"))
preds_1_na = as.data.frame(read.csv("predictions_1_NA.csv"))
preds_7_na = as.data.frame(read.csv("predictions_7_NA.csv"))
preds_8_na = as.data.frame(read.csv("predictions_8_NA.csv"))
preds_10_na = as.data.frame(read.csv("predictions_10_NA.csv"))
preds_11_na = as.data.frame(read.csv("predictions_11_NA.csv"))

#Combining predictions into a single data frame.
preds_total = rbind(preds_0_na, preds_1_na, preds_7_na, preds_8_na, preds_10_na, preds_11_na)

#Sorting the predictions from lowest to highest.
preds_sorted = preds_total[order(preds_total$predictions), ]

#Ranking our predictions
preds_sorted$RankOrder = 1:550000

threshold = 550000 - as.integer(0.15 * 550000)

#Classifying our predictions
preds_sorted$Class = ifelse(preds_sorted$RankOrder <= threshold, "b", "s")

#Getting rid of predictions and saving to a csv.
preds_sorted$predictions = NULL

write.csv(preds_sorted, "submission_6_correct.csv", row.names = F)
