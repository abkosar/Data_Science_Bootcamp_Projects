setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation/stacked_avg_no_phi")

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

stacked_preds_0 = as.data.frame(read.csv("predictions_stacked_0_test.csv"))
stacked_preds_1111 = as.data.frame(read.csv("predictions_stacked_1111_test.csv"))
stacked_preds_1234 = as.data.frame(read.csv("predictions_stacked_1234_test.csv"))
rf_preds = as.data.frame(read.csv("rfjetnum_Reducedfeature_pred.csv"))

preds_0_sorted = stacked_preds_0[order(stacked_preds_0$EventId), ]
preds_1111_sorted = stacked_preds_1111[order(stacked_preds_1111$EventId), ]
preds_1234_sorted = stacked_preds_1234[order(stacked_preds_1234$EventId), ]

preds = (preds_0_sorted$predictions + preds_1111_sorted$predictions + preds_1234_sorted$predictions) / 3

ada_frame = as.data.frame(read.csv("to_kyle_adaboost.csv"))
summary(ada_frame)

preds_normalized = range01(preds)
summary(preds_normalized)
preds = (preds_normalized * 1.01) + (rf_preds$RankOrder * 0) + (ada_frame$Predictions * 0.07)

summary(preds)
preds_sorted = data.frame(EventId = preds_0_sorted$EventId,
                          predictions = preds)
submission_sorted = preds_sorted[order(preds_sorted$predictions), ]

#Sorting the predictions from lowest to highest.
submission_sorted$RankOrder = 1:550000

threshold = 550000 - as.integer(0.15 * 550000)

submission_sorted$Class = ifelse(submission_sorted$RankOrder <= threshold, "b", "s")
save_predictions = submission_sorted$predictions
submission_sorted$predictions = NULL

write.csv(submission_sorted, "submission_stacked_avg_no_phi_220_rounds.csv", row.names = F)

