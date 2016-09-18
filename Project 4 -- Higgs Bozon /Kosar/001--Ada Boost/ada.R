library(ada)

#Reading in the files
training <- read.csv("~/to_server/training.csv", stringsAsFactors=FALSE)
test <- read.csv("~/to_server/test.csv", stringsAsFactors=FALSE)
test_new_feature <- read.csv("~/to_server/test_stacked_avg.csv", stringsAsFactors=FALSE)
training_new_feature <- read.csv("~/to_server/train_stacked_avg.csv", stringsAsFactors=FALSE)

# #Removing phi features
# training = training[, -c(17, 20, 22, 27, 30)]
# test = test[, -c(17, 20, 22, 27, 30)]


eventid = training$EventId
weight = training$Weight
training$EventId = NULL
training$Weight = NULL

#Adding the new features
training$newfeature = training_new_feature$preds
test$newfeature = test_new_feature$preds

#Training ada model on the whole training set
ada_model = ada(Label ~ ., data = training, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 1)
ada_model_loss_exponential = ada(Label ~ ., data = training, verbose = TRUE, loss = "exponential", type = "real", iter = 50, nu = 0.05)
 ada_model_loss_logistic = ada(Label ~ ., data = training, verbose = TRUE, loss = "logistic", type = "real", 
                               iter = 50, nu = 0.05,
                               bag.frac = 2)


#Making the predictions on the test set
ypred = predict(ada_model_loss_exponential, test[,-1], type = "prob")

#Creating the submission file
submission = as.data.frame(ypred)
submission$EventId = test$EventId
submission$V1 = NULL
submission_sorted = submission[order(submission$V2),]
submission_sorted$RankOrder = 1:length(submission_sorted$V2)

threshold = length(submission_sorted$V2) - as.integer(0.15*length(submission_sorted$V2))
 
submission_sorted$Class = ifelse(submission_sorted$RankOrder <= threshold, "b", "s")
submission_sorted$V2 = NULL

write.csv(submission_sorted, "ada_model_nu_0.05_loss_exponential.csv", row.names = FALSE)


varplot(ada_model_with_phi, plot.it = TRUE, type = "scores")
varplot(ada_model_loss_exponential, plot.it = TRUE, type = "scores")




#__________________________CV for adaboost__________________________
#nu = 1, iter = 50 > AVG AMS = 2.52493888789
#nu = 5, iter = 50 > AVG AMS = 0.901745966206
#nu = 0.1, iter = 50 > AVG AMS = 2.56597832629
#nu = 0.05, iter = 50 > AVG AMS = 2.58215554548
#nu = 0.01, iter = 50 > AVG AMS = 2.48066181967
#nu = 0.05, iter = 100 > AVG AMS = 2.56516010416
#loss = logistic
#nu = 0.05, iter = 50 > AVG AMS = 2.57471235065
#bag.frac = 0.5
#nu = 0.05, iter = 50 > AVG AMS = 2.5722500734
#bag.frac = 2
#nu = 0.05, iter = 50 > AVG AMS = 2.57692706437
#bag.frac = 10 > AVG AMS = 2.57692706437

#scp akosar@216.230.228.88:~/to_server/ada_model_nu_0.05_loss_logistic.csv /Users/ardakosar/Desktop


