library(e1071)
library(caret)
library(doMC)
library(ada)
library(corrplot)
registerDoMC(cores = 2)

training = as.data.frame(read.csv("training.csv"))
test = as.data.frame(read.csv("test.csv"))

summary(training)
plot(training$DER_deltar_tau_lep, training$PRI_met, col = training$Label)


#Correlation plot for the training set
training$Label = as.numeric(training$Label) -1 
M <- cor(training[,-c(1,32,33)])
corrplot(M, method="circle")


set.seed(0)
idx = createFolds(training$Label, k = 20, list = TRUE, returnTrain = FALSE)
idx = as.data.frame(idx)



proc = proc.time()

foreach(i=1:length(idx)) %dopar% {

  training.label = label[idx[[i]]]
  training.subset = training[idx[[i]],]

  svm_model = svm(training.subset, training.label, type = "C-classification", kernel = "linear",
                  probability = TRUE,
                  cost = )

  predictions = predict(svm_model, test[,-1], probability = TRUE)

  pred_prob = attr(predictions, "probabilities")

  submission = data.frame(EventId = test$EventId, Label = pred_prob)

  csv_file_name = paste0("prediction_", i, "th_fold.csv")

  write.csv(submission, csv_file_name, row.names = FALSE)


}

proc.time() - proc

prediction_total = data.frame(matrix(0, nrow=550000, ncol=20))

for (i in 1:length(idx)) {

  csv_file_read = paste0("prediction_", i, "th_fold.csv")
  svm_fold_i = read.csv(csv_file_read, header = TRUE)
  prediction_total[i] = svm_fold_i[,3]
  print(i)

}

prediction_total$EventId = test$EventId

RowM <- rowMeans(prediction_total[, -21])

submission = data.frame(EventId = test$EventId, Label = RowM)

submission_sorted = submission[order(submission$Label),]
submission_sorted$RankOrder = 1:length(submission_sorted$Label)

threshold = length(submission_sorted$Label) - as.integer(0.15*length(submission_sorted$Label))

submission_sorted$Class = ifelse(submission_sorted$RankOrder <= threshold, "b", "s")
submission_sorted$Label = NULL

write.csv(submission_sorted, file = "model_svm_20_folds.csv", row.names = FALSE)




#__________________SVM Model with Cost = 5__________________


training$Label = as.numeric(training$Label) -1 

label = training$Label

set.seed(0)
idx = createFolds(training$Label, k = 20, list = TRUE, returnTrain = FALSE)
idx = as.data.frame(idx)

foreach(i=1:length(idx)) %dopar% {
  
  training.label = label[idx[[i]]]  
  validation_set.label = label[-idx[[i]]] #Labels
  training.subset = training[idx[[i]],] #Subsetting the training set with idx
  validation_set = training[-idx[[i]],] #Selecting the remaining training set as a test set
  
  
  train_label <- as.vector(training.subset$Label)
  training.subset$Label = NULL
  
  validation_set_label <- as.vector(validation_set$Label)
  validation_set$Label = NULL
  
  train_ID <- as.vector(training.subset$EventId)
  training.subset$EventId = NULL
  
  validation_set_ID <- as.vector(validation_set$EventId)
  validation_set$EventId = NULL
  
  train_Weight <- as.vector(training.subset$Weight)
  training.subset$Weight = NULL
  
  train_test_Weight <- as.vector(validation_set$Weight)
  validation_set$Weight = NULL
  
  test_ID <- as.vector(test$EventId)
  test$EventId = NULL
  
  
  svm_model_with_cost = svm(training.subset, training.label, type = "C-classification", kernel = "linear",
                   probability = TRUE,
                   cost = 5)
  
  predictions_validation_set = predict(svm_model_with_cost, validation_set, probability = TRUE)
  predictions_test_set = predict(svm_model_with_cost, test, probability = TRUE)
  
  prediction_prob_validation_set = attr(predictions_validation_set, "probabilities")
  prediction_prob_test_set = attr(predictions_test_set, "probabilities")
  
  
  submission_validation_set = data.frame(EventId = validation_set_ID, Label = prediction_prob_validation_set)
  submission_test_set = data.frame(EventId = test_ID, Label = prediction_prob_test_set)
  
  
  csv_file_validation = paste0("prediction_fold", i, "_validation_set.csv")
  csv_file_test = paste0("prediction_fold", i, "_test_set.csv")
  
  write.csv(submission_validation_set, csv_file_validation, row.names = FALSE)
  write.csv(submission_test_set, csv_file_test, row.names = FALSE)

}


prediction_train = data.frame(matrix(0, nrow=250000, ncol=20))
prediction_train$EventId = training$EventId
prediction_test = data.frame(matrix(0, nrow=550000, ncol=20))

for (i in 1:length(idx)) {
     
     csv_file_read_validation_set = paste0("prediction_fold", i, "_validation_set.csv")
     svm_fold_validation = read.csv(csv_file_read_validation_set, header = TRUE)
     print(i)
     
     for (id in svm_fold_validation$EventId) {
       prediction_train[prediction_train$EventId == id, i] = svm_fold_validation[svm_fold_validation$EventId == id, 3]
     }
     

     csv_file_read_test_set = paste0("prediction_fold", i, "_test_set.csv")
     svm_fold_test = read.csv(csv_file_read_validation_set, header = TRUE)
     
     prediction_test[i] = svm_fold_test[,3]
     
     
   }


csv_file_read_validation_set = paste0("prediction_fold", i, "_validation_set.csv")
svm_fold_validation = read.csv(csv_file_read_validation_set, header = TRUE)
print(i)

for (id in svm_fold_validation$EventId) {
  prediction_train[prediction_train$EventId == id, 1] = svm_fold_validation[svm_fold_validation$EventId == id, 3]
}


csv_file_read_test_set = paste0("prediction_fold", i, "_test_set.csv")
svm_fold_test = read.csv(csv_file_read_validation_set, header = TRUE)

prediction_test[i] = svm_fold_test[,3]

################################################

submission = read.csv("test_svm_cost_5.csv", header = TRUE)

submission$EventId = test$EventId

submission_sorted = submission[order(submission$proba_s),]
submission_sorted$RankOrder = 1:length(submission_sorted$proba_s)

submission_sorted = submission_sorted[,-c(1:30)]
submission_sorted = submission_sorted[,-1]
colnames(submission_sorted) = c("probability", "Class", "EventId", "RankOrder")
submission_sorted$RankOrder = 1:length(submission_sorted$Label)


submission_final = submission_sorted[,c(2,1,3)]
write.csv(submission_final,"submission_svm_cost_5.csv", row.names = FALSE)















