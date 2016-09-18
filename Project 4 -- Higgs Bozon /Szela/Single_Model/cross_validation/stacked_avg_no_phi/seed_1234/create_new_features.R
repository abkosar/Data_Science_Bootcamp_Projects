setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation/stacked_avg_no_phi/seed_0")
the_seed = 0
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

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation/stacked_avg_no_phi/seed_1111")
the_seed = 1111
train.cv.extra_1111 = rbind(as.data.frame(read.csv(paste0("stack_1_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_2_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_3_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_4_", the_seed, "_train.csv"))),
                       as.data.frame(read.csv(paste0("stack_5_", the_seed, "_train.csv"))))

test.preds1_1111 = as.data.frame(read.csv(paste0("stack_1_", the_seed, "_test.csv")))
test.preds2_1111 = as.data.frame(read.csv(paste0("stack_2_", the_seed, "_test.csv")))
test.preds3_1111 = as.data.frame(read.csv(paste0("stack_3_", the_seed, "_test.csv")))
test.preds4_1111 = as.data.frame(read.csv(paste0("stack_4_", the_seed, "_test.csv")))
test.preds5_1111 = as.data.frame(read.csv(paste0("stack_5_", the_seed, "_test.csv")))

test.preds.avg_1111 = (test.preds1_1111$predictions + test.preds2_1111$predictions + test.preds3_1111$predictions + 
                    test.preds4_1111$predictions + test.preds5_1111$predictions) / 5

setwd("~/GitHub-kszela24/higgs-bozon/Szela/Single_Model/cross_validation/stacked_avg_no_phi/seed_1234")
the_seed = 1234
train.cv.extra_1234 = rbind(as.data.frame(read.csv(paste0("stack_1_", the_seed, "_train.csv"))),
                            as.data.frame(read.csv(paste0("stack_2_", the_seed, "_train.csv"))),
                            as.data.frame(read.csv(paste0("stack_3_", the_seed, "_train.csv"))),
                            as.data.frame(read.csv(paste0("stack_4_", the_seed, "_train.csv"))),
                            as.data.frame(read.csv(paste0("stack_5_", the_seed, "_train.csv"))))

test.preds1_1234 = as.data.frame(read.csv(paste0("stack_1_", the_seed, "_test.csv")))
test.preds2_1234 = as.data.frame(read.csv(paste0("stack_2_", the_seed, "_test.csv")))
test.preds3_1234 = as.data.frame(read.csv(paste0("stack_3_", the_seed, "_test.csv")))
test.preds4_1234 = as.data.frame(read.csv(paste0("stack_4_", the_seed, "_test.csv")))
test.preds5_1234 = as.data.frame(read.csv(paste0("stack_5_", the_seed, "_test.csv")))

test.preds.avg_1234 = (test.preds1_1234$predictions + test.preds2_1234$predictions + test.preds3_1234$predictions + 
                         test.preds4_1234$predictions + test.preds5_1234$predictions) / 5

train.cv.extra = train.cv.extra[order(train.cv.extra$EventId), ]
train.cv.extra_1111 = train.cv.extra_1111[order(train.cv.extra_1111$EventId), ]
train.cv.extra_1234 = train.cv.extra_1234[order(train.cv.extra_1234$EventId), ]

train_stacked_avg = data.frame(EventId = train.cv.extra$EventId,
                               preds = ((train.cv.extra$predictions + 
                                           train.cv.extra_1111$predictions + 
                                           train.cv.extra_1234$predictions) / 3))
test_stacked_avg_pred = (test.preds1$predictions + test.preds2$predictions + test.preds3$predictions + 
  test.preds4$predictions + test.preds5$predictions + test.preds1_1111$predictions + 
  test.preds2_1111$predictions + test.preds3_1111$predictions + test.preds4_1111$predictions + 
  test.preds5_1111$predictions + test.preds1_1234$predictions + test.preds2_1234$predictions + 
  test.preds3_1234$predictions + test.preds4_1234$predictions + test.preds5_1234$predictions) / 15

test_stacked_avg = data.frame(EventId = test.preds1$EventId, 
                              preds = test_stacked_avg_pred)

write.csv(test_stacked_avg, "test_stacked_avg.csv", row.names = F)
write.csv(train_stacked_avg, "train_stacked_avg.csv", row.names = F)

