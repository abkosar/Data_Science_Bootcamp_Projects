#Loading important packages.
library(xgboost)
library(VIM)

setwd("~/GitHub-kszela24/higgs-bozon/Szela")

#Reading in training and testing data.
train = read.csv("training.csv")
test = read.csv("test.csv")

#Set -999's to NA for separation.
train[train == -999] = NA
test[test == -999] = NA

unique(rowSums(is.na(train)))

#Separate into 6 separate groups of NA's
train.0.NA = train[rowSums(is.na(train)) == 0,]
test.0.NA = test[rowSums(is.na(test)) == 0,]

colnames(train.0.1.NA)

train.1.NA = train[rowSums(is.na(train)) == 1,]
test.1.NA = test[rowSums(is.na(test)) == 1,]

train.7.NA = train[rowSums(is.na(train)) == 7,]
test.7.NA = test[rowSums(is.na(test)) == 7,]

train.8.NA = train[rowSums(is.na(train)) == 8,]
test.8.NA = test[rowSums(is.na(test)) == 8,]

train.10.NA = train[rowSums(is.na(train)) == 10,]
test.10.NA = test[rowSums(is.na(test)) == 10,]

train.11.NA = train[rowSums(is.na(train)) == 11,]
test.11.NA = test[rowSums(is.na(test)) == 11,]

#Removing NA columns.
train.1.NA = train.1.NA[, colSums(is.na(train.1.NA)) != nrow(train.1.NA)]
test.1.NA = test.1.NA[, colSums(is.na(test.1.NA)) != nrow(test.1.NA)]

train.7.NA = train.7.NA[, colSums(is.na(train.7.NA)) != nrow(train.7.NA)]
test.7.NA = test.7.NA[, colSums(is.na(test.7.NA)) != nrow(test.7.NA)]

train.8.NA = train.8.NA[, colSums(is.na(train.8.NA)) != nrow(train.8.NA)]
test.8.NA = test.8.NA[, colSums(is.na(test.8.NA)) != nrow(test.8.NA)]

train.10.NA = train.10.NA[, colSums(is.na(train.10.NA)) != nrow(train.10.NA)]
test.10.NA = test.10.NA[, colSums(is.na(test.10.NA)) != nrow(test.10.NA)]

train.11.NA = train.11.NA[, colSums(is.na(train.11.NA)) != nrow(train.11.NA)]
test.11.NA = test.11.NA[, colSums(is.na(test.11.NA)) != nrow(test.11.NA)]

write.csv(train.0.NA, "train_0_NA.csv", row.names = F)
write.csv(test.0.NA, "test_0_NA.csv", row.names = F)

write.csv(train.1.NA, "train_1_NA.csv", row.names = F)
write.csv(test.1.NA, "test_1_NA.csv", row.names = F)

write.csv(train.7.NA, "train_7_NA.csv", row.names = F)
write.csv(test.7.NA, "test_7_NA.csv", row.names = F)

write.csv(train.8.NA, "train_8_NA.csv", row.names = F)
write.csv(test.8.NA, "test_8_NA.csv", row.names = F)

write.csv(train.10.NA, "train_10_NA.csv", row.names = F)
write.csv(test.10.NA, "test_10_NA.csv", row.names = F)

write.csv(train.11.NA, "train_11_NA.csv", row.names = F)
write.csv(test.11.NA, "test_11_NA.csv", row.names = F)

#Old separations.
train.NA.DER_mass_MMC = train[rowSums(is.na(train['DER_mass_MMC'])) > 0,]
test.NA.DER_mass_MMC = test[rowSums(is.na(test['DER_mass_MMC'])) > 0,]

train = train[rowSums(is.na(train['DER_mass_MMC'])) == 0,]
test = test[rowSums(is.na(test['DER_mass_MMC'])) == 0,]

train.NA.PRI_jet_leading_pt = train[rowSums(is.na(train['PRI_jet_leading_pt'])) > 0,]
test.NA.PRI_jet_leading_pt = test[rowSums(is.na(test['PRI_jet_leading_pt'])) > 0,]

train = train[rowSums(is.na(train['PRI_jet_leading_pt'])) == 0,]
test = test[rowSums(is.na(test['PRI_jet_leading_pt'])) == 0,]

train.NA.DER_mass_jet_jet = train[rowSums(is.na(train['DER_mass_jet_jet'])) > 0,]
test.NA.DER_mass_jet_jet = test[rowSums(is.na(test['DER_mass_jet_jet'])) > 0,]

train.no.NA = train[rowSums(is.na(train['DER_mass_jet_jet'])) == 0,]
test.no.NA = test[rowSums(is.na(test['DER_mass_jet_jet'])) == 0,]

train.NA.DER_mass_jet_jet = train.NA.DER_mass_jet_jet[, colSums(is.na(train.NA.DER_mass_jet_jet)) != nrow(train.NA.DER_mass_jet_jet)]
test.NA.DER_mass_jet_jet = test.NA.DER_mass_jet_jet[, colSums(is.na(test.NA.DER_mass_jet_jet)) != nrow(test.NA.DER_mass_jet_jet)]

train.NA.DER_mass_MMC = train.NA.DER_mass_MMC[, colSums(is.na(train.NA.DER_mass_MMC)) != nrow(train.NA.DER_mass_MMC)]
test.NA.DER_mass_MMC = test.NA.DER_mass_MMC[,colSums(is.na(test.NA.DER_mass_MMC)) != nrow(test.NA.DER_mass_MMC)]

train.NA.PRI_jet_leading_pt = train.NA.PRI_jet_leading_pt[, colSums(is.na(train.NA.PRI_jet_leading_pt)) != nrow(train.NA.PRI_jet_leading_pt)]
test.NA.PRI_jet_leading_pt = test.NA.PRI_jet_leading_pt[, colSums(is.na(test.NA.PRI_jet_leading_pt)) != nrow(test.NA.PRI_jet_leading_pt)]

#Save each of the four separate groups to a csv for further processing.
write.csv(train.no.NA, "train_no_NA.csv", row.names = F)
write.csv(test.no.NA, "test_no_NA.csv", row.names = F)

write.csv(train.NA.DER_mass_jet_jet, "train_NA_DER_mass_jet_jet.csv", row.names = F)
write.csv(test.NA.DER_mass_jet_jet, "test_NA_DER_mass_jet_jet.csv", row.names = F)

write.csv(train.NA.PRI_jet_leading_pt, "train_NA_PRI_jet_leading_pt.csv", row.names = F)
write.csv(test.NA.PRI_jet_leading_pt, "test_NA_PRI_jet_leading_pt.csv", row.names = F)

write.csv(train.NA.DER_mass_MMC, "train_NA_DER_mass_MMC.csv", row.names = F)
write.csv(test.NA.DER_mass_MMC, "test_NA_DER_mass_MMC.csv", row.names = F)