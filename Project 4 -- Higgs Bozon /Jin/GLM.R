library(VIM)
library(car)
library(psych)
library(mice)
library(doMC)
library(caret)
library(dplyr)
library(tree)
library(MASS)
library(corrplot)
training <- read.csv('./data/training.csv', header=T)
test <- read.csv('./data/test.csv', header=T)


#Some basic exploratory functions
summary(training)
head(training)
sapply(training,sd)
cor_train = cor(training[,-33])


#Visualizing the missingness
aggr(training)

#Splitting the training set into three different dataset according to the missing values
train_prijet_0 = training[training$PRI_jet_num == 0,]
train_prijet_1 = training[training$PRI_jet_num == 1,]
train_prijet_2_3 = training[training$PRI_jet_num == 2 | training$PRI_jet_num == 3,]

#Splitting the test set into three different dataset according to the missing values
test_prijet_0 = test[test$PRI_jet_num == 0,]
test_prijet_1 = test[test$PRI_jet_num == 1,]
test_prijet_2_3 = test[test$PRI_jet_num == 2 | test$PRI_jet_num == 3,]


train_prijet_0 = train_prijet_0[,-c(6:8)]
train_prijet_0 = train_prijet_0[,-11]
train_prijet_0 = train_prijet_0[,-c(21:26)]#######################Dropped the -999 columns
train_prijet_0 = train_prijet_0[,-c(20:21)]#######################Dropped the columns which I used as a factor (they include all 0
train_prijet_0$DER_mass_MMC[train_prijet_0$DER_mass_MMC == -999] = 0.0001 

train_prijet_1 = train_prijet_1[,-c(6:8)]
train_prijet_1 = train_prijet_1[,-11]
train_prijet_1 = train_prijet_1[,-c(24:26)]
train_prijet_1 = train_prijet_1[,-20]

train_prijet_2_3 = train_prijet_2_3[,-24]
train_prijet_2_3$DER_mass_MMC[train_prijet_2_3$DER_mass_MMC == -999] = 0.0001 



test_prijet_0 = test_prijet_0[,-c(6:8)]
test_prijet_0 = test_prijet_0[,-11]
test_prijet_0 = test_prijet_0[,-c(21:26)]#######################Dropped the -999 columns
test_prijet_0 = test_prijet_0[,-c(20:21)]#######################Dropped the columns which I used as a factor (they include all 0
test_prijet_0$DER_mass_MMC[test_prijet_0$DER_mass_MMC == -999] = 0.0001 

test_prijet_1 = test_prijet_1[,-c(6:8)]
test_prijet_1 = test_prijet_1[,-11]
test_prijet_1 = test_prijet_1[,-c(24:26)]#######################Dropped the -999 columns
test_prijet_1 = test_prijet_1[,-20]#######################Dropped the columns which I used as a factor (they include all 0
test_prijet_1$DER_mass_MMC[test_prijet_1$DER_mass_MMC == -999] = 0.0001 



#PCA for training set where PRI_Jet_Num = 0
fa.parallel(train_prijet_0[-21], #The data in question.
            fa = "pc", #Display the eigenvalues for PCA.
            n.iter = 10)

pc_train_pri_jet_0 = principal(train_prijet_0[-21], #The data in question.
                               nfactors = 7, #The number of PCs to extract.
                               rotate = "none")

pc_train_pri_jet_0

#PC1 highly correlates with DER_mass_MMC, DER_mass_vis, DER_sum_pt, PRI_tau_pt
#PC2 highly correlates with DER_mass_transverse_met_lep, DER_pt_ratio_lep_tau, PRI_lep_pt
#PC3 highly correlates with DER_pt_h, DER_pt_tot
#PC4 does not seem to highly correlate with anything (everything less than .6)
#PC5 highly correlates with PRI_tau_eta, PRI_lep_eta
#PC6 highly correlates with PRI_tau_phi / negatively correlates with PRI_lep_phi
#PC7 highly correlates with PRI_met_phi

'''
Principal Components Analysis
Call: principal(r = train_prijet_0[-21], nfactors = 7, rotate = "none")
Standardized loadings (pattern matrix) based upon correlation matrix
PC1   PC2   PC3   PC4   PC5   PC6   PC7    h2    u2 com
EventId                      0.00  0.00  0.00  0.00  0.02  0.01 -0.10 0.011 0.989 1.1
DER_mass_MMC                 0.74 -0.18 -0.28  0.37 -0.04  0.00 -0.01 0.793 0.207 1.9
DER_mass_transverse_met_lep -0.07  0.93  0.12 -0.03  0.02  0.00  0.00 0.896 0.104 1.1
DER_mass_vis                 0.81  0.18 -0.28  0.07  0.00  0.00 -0.01 0.780 0.220 1.4
DER_pt_h                     0.27 -0.08  0.89  0.23 -0.03 -0.01  0.00 0.924 0.076 1.3
DER_deltar_tau_lep           0.57 -0.23 -0.40  0.50 -0.04 -0.01 -0.01 0.788 0.212 3.2
DER_pt_tot                   0.27 -0.08  0.89  0.23 -0.03 -0.01  0.00 0.924 0.076 1.3
DER_sum_pt                   0.83  0.33 -0.09 -0.36  0.03  0.00  0.01 0.935 0.065 1.7
DER_pt_ratio_lep_tau        -0.09  0.70 -0.04  0.60 -0.03  0.00  0.00 0.865 0.135 2.0
DER_met_phi_centrality       0.25 -0.36  0.39  0.39 -0.04 -0.01 -0.01 0.498 0.502 3.7
PRI_tau_pt                   0.70 -0.14 -0.03 -0.66  0.04  0.00  0.00 0.947 0.053 2.1
PRI_tau_eta                  0.01 -0.03  0.00  0.08  0.88  0.02 -0.01 0.775 0.225 1.0
PRI_tau_phi                 -0.01  0.00  0.00 -0.01 -0.02  0.67 -0.60 0.806 0.194 2.0
PRI_lep_pt                   0.61  0.69 -0.11  0.11  0.00  0.00  0.01 0.859 0.141 2.1
PRI_lep_eta                  0.02 -0.03  0.01  0.08  0.88  0.02  0.00 0.775 0.225 1.0
PRI_lep_phi                  0.00  0.00 -0.01 -0.01  0.02 -0.85 -0.03 0.722 0.278 1.0
PRI_met                     -0.08  0.63  0.52 -0.33  0.04  0.00  0.00 0.775 0.225 2.5
PRI_met_phi                  0.01 -0.02  0.01  0.01  0.00  0.46  0.82 0.880 0.120 1.6
PRI_met_sumet                0.54 -0.02  0.43 -0.11  0.00  0.01 -0.01 0.491 0.509 2.0
Weight                      -0.27  0.56 -0.22  0.20 -0.01  0.01  0.00 0.471 0.529 2.1

PC1  PC2  PC3  PC4  PC5  PC6  PC7
SS loadings           3.68 2.93 2.59 1.75 1.55 1.38 1.03
Proportion Var        0.18 0.15 0.13 0.09 0.08 0.07 0.05
Cumulative Var        0.18 0.33 0.46 0.55 0.62 0.69 0.75
Proportion Explained  0.25 0.20 0.17 0.12 0.10 0.09 0.07
Cumulative Proportion 0.25 0.44 0.62 0.73 0.84 0.93 1.00

Mean item complexity =  1.8
Test of the hypothesis that 7 components are sufficient.

The root mean square of the residuals (RMSR) is  0.05 
with the empirical chi square  104949.6  with prob <  0 

Fit based upon off diagonal values = 0.95
'''

factor.plot(pc_train_pri_jet_0)


######################################################
##########GENERALIZED MODEL WITH FORWARD AIC##########
######################################################

#__________For Prijet_Num_0__________

train_logistic = train_prijet_0[,-c(1,20)]
levels(train_logistic$Label) = c(0,1)
train_logistic$Label = as.numeric(train_logistic$Label) - 1

abc = train_logistic[,-19]

corrplot(cor(abc), order="hclust")

#standardize variables by scaling data between 0 and 1
normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

train_logistic = as.data.frame(lapply(train_logistic, normalize))

summary(train_logistic)


#Building the models for stepwise regression
model.empty.prijet_0 = glm(Label ~ 1, family = "binomial", data = train_logistic)
model.full.prijet_0 = glm(Label ~ ., family = "binomial", data = train_logistic)
scope = list(lower = formula(model.empty.prijet_0), upper = formula(model.full.prijet_0))

pchisq(model.full.prijet_0$deviance, model.full.prijet_0$df.residual, lower.tail = FALSE)
'''
[1] 1
'''
#variable selection AIC
forwardAIC = step(model.empty.prijet_0, scope, direction = "forward", k = 2) #AIC = 78217
'''
Step:  AIC=78216.72
Label ~ DER_mass_transverse_met_lep + PRI_tau_pt + DER_deltar_tau_lep + 
DER_mass_vis + DER_sum_pt + DER_pt_ratio_lep_tau + PRI_met_sumet + 
PRI_met + DER_mass_MMC + PRI_lep_eta + PRI_met_phi

Df Deviance   AIC
<none>                         78193 78217
+ DER_pt_h                1    78191 78217
+ DER_pt_tot              1    78191 78217
+ DER_met_phi_centrality  1    78193 78219
+ PRI_tau_eta             1    78193 78219
+ PRI_lep_phi             1    78193 78219
+ PRI_lep_pt              1    78193 78219
+ PRI_tau_phi             1    78193 78219
'''

summary(forwardAIC)
plot(forwardAIC)


logit.new = glm(Label ~ DER_mass_transverse_met_lep + PRI_tau_pt + DER_deltar_tau_lep + 
                  DER_mass_vis + DER_sum_pt + DER_pt_ratio_lep_tau + PRI_met_sumet + 
                  PRI_met + DER_mass_MMC + PRI_lep_eta + PRI_met_phi,
                family = "binomial",
                data = train_logistic)
vif(logit.new)

#remove PRI_tau_pt
logit.new_j = glm(Label ~ DER_mass_transverse_met_lep + DER_deltar_tau_lep + 
                  DER_mass_vis + DER_sum_pt + DER_pt_ratio_lep_tau + PRI_met_sumet + 
                  PRI_met + DER_mass_MMC + PRI_lep_eta + PRI_met_phi,
                family = "binomial",
                data = train_logistic)
vif(logit.new_j)

anova(logit.new, model.full.prijet_0, test = "Chisq")

anova(logit.new_j, model.full.prijet_0, test = "Chisq")
#reject model

#full model
train_logistic.predicted = round(model.full.prijet_0$fitted.values)

table(truth = train_logistic$Label, prediction = train_logistic.predicted)
'''
     prediction
truth     0     1
0 68302  6119
1 11318 14174
'''
(68302+14174)/(68302+14174+6119+11318)
'''
[1] 0.8254782
'''
#82.5% accuracy

#reduced model
train_logistic.predicted_new = round(logit.new$fitted.values)

table(truth = train_logistic$Label, prediction = train_logistic.predicted_new)
'''
     prediction
truth     0     1
0     68322  6099
1     11332 14160
'''
(68322+14160)/(68322+14160+11332+6099)
'''
[1] 0.8255382
'''

(11332+14160)/(68322+14160+11332+6099)



#Making predictions
logit.predict.prijet_0 = predict(forwardAIC, test_prijet_0, interval = "prediction")

predictions.logit.prijet_0 = data.frame(test_prijet_0$EventId, logit.predict.prijet_0)
colnames(predictions.logit.prijet_0) = c("EventId", "Label")

write.csv(predictions.logit.prijet_0, file = "Pred_Logit_Prijet_0.csv")

#__________For Prijet_Num_1__________

train_logistic_prijet_1 = train_prijet_1[,-c(1,24)]
levels(train_logistic_prijet_1$Label) = c(0,1)
train_logistic_prijet_1$Label = as.numeric(train_logistic_prijet_1$Label) - 1

#Building the models for stepwise regression
model.empty.prijet_1 = glm(Label ~ 1, family = "binomial", data = train_logistic_prijet_1)
model.full.prijet_1 = glm(Label ~ ., family = "binomial", data = train_logistic_prijet_1)
scope = list(lower = formula(model.empty.prijet_1), upper = formula(model.full.prijet_1))

forwardAIC_prijet_1 = step(model.empty.prijet_1, scope, direction = "forward", k = 2) 
summary(forwardAIC_prijet_1) #AIC = 83792

'''
Step:  AIC=83791.6
Label ~ DER_mass_transverse_met_lep + PRI_tau_pt + DER_met_phi_centrality + 
DER_deltar_tau_lep + DER_mass_vis + DER_sum_pt + DER_pt_tot + 
PRI_lep_pt + DER_pt_ratio_lep_tau + DER_mass_MMC + PRI_met_sumet + 
PRI_met + PRI_lep_eta
'''

logit.new_1 = glm(Label ~ DER_mass_transverse_met_lep + PRI_tau_pt + DER_met_phi_centrality + 
                    DER_deltar_tau_lep + DER_mass_vis + DER_sum_pt + DER_pt_tot + 
                    PRI_lep_pt + DER_pt_ratio_lep_tau + DER_mass_MMC + PRI_met_sumet + 
                    PRI_met + PRI_lep_eta,
                family = "binomial",
                data = train_logistic)

train_logistic_1.predicted = round(model.full.prijet_1$fitted.values)

table(truth = train_logistic_prijet_1$Label, prediction = train_logistic_1.predicted)
'''
     prediction
truth     0     1
0 41679  8155
1 14111 13599
'''
(41679+13599)/(41679+13599+14111+8155)
'''
[1] 0.7128598
'''

train_logistic_1.predicted_new = round(logit.new_1$fitted.values)

table(truth = train_logistic_prijet_1$Label, prediction = train_logistic_1.predicted_new)



logit.predict.prijet_1 = predict(forwardAIC_prijet_1, test_prijet_1, interval = "prediction")

predictions.logit.prijet_1 = data.frame(test_prijet_1$EventId, logit.predict.prijet_1)
colnames(predictions.logit.prijet_1) = c("EventId", "Label")

write.csv(predictions.logit.prijet_1, file = "Pred_Logit_Prijet_1.csv")

#__________For Prijet_Num_2_3__________

train_logistic_prijet_2_3 = train_prijet_2_3[,-c(1,31)]
levels(train_logistic_prijet_2_3$Label) = c(0,1)
train_logistic_prijet_2_3$Label = as.numeric(train_logistic_prijet_2_3$Label) - 1

#Building the models for stepwise regression
model.empty.prijet_2_3 = glm(Label ~ 1, family = "binomial", data = train_logistic_prijet_2_3)
model.full.prijet_2_3 = glm(Label ~ ., family = "binomial", data = train_logistic_prijet_2_3)
scope = list(lower = formula(model.empty.prijet_2_3), upper = formula(model.full.prijet_2_3))

forwardAIC_prijet_2_3 = step(model.empty.prijet_2_3, scope, direction = "forward", k = 2) 
summary(forwardAIC_prijet_2_3) #AIC = 76366

'''
Step:  AIC=76365.97
Label ~ DER_mass_jet_jet + DER_mass_transverse_met_lep + DER_lep_eta_centrality + 
DER_met_phi_centrality + PRI_jet_all_pt + PRI_tau_pt + PRI_met + 
DER_deltar_tau_lep + DER_pt_h + DER_mass_vis + PRI_met_sumet + 
PRI_lep_pt + DER_pt_ratio_lep_tau + DER_deltaeta_jet_jet + 
PRI_jet_subleading_pt + DER_mass_MMC + DER_pt_tot + DER_prodeta_jet_jet + 
PRI_tau_eta + PRI_jet_leading_pt
'''

logit.predict.prijet_2_3 = predict(forwardAIC_prijet_2_3, test_prijet_2_3, interval = "prediction")

predictions.logit.prijet_2_3 = data.frame(test_prijet_2_3$EventId, logit.predict.prijet_2_3)
colnames(predictions.logit.prijet_2_3) = c("EventId", "Label")

write.csv(predictions.logit.prijet_2_3, file = "Pred_Logit_Prijet_2_3.csv")


#Combining the results for submission
total = rbind(predictions.logit.prijet_0, predictions.logit.prijet_1, predictions.logit.prijet_2_3)
total_sorted = total[order(total$Label),] #sorting the results

total_sorted$RankOrder = 1:length(total_sorted$Label) #creating the RankOrder column

threshold = length(total_sorted$Label) - as.integer(0.15*length(total_sorted$Label)) #setting the threshold of 15%

total_sorted$Class = ifelse(total_sorted$RankOrder <= threshold, "b", "s") 
total_sorted$Label = NULL

write.csv(total_sorted, file = "model_AIC.csv", row.names = FALSE)




