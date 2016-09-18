#1.1 reading csv file
abl=read.csv("~/classnotes/week7/[14] Abalone.csv")

normalize = function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}

#We now apply our normalization function to all the variables within our dataset;
#we store the result as a data frame for future manipulation.
abl_norm = as.data.frame(lapply(abl, normalize))
summary(abl_norm)
View(abl)

set.seed(0)
nrow(abl_norm)*.80
abl_test=abl_norm[1:3342,]   # 80 percent Test !!!! only for this example
abl_train=abl_norm[3343:4177,]   # 20 percent Train!!! only for this example

#1.3


formula = as.formula(paste("Rings ~",paste(names(abl[,-8]), collapse = " + ")))
formula
library("neuralnet")

abl_net_basic=neuralnet(formula = formula,data=abl_train,hidden = 1)
abl_net_moderate=neuralnet(formula = formula,data=abl_train,hidden=25)
abl_net_complex=neuralnet(formula = formula,data=abl_train,hidden=50)
abl_net_mul_1=neuralnet(formula = formula,data=abl_train,hidden=c(10,10,10))
abl_net_mul_2=neuralnet(formula = formula,data=abl_train,hidden=c(10,5,2))
abl_net_mul_3=neuralnet(formula = formula,data=abl_train,hidden=c(5,5,5))




#1.5
#Plotting
plot(abl_net_basic)
plot(abl_net_moderate)
plot(abl_net_complex)
plot(abl_net_mul_1)
plot(abl_net_mul_2)
plot(abl_net_mul_3)

#Record Training Error
abl_results_basic = compute(abl_net_basic,abl_test[,1:7])
abl_results_moderate = compute(abl_net_moderate,abl_test[,1:7])
abl_results_complex = compute(abl_net_complex,abl_test[,1:7])
abl_results_mul_1 = compute(abl_net_mul_1,abl_test[,1:7])
abl_results_mul_2 = compute(abl_net_mul_2,abl_test[,1:7])
abl_results_mul_3= neuralnet::compute(abl_net_mul_3,abl_test[,1:7])






predicted_basic_Rings=abl_results_basic$net.result
predicted_moderate_Rings=abl_results_moderate$net.result
predicted_complex_Rings=abl_results_complex$net.result
predicted_mul_1_Rings=abl_results_mul_1$net.result
predicted_mul_2_Rings=abl_results_mul_2$net.result
predicted_mul_3_Rings=abl_results_mul_3$net.result






cor(predicted_basic_Rings,abl_test$Rings)

summary(abl_net_basic)
# test error
abl_net_basic_test_error=0.5*sum((predicted_basic_Rings - abl_test$Rings)^2)
abl_net_moderate_test_error=0.5*sum((predicted_moderate_Rings - abl_test$Rings)^2)
abl_net_complex_test_error=0.5*sum((predicted_complex_Rings - abl_test$Rings)^2)
abl_net_mul_1_test_error=0.5*sum((predicted_mul_1_Rings- abl_test$Rings)^2)
abl_net_mul_2_test_error=0.5*sum((predicted_mul_2_Rings- abl_test$Rings)^2)
abl_net_mul_3_test_error=0.5*sum((predicted_mul_3_Rings - abl_test$Rings)^2)




#1.5


# Basic Model..
#1 hidden layer with 1 hidden node
abl_net_basic
plot(predicted_basic_Rings,abl_test$Rings)
abline(a=0,b=1)
cor(predicted_basic_Rings,abl_test$Rings)
abl_net_basic_test_error
# Training Error=1.930330843
# Test Error = 11.89441487
# Steps =  5887 
#Correlation= 0.7291490549



##### Moderate..
#1 hidden layer with 25 hidden node
abl_net_moderate
plot(predicted_moderate_Rings,abl_test$Rings)
abline(a=0,b=1)
cor(predicted_moderate_Rings,abl_test$Rings)
abl_net_moderate_test_error


# Training Error=1.467420337 
# Test Error = 14.26445735
# Steps =  10863
#Correlation= 0.6564248364

##### Complex..
#1 hidden layer with 50 hidden node
abl_net_complex
plot(predicted_complex_Rings,abl_test$Rings)
abline(a=0,b=1)
cor(predicted_complex_Rings,abl_test$Rings)
abl_net_complex_test_error


# Training Error=1.522193667
# Test Error = 11.96407036
# Steps =  3582
#Correlation= 0.7170009966

##### Multilayer 1.
#3  hidden layer with 10 hidden nodes each
abl_net_mul_1
plot(predicted_mul_1_Rings,abl_test$Rings)
abline(a=0,b=1)
cor(predicted_mul_1_Rings,abl_test$Rings)
abl_net_mul_1_test_error


# Training Error=1.06202953 
# Test Error = 13.77083678
# Steps =  6887
#Correlation=  0.6688168184

##### Multilayer 2
#3  hidden layers with 10,5,2 hidden nodes each
abl_net_mul_2
plot(predicted_mul_2_Rings,abl_test$Rings)
abline(a=0,b=1)
cor(predicted_mul_2_Rings,abl_test$Rings)
abl_net_mul_2_test_error


# Training Error=1.344864375 
# Test Error = 11.40400541
# Steps =  4125
#Correlation=   0.7305449029

##### Multilayer 3
#3  hidden layers with 5 hidden nodes each
abl_net_mul_3
plot(predicted_mul_3_Rings,abl_test$Rings)
abline(a=0,b=1)
cor(predicted_mul_3_Rings,abl_test$Rings)
abl_net_mul_3_test_error


# Training Error=1.571237909 
# Test Error = 10.86380421
# Steps =  1342
#Correlation=  0.7485426238


#1.7
#Basic model is better because only 1 hidden node , test error is lowest with higgest correlation and it look less time to run 

#Basic Model 
# Training Error=1.930330843
# Test Error = 11.89441487
# Steps =  5887 
#Correlation= 0.7291490549




#1.8
 # Multilayer model 3 is better with lowest test error and highest correlation 

# Training Error=1.571237909 
# Test Error = 10.86380421
# Steps =  1342
#Correlation=  0.7485426238
