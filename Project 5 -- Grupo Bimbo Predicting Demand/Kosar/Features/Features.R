library(hash)
library(data.table)
library(Metrics)
library(dplyr)
library(xgboost)
library(Matrix)
library(slackr)



#__________________________Slackr setup__________________________
slackr_setup(channel = "@akosar", username = "i_bimbobot", icon_emoji = "",
             incoming_webhook_url = "datasqawd.slack.com", api_token = "xoxb-51493476675-ZkuheKfwDSdeEtTok0NRPcG6",
             echo = FALSE)

#__________________________Reading in the required files__________________________
town <-fread('town_state_with_town_id.csv')
train <- fread("train_lag_wtpcs_freq_count.csv")
train$NombreCliente = as.factor(train$NombreCliente)



#__________________________Adding Feature: Product Count by Town IDs__________________________
freq_prod_by_town <-  train %>% select(town_id, Producto_ID)

freq <- freq_prod_by_town %>% 
  group_by(town_id, Producto_ID) %>%
  summarise(count = n())

train = merge(train, freq, by = c("town_id", "Producto_ID"), all.x = TRUE)

#_____________________________________Adding Feature: Client Count by Town IDs____________________________
client_by_town <-  train %>% 
  select(town_id, Cliente_ID)

freq_client <- client_by_town %>% 
  group_by(town_id, Cliente_ID) %>%
  summarise(count = n())

train = merge(train, freq_client, by = c("town_id", "Cliente_ID"), all.x = TRUE)
head(train)

#____________Adding Feature: Product count by each client in each town_________________
townid_prodid_clientid = select(train, town_id, Cliente_ID, Producto_ID)
freq_prod_client_town <- townid_prodid_clientid %>% 
  group_by(town_id, Cliente_ID, Producto_ID) %>%
  summarise(count = n())
head(freq_prod_client_town)

train = merge(train, freq_prod_client_town, by = c("town_id", "Cliente_ID", "Producto_ID"), all.x = TRUE)
head(train)


#____________Adding Feature: Product count by each client in each route in each town________________

town_route_client_prod <-  train %>% 
  select(town_id, Ruta_SAK,Cliente_ID, Producto_ID) %>%
  group_by(town_id, Ruta_SAK,Cliente_ID, Producto_ID) %>%
  summarise(count = n())
colnames(town_route_client_prod) = c('town_id', 'Ruta_SAK', 'Cliente_ID', 'Producto_ID', 'prod_count_route')
head(town_route_client_prod)
train = merge(train, town_route_client_prod, by = c("town_id", "Ruta_SAK", "Cliente_ID", "Producto_ID"), all.x = TRUE)


#____________Adding Feature: Client count in eaech route in each town____________

town_route_client <-  train %>% 
                        select(town_id, Ruta_SAK,Cliente_ID) %>%
                        group_by(town_id, Ruta_SAK,Cliente_ID) %>%
                        summarise(count = n())

colnames(town_route_client) = c('town_id', 'Ruta_SAK', 'Cliente_ID', 'client_count_route')
train = merge(train, town_route_client, by = c("town_id", "Ruta_SAK", "Cliente_ID"), all.x = TRUE)

head(train)
     
#____________Creating train and test sets____________
test <- train[Semana == 9, ]
train <- train[Semana < 9, ]

test[is.na(test)] <- 0
train[is.na(train)] <- 0
head(train)
head(test)

train.y <- train$Demanda_uni_equil
# test.id <- test$id
# test$id <- NULL
test.y <- test$Demanda_uni_equil
test$Demanda_uni_equil <- NULL

gc()

#______________________________________XGBoost Model______________________________________
train.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = as.data.frame(train))

dtrain <- xgb.DMatrix(data = train.model, label = train.y)
watchlist <- list(train=dtrain)
rm(train.model, train)
gc()

#rounds_cv = c(13, 15, 17, 20, 23, 25, 27, 30)
depths = c(15,17,20,22,25,27)
results = as.numeric(1:length(depths))

ptm <- proc.time()
for (depth_cv in 1:length(depths)) {
  #foreach(depth_cv = 1:length(depths)) %dopar% {
  
  set.seed(1234)
  param <- list(  objective           = "reg:linear",
                  booster             = "gbtree",
                  eval_metric         = "rmse",
                  eta                 = 0.2,
                  max_depth           = depths[depth_cv]
  )
  
  clf <- xgb.train(   params              = param, 
                      data                = dtrain, 
                      nrounds             = 9,
                      verbose             = 1,
                      watchlist           = watchlist,
                      maximize            = FALSE
  )
  
  test$Demanda_uni_equil <- -1.
  test.model <- sparse.model.matrix(Demanda_uni_equil ~ ., data = test)
  
  preds <- predict(clf, test.model)
  test.y <- as.numeric(test.y)
  
  preds[preds < 0] = 0.1
  result = rmsle(test.y, preds)
  
  results[depth_cv] = result
  message = paste0("Feature: Median sales and return for each product for each client in each town / For depth ", depths[depth_cv], ", nround:9", ", your rmsle was: ", result)
  slackr(message)
  to_csv  = data.frame(nrounds = depths, results = results)
  write.csv(to_csv, "results_with_town_id.csv", row.names = F)
}

proc.time() - ptm





#____________Added Features and rmsle results____________#
#Avg sales per week, per product > For depth 20: 0.4996
#                                  For depth 25: 0.4995 


#Town_id > For depth 17: 0.4987
#          For depth 20: 0.4917
#          For depth 22: 0.4898
#          For depth 25: 0.4888
#          For depth 27: 0.4902

#Product count by town_id > For depth 15: 0.5070
#                           For depth 17: 0.4989
#                           For depth 20: 0.4917
#                           For depth 22: 0.4897
#                           For depth 25: 0.4823
#                           For depth 27:



#Client count by town_id, nround = 10 >  For depth 15: 0.5051
#                                        For depth 17: 0.4958
#                                        For depth 20: 0.4865
#                                        For depth 22: 0.4834
#                                        For depth 25: 0.4823
#                                        For depth 27: 

#Client count by town_id, nround = 9 >   For depth 15: 0.5040
#                                        For depth 17: 0.4947
#                                        For depth 20: 0.4860
#                                        For depth 22: 0.4828
#                                        For depth 25: 0.4820
#                                        For depth 27: 0.4835 



#Product count for each client in each town, nround = 10 (benchmark =  0.4787) > For depth 15: 0.4925
#                                                                                For depth 17: 0.4868
#                                                                                For depth 20: 0.4810
#                                                                                For depth 22: 0.4789
#                                                                                For depth 25: 0.4785 



#Product count for each client in each route in each town, nround = 9 (benchmark =  0.4787) >  For depth 15: 0.4934
#                                                                                              For depth 17: 0.4873
#                                                                                              For depth 20: 0.4812
#                                                                                              For depth 22: 0.4788
#                                                                                              For depth 25: 0.4786









