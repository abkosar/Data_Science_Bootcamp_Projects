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
train <- fread("train_with_mde_features.csv") 


#__________________________Adding Feature: Town IDs__________________________
town = town[ , .(Agencia_ID, town_id)]
train = merge(train, town, by = "Agencia_ID", all.x = TRUE)

