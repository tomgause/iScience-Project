### rf_testing_multi_cell
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 5/20/2022

library(data.table)
library(rvest)
library(stringr)
library(tidyverse)
library(tidytext)
library(ggplot2)
library(readxl)
library(class)
library(FNN)
library(MASS)
library(ISLR)
library(rpart)
library(rattle)
library(ipred)
library(randomForest)
library(caret)
library(dendextend)
library(arules)
library(microbenchmark)
library(ranger)
library(lubridate)

#set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 

# Load in our RFs and our test data; and train data for climate norm calculations
#to do: check to see that loading in RF works
load("finalVTrf.allpixels.RData") #called final.rf
train <- readRDS("./data/train_subset_Vermont_2022-04-16_14-55-49.RDS")
test <- readRDS("./data/test_subset_2022-05-09_19-15-23.RDS")
test.data <- test

# Generate Climate Norm
# Mean of historic observed temperatures for each month
# only use train data for climate norm calculation
climate.norm2 <- train%>%
  dplyr::select(target_month, obs_tmp_k, fcst_cell, forecast_target)%>%
  unique()

climate.norm2 <- climate.norm2%>%
  group_by(fcst_cell, target_month)%>%
  summarize(fcst_climate_norm = mean(obs_tmp_k))

test.data <- left_join(test.data, climate.norm2, 
                       by = c("fcst_cell", "target_month"))

# Generate mse for qm, base, and climate.norm against obs
test.data <- test.data%>%
  mutate(qm.se = (test.data$obs_tmp_k - test.data$fcst_qm_tmp_k)^2)%>%
  mutate(base.se = (test.data$obs_tmp_k - test.data$fcst_tmp_k)^2)%>%
  mutate(climate.norm.se = (test.data$obs_tmp_k - test.data$fcst_climate_norm)^2)  

# Generate predictions to find rf mse against obs
rf.bias.pred <- predict(final.rf, data = test.data)
test.data <- test.data%>%
  mutate(fcst_rf_tmp = test.data$fcst_tmp_k + rf.bias.pred$predictions) #calculate new bias-corrected forecast value

test.data <- test.data%>%
  mutate(rf.se = (test.data$obs_tmp_k - test.data$fcst_rf_tmp)^2)

mean.errors <- test.data%>%
  summarize(qm.mse = mean(qm.se),
            climate.mse = mean(climate.norm.se),
            base.mse = mean(base.se),
            rf.mse = mean(rf.se))

cell.errors <- test.data%>%
  group_by(x,y)%>%
  summarize(qm.mse = mean(qm.se),
            climate.mse = mean(climate.norm.se),
            base.mse = mean(base.se),
            rf.mse = mean(rf.se))


# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename_cellerrors <- paste0("./data/", "FINAL_cell_errors_test_results_", currentTime, ".RDS")
filename_meanerrors <- paste0("./data/", "FINAL_mean_errors_test_results_", currentTime, ".RDS")


# And save!
saveRDS(cell.errors, file = filename_cellerrors)
saveRDS(mean.errors, file = filename_meanerrors)
