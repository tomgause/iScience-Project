### rf_testing_multi_cell
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 5/12/2022

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
test <- readRDS("./data/test_subset_2022-04-18_10-27-26.RDS")

# Generate Climate Norm
# Mean of historic observed temperatures for each month
# only use train data for climate norm calculation
climate.norm2 <- train%>%
  dplyr::select(target_month, obs_tmp_k, fcst_cell, forecast_target)%>%
  unique()

climate.norm2 <- climate.norm2%>%
  group_by(fcst_cell, target_month)%>%
  summarize(mean_temp = mean(obs_tmp_k))

# Select and store all forecast cells
sample.cells <- test %>%
  dplyr::select(fcst_cell,x,y) %>%
  unique()

# Create DF to store mse from RF, quantile matching, and base
cell.error <- data.frame(0,0,0,0)
colnames(cell.error) <- c("qm.mse", "base.mse",
                          "climate.norm.mse","rf.mse")

for (cell in sample.cells[,1]) {
  # Select a single point
  cell.data <- test %>%
    filter(fcst_cell == cell)
  
  #climate norm method 2
  climate.norm.cell <- climate.norm2%>%
    filter(fcst_cell == cell)
  climate.norm.preds <- left_join(cell.data, climate.norm.cell, 
                                  by = c("fcst_cell", "target_month"))
  
  # Generate mse for qm, base, and climate.norm against obs
  qm.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_qm_tmp_k)^2)
  base.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_tmp_k)^2)
  
  #climate.norm method 2
  climate.norm.mse <- mean((climate.norm.preds$obs_tmp_k - 
                              climate.norm.preds$mean_temp)^2)
  
  # Generate predictions to find rf mse against obs
  pred <- predict(final.rf, data = cell.data)
  rf.temp.predictions <- cell.data$obs_tmp_k - pred$predictions
  rf.mse <- mean((cell.data$obs_tmp_k - rf.temp.predictions)^2)
  
  cell.error <- rbind(cell.error, data.frame(qm.mse,base.mse,
                                             climate.norm.mse,
                                             rf.mse))
}

cell.error <- cell.error[2:nrow(cell.error),] #remove 0 row
cell.error <- cell.error%>%
  mutate(x = sample.cells$x)%>%
  mutate(y = sample.cells$y)
colnames(cell.error)[1:4] <- c("Quantile Matching","Base","Climate Norm","RF All Pixels")

# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename_cellerrors <- paste0("./data/", "cell_errors_test_results_", currentTime, ".RDS")
filename_meanerrors <- paste0("./data/", "mean_errors_test_results_", currentTime, ".RDS")


# And save!
saveRDS(cell.error, file = filename_cellerrors)
saveRDS(mean.errors, file = filename_meanerrors)