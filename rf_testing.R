### rf testing
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 4/17/2022

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

#set working directory to file location
setwd(dirname(getActiveDocumentContext()$path)) 

# Load in our RFs and our test data
rf.all <- final.rf #TO DO: save final.rf and load in somehow
test <- readRDS("./data/test_subset_2022-04-17_22-15-42.RDS") #AND THIS

# Generate Climate Norm
# Mean of observed temperatures for each month
#TO DO: fix this, only gives a single row
climate.norm <- aggregate(test$obs_tmp_k, list(test$target_month), FUN=mean)
colnames(climate.norm) <- c("month","mean")
climate.norm <- climate.norm[order("month"),]

# Select and store all forecast cells
sample.cells <- test %>%
  dplyr::select(fcst_cell) %>%
  unique()

# Create DF to store mse from RF, quantile matching, and base
cell.error <- data.frame(0,0,0,0,0,0)
colnames(cell.error) <- c("rf.norm.mse", "qm.norm.mse", "base.norm.mse",
                          "rf.obs.mse", "qm.obs.mse", "base.obs.mse")

#rf.identifier <- 1
for (cell in sample.cells[,1]) {
  # Select a single point
  cell.data <- test %>%
    filter(fcst_cell == cell)
  
  
  #why?
  # Generate mse for qm and base against obs and climate.norm
  qm.obs.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_qm_tmp_k)^2)
  base.obs.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_tmp_k)^2)
  #these two below are giving NAs
  #qm.climate.norm.mse <- mean((climate.norm[cell.data$target_month,2] - cell.data$fcst_qm_tmp_k)^2)
  #base.climate.norm.mse <- mean((climate.norm[cell.data$target_month,2] - cell.data$fcst_tmp_k)^2)
  
  # Generate predictions to find rf mse against obs and climate.norm
  pred <- predict(rf.all, data = cell.data)
  rf.temp.predictions <- cell.data$obs_tmp_k - pred$predictions
  rf.obs.mse <- mean((cell.data$obs_tmp_k - rf.temp.predictions)^2)
  #rf.climate.norm.mse <- mean((climate.norm[cell.data$target_month,2] - pred$predictions)^2)
  
  cell.error <- rbind(cell.error, data.frame(rf.climate.norm.mse,
                                             qm.climate.norm.mse,
                                             base.climate.norm.mse,
                                             rf.obs.mse,
                                             qm.obs.mse,
                                             base.obs.mse))
  
  # Method for cycling through RFs
  #rf.identifier <- rf.identifier + 1
}

#see results of cell.error
cell.errors <- cell.error[,4:6]
colMeans(cell.errors[,])

# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename <- paste0("./data/", "test_results_", currentTime, ".RDS")

# And save!
saveRDS(cell.error, file = filename)

