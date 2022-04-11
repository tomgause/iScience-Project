### rf testing
# Tom Gause
# 4/8/2022

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
rf.all <- readRDS("./data/train_subset__2022-04-06_01-10-09.RDS") #TODO: UPDATE
test <- readRDS("./data/test_subset_2022-04-06_01-19-58.RDS") #AND THIS

# Generate Climate Norm
# Mean of observed temperatures for each month
norm <- aggregate(test$obs_tmp_k.x, list(test$target_month), FUN=mean)
colnames(norm) <- c("month","mean")
norm <- norm[order("month"),]

# Select and store all forecast cells
sample.cells <- test %>%
  dplyr::select(fcst_cell) %>%
  unique()

# Create DF to store mse from RF, quantile matching, and base
cell.error <- data.frame(0,0,0,0,0,0)
colnames(cell.error) <- c("rf.norm.mse", "qm.norm.mse", "base.norm.mse",
                          "rf.obs.mse", "qm.obs.mse", "base.obs.mse")

rf.identifier <- 1
for (cell in sample.cells[,1]) {
  # Select a single point
  cell.data <- test %>%
    filter(fcst_cell == cell)
  
  # Generate mse for qm and base against obs and norm
  qm.obs.mse <- mean((cell.data$obs_tmp_k.x - cell.data$fcst_qm_tmp_k)^2)
  base.obs.mse <- mean((cell.data$obs_tmp_k.x - cell.data$fcst_tmp_k)^2)
  qm.norm.mse <- mean((norm[cell.data$target_month,2] - cell.data$fcst_qm_tmp_k)^2)
  base.norm.mse <- mean((norm[cell.data$target_month,2] - cell.data$fcst_tmp_k)^2)
  
  # Generate predictions to find rf mse against obs and norm
  pred <- predict(rf.all[], data = cell.data)
  rf.obs.mse <- mean((cell.data$obs_tmp_k.x - pred$predictions)^2)
  rf.norm.mse <- mean((norm[cell.data$target_month,2] - pred$predictions)^2)

  cell.error <- rbind(cell.error, data.frame(rf.norm.mse,
                                             qm.norm.mse,
                                             base.norm.mse,
                                             rf.obs.mse,
                                             qm.obs.mse,
                                             base.obs.mse))
  
  # Method for cycling through RFs
  rf.identifier <- rf.identifier + 1
}
  
# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename <- paste0("./data/", "test_results_", currentTime, ".RDS")

# And save!
saveRDS(cell.error, file = filename)

