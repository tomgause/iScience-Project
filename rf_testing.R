### rf testing
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 4/18/2022

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

# Load in our RFs and our test data
rf.all <- final.rf #TO DO: save final.rf and load in somehow
test <- readRDS("./data/test_subset_2022-04-18_10-27-26.RDS")

#To do: include training data
# Generate Climate Norm
# Mean of historic observed temperatures for each month
climate.norm <- test%>%
  dplyr::select(forecast_target, fcst_cell, 
                obs_tmp_k, x, y, target_month)%>%
  unique()%>%
  group_by(target_month,fcst_cell)%>%
  mutate(cummean_temp = cummean(obs_tmp_k))

#add 1 year to forecast_target so that each forecast is the historic mean of temp 
#NOT including current month's temperature
climate.norm$forecast_target <- climate.norm$forecast_target %m+% months(12)

#left_join with actual observed to line up properly with forecast_target
colnames(climate.norm)[3] <- "old_obs_tmp_k"
test.for.join <- test%>%
  dplyr::select(fcst_cell,forecast_target,obs_tmp_k)%>%
  unique()
climate.norm.joined <- left_join(climate.norm, test.for.join, by = c("fcst_cell",
                                                                     "forecast_target"))
climate.norm$old_obs_tmp_k <- climate.norm.joined$obs_tmp_k 
colnames(climate.norm)[3] <- "obs_tmp_k"
climate.norm <- na.omit(climate.norm)

# Select and store all forecast cells
sample.cells <- test %>%
  dplyr::select(fcst_cell) %>%
  unique()

# Create DF to store mse from RF, quantile matching, and base
cell.error <- data.frame(0,0,0,0)
colnames(cell.error) <- c("qm.mse", "base.mse",
                          "climate.norm.mse","rf.mse")

#rf.identifier <- 1
for (cell in sample.cells[,1]) {
  # Select a single point
  cell.data <- test %>%
    filter(fcst_cell == cell)
  
  climate.norm.cell <- filter(climate.norm,fcst_cell == cell)
  
  # Generate mse for qm, base, and climate.norm against obs
  qm.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_qm_tmp_k)^2)
  base.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_tmp_k)^2)
  climate.norm.mse <- mean((climate.norm.cell$obs_tmp_k - 
                              climate.norm.cell$cummean_temp)^2)
  
  # Generate predictions to find rf mse against obs
  pred <- predict(rf.all, data = cell.data)
  rf.temp.predictions <- cell.data$obs_tmp_k - pred$predictions
  rf.mse <- mean((cell.data$obs_tmp_k - rf.temp.predictions)^2)
  
  cell.error <- rbind(cell.error, data.frame(qm.mse,base.mse,
                                             climate.norm.mse,
                                             rf.mse))
  
  # Method for cycling through RFs
  #rf.identifier <- rf.identifier + 1
}

#remove 0 row
cell.error <- cell.error[2:nrow(cell.error),]
mean.errors <- data.frame(colMeans(cell.error[,]))
mean.errors <- mean.errors %>%
  mutate(bias.correction.method = rownames(mean.errors))
colnames(mean.errors)[1] = "MSE"
mean.errors$bias.correction.method <- c("Quantile Matching","Base","Climate Norm","RF All Pixels")

#generate plot
mean.errors %>%
  ggplot(mapping = aes(x = bias.correction.method, y = MSE))+
  geom_bar(stat = 'identity',fill="#b4eeb4",color = "#b4eeb4")+
  xlab("Bias Correction Method")+
  theme_test()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2013-2021 Data")


# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename <- paste0("./data/", "test_results_", currentTime, ".RDS")

# And save!
saveRDS(cell.error, file = filename)
