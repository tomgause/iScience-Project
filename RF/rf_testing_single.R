### rf testing
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 4/19/2022

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
#rf.all <- final.rf # this doesn't work: load("finalVTrf.allpixels.RData")
train <- readRDS("./data/train_subset_Vermont_2022-04-16_14-55-49.RDS")
test <- readRDS("./data/test_subset_2022-04-18_10-27-26.RDS")

#To do: include training data
# Generate Climate Norm
# Mean of historic observed temperatures for each month
train1 <- train%>%
  dplyr::select(-obs_cell)

test1 <- test%>%
  dplyr::select(-fcst_qm_pr_m_day,-fcst_qm_tmp_k)

all.data <- rbind(train1,test1)

climate.norm <- all.data%>%
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
all.data.for.join <- all.data%>%
  dplyr::select(fcst_cell,forecast_target,obs_tmp_k)%>%
  unique()
climate.norm.joined <- left_join(climate.norm, all.data.for.join, by = c("fcst_cell",
                                                                         "forecast_target"))
climate.norm$old_obs_tmp_k <- climate.norm.joined$obs_tmp_k 
colnames(climate.norm)[3] <- "obs_tmp_k"
climate.norm <- na.omit(climate.norm)

#test for one cell: looks like it worked!
mytest <- climate.norm%>%
  filter(fcst_cell == 261073,
         target_month == 6)

mytest1 <- train1%>%
  filter(fcst_cell == 261073,
         target_month == 6)%>%
  dplyr::select(forecast_target,obs_tmp_k)%>%
  unique()

#climate.norm is missing some data, as there is a gap in between the train and test set 
#(missing climate.norm data = Jan 2010 = Feb 2014)

#remove data before March 2014 from the test set to make testing fair
test <- test%>%
  filter(forecast_target > "2014-02-01")

#climate.norm method 2: only use train data for climate norm calculation!
climate.norm2 <- train%>%
  dplyr::select(target_month, obs_tmp_k, fcst_cell, forecast_target)%>%
  unique()

climate.norm2 <- climate.norm2%>%
  group_by(fcst_cell, target_month)%>%
  summarize(mean_temp = mean(obs_tmp_k))

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
  
  #climate.norm method 1
  # climate.norm.cell <- climate.norm%>%
  #   filter(fcst_cell == cell)%>%
  #   ungroup()%>%
  #   dplyr::select(forecast_target,cummean_temp)
  # climate.norm.preds <- left_join(cell.data, climate.norm.cell, 
  #                                 by = "forecast_target")
  
  #climate norm method 2
  climate.norm.cell <- climate.norm2%>%
    filter(fcst_cell == cell)
  climate.norm.preds <- left_join(cell.data, climate.norm.cell, 
                                  by = c("fcst_cell", "target_month"))
  
  # Generate mse for qm, base, and climate.norm against obs
  qm.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_qm_tmp_k)^2)
  base.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_tmp_k)^2)
  #climate.norm method 1
  # climate.norm.mse <- mean((climate.norm.preds$obs_tmp_k - 
  #                             climate.norm.preds$cummean_temp)^2)
  #climate.norm method 2
  climate.norm.mse <- mean((climate.norm.preds$obs_tmp_k - 
                              climate.norm.preds$mean_temp)^2)

  rf <- ranger(bias.t ~ ., # More efficient
               data = cell.data,
               num.trees = 2000, # Adjust this later
               mtry = 15, 
               min.node.size = 3,
               sample.fraction = 1,
               num.threads = 8, # 20 threads on Alex machine
               oob.error = TRUE)
  
  # Generate predictions to find rf mse against obs
  pred <- predict(rf, data = cell.data)
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

mean.errors.noclimatenorm <- mean.errors%>%
  filter(bias.correction.method != "Climate Norm")

#generate plot
mean.errors %>%
  ggplot(mapping = aes(x = bias.correction.method, y = MSE))+
  geom_bar(stat = 'identity',fill="#b4eeb4",color = "#b4eeb4")+
  xlab("Bias Correction Method")+
  theme_test()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2014-2021 Data")


# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename <- paste0("./data/", "test_results_", currentTime, ".RDS")

# And save!
saveRDS(cell.error, file = filename)