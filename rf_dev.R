### rf development scripts
# Tom Gause
# 4/7/2022

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

# Load in our data sets.
# For the first experiment, let's use a single cell!
train <- readRDS("./data/train_subset__2022-04-06_01-10-09.RDS") #TODO: UPDATE
test <- readRDS("./data/test_subset_2022-04-06_01-19-58.RDS") #AND THIS



# Currently, this model is designed to train on individual pixels.
# The experiment below will reveal approximately how often the rf performs
# better than the qm method by testing on 200 data points. We'll also save
# optimal parameters along the way so we can quickly reproduce results in
# the downstream (assuming any models perform well...)
sample.cells <- train %>%
  dplyr::select(fcst_cell) %>%
  unique()

min.cell.errors <- data.frame(0,0,0,0,0,0)
colnames(min.cell.errors) <- c("rf.mse", "qm.mse", "base.mse",
                               "best.mtry", "best.nodesize", "best.samplefrac")

for (i in sample.cells[,1]) {
  cell <- i
  
  # Select a single point
  train.data <- train %>%
    filter(fcst_cell == cell)
  test.data <- test %>%
    filter(fcst_cell == cell)
  
  # generate mse for qm predictions (for later analysis)
  qm.mse <- mean((test.data$obs_tmp_k.x - test.data$fcst_qm_tmp_k)^2)
  base.mse <- mean((test.data$obs_tmp_k.x - test.data$fcst_tmp_k)^2)
  #also include naive method mean((test.data$obs_tmp_k.x - test.data$HISTORICALGUESS)^2))
  #(need to generate historical method data first)
  
  # I removed the "bias" column. We can figure out if we're making an over-
  # or under-prediction downstream. Instead, I've predicted obs_tmp_k.x!
  # Predictor variables to use are: 
  # - x
  # - y
  # - target_month
  # - lead
  # - fcst_tmp_k
  # - elevation 
  # - c(lag1--> lag12) 
  # - fcst_pr_m_day 
  # ***fcst_pr_m_day: forecast precipitation ***
  #   ***lag1 : observed temperature one month ago***
  #   
  #   Target variable:
  #   - obs_tmp_k.x
  
  # clean up data. We don't need any of these columns for our analysis
  train.data <- subset(train.data, select = c(-forecast_timestamp,
                                              -fcst_cell,
                                              -obs_pr_m_day.x,
                                              -obs_cell))
  
  train.data <- na.omit(train.data)
  test.data <- subset(test.data, select = c(-forecast_timestamp,
                                            -fcst_cell,
                                            -obs_pr_m_day.x,
                                            -fcst_qm_tmp_k,
                                            -fcst_qm_pr_m_day))
  test.data <- na.omit(test.data)
  
  # clean up memory!
  gc()
  
  # For now, PCA is de-selected!
  #Run a PCA, to explore relationship between variables. 
  # data_for_pca <- hindcast_data%>%
  #   dplyr::select(-unique_row_identifer)
  # pca1 <- prcomp(data_for_pca, scale = TRUE)
  # pca1 #see outputs of PCA
  # plot(pca1) #see how much variance is explained by first few PCs
  # plot(cumsum(pca1$sdev^2/4))
  # Take out correlated variables if found.  
  
  
  
  # To optimize our random forest, we will need to pick the best hyperparameters
  # including mtry (the number of variables for each bagged regression tree to
  # consider), node size (the minimum size of a terminal node), and bootstrap 
  # resample size. We fix ntree (the number of trees in our random forest) to be 
  # 100 and optimize it later, as we know more trees in our forest will only 
  # increase the model's performance.
  
  metric.data <- data.frame(0,0,0,0)
  colnames(metric.data) <- c("i", "j", "k", "error")
  count <- 0
  
  for (i in 15:18){ #range of mtry, down to 18 as we are not using x,y
    for (j in c(10000, 15000, 20000)){ #range of nodesizes
      for (k in c(0.8)) { #range of sample fractions
        
        count <- count + 1
        cat(sprintf("\n\n MODEL %f\n", count))
        
        rf <- ranger(obs_tmp_k.x ~ ., # More efficient
                     data = train.data,
                     num.trees = 110, # Adjust this later
                     mtry = i, 
                     min.node.size = j,
                     sample.fraction = k,
                     num.threads = 16, # 16/20 threads on Alex machine
                     oob.error = TRUE) # use OOB error for cross validation
        
        # Generate predictions on test data and find MSE
        pred <- predict(rf, data = test.data)
        error <- mean((test.data$obs_tmp_k.x-pred$predictions)^2)
        cat(sprintf("ERROR: %f\n", error))
        
        # Bind metric data to frame
        metric.data <- rbind(metric.data, data.frame(i,j,k,error))
      }
    }
  }
  
  # Rename columns in metric data
  colnames(metric.data) <- c("mtry","nodesize","samplefrac","error")
  
  # Save the metric data here
  # First, grab and format the current time
  currentTime <- Sys.time()
  currentTime <- gsub(" ", "_", currentTime)
  currentTime <- gsub(":", "-", currentTime)
  filename <- paste0("./data/", "rf_parameters_", currentTime, ".RDS")
  
  # Don't need to save in the loop like this
  #print(paste0("saving metric data as ", filename, "..."))
  #saveRDS(metric.data, file = filename)
  
  
  
  #Print out optimal hyperparameters.
  metric.data1 <- metric.data
  metric.data1 <- metric.data1[2:nrow(metric.data1),] #remove 0,0,0,0 row
  which.min.error <- which.min(metric.data1$error)
  best.parameters <- metric.data1[which.min.error,]
  print(paste("The most ideal hyperparameters are mtry = ", best.parameters$mtry,
              ", nodesize = ", best.parameters$nodesize,
              ", bootstrap resample size = ", best.parameters$samplefrac,
              ", with an error of", best.parameters$error,
              ", vs qm error of", qm.mse))
  
  # save these for use in the next step!
  best.mtry <- best.parameters$mtry
  best.nodesize <- best.parameters$nodesize
  best.samplefrac <- best.parameters$samplefrac
  
  rf.mse <- best.parameters$error
  min.cell.errors <- rbind(min.cell.errors, data.frame(rf.mse, qm.mse, base.mse,
                                                       best.mtry,
                                                       best.nodesize,
                                                       best.samplefrac))
  gc()
}
saveRDS(min.cell.errors, "./data/error_200_points.RDS")

# test for num-trees
metric.data <- data.frame(0,0)
colnames(metric.data) <- c("i","error")
count <- 0

for (i in seq(50, 200, by = 10)) {
  count <- count + 1
  cat(sprintf("\n\n\n MODEL %f \n", count))
  
  rf <- ranger(obs_tmp_k.x ~ ., # More efficient
               data = train.data,
               num.trees = i,
               mtry = best.mtry, 
               min.node.size = best.nodesize,
               sample.fraction = best.samplefrac,
               num.threads = 16) # 16/20 threads on Alex machine
  
  # Generate predictions on test data and find MSE
  pred <- predict(rf, data = test.data)
  error <- mean((test.data$obs_tmp_k.x-pred$predictions)^2)
  
  # Bind metric data to frame
  metric.data <- rbind(metric.data, data.frame(i,error))
}

# Rename columns in metric data
colnames(metric.data) <- c("num.trees","error")

# Save the metric data here
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename <- paste0("./data/", "rf_tree_params_", currentTime, ".RDS")

print(paste0("saving metric data as ", filename, "..."))
saveRDS(metric.data, file = filename)



#Print out optimal hyperparameter
metric.data1 <- metric.data
metric.data1 <- metric.data1[2:nrow(metric.data1),] #remove 0,0,0,0 row
which.min.error <- which.min(metric.data1$error)
best.parameters <- metric.data1[which.min.error,]
print(paste("Ideal number of trees = ", best.parameters$num.trees,
            ", with an error of", best.parameters$error))



