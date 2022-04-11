### rf development scripts
# Tom Gause
# 4/11/2022

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
#test <- readRDS("./data/test_subset_2022-04-06_01-19-58.RDS") #AND THIS



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

train.data <- subset(train, select = c(-forecast_timestamp,
                                            -fcst_cell,
                                            -obs_pr_m_day.x,
                                            -obs_cell,
                                            -lag1,
                                            -lag2,
                                            -lag3,
                                            -lag4,
                                            -lag5,
                                            -lag6,
                                            -lag7,
                                            -lag8,
                                            -lag9,
                                            -lag10,
                                            -lag11,
                                            -lag12))
# omit all rows that contain an NA
train.data <- na.omit(train.data)
# clean up memory!
gc()
ncol(train.data)

# To optimize our random forest, we will need to pick the best hyperparameters
# including mtry (the number of variables for each bagged regression tree to
# consider), node size (the minimum size of a terminal node), and bootstrap 
# resample size. We fix ntree (the number of trees in our random forest) to be 
# 100 and optimize it later, as we know more trees in our forest will only 
# increase the model's performance.
  
metric.data <- data.frame(0,0,0,0)
colnames(metric.data) <- c("i", "j", "k", "error")
count <- 0

for (i in 6){ #range of mtry, down to 18 as we are not using x,y
  for (j in c(10000)){ #range of nodesizes
    for (k in c(0.8)) { #range of sample fractions
      
      count <- count + 1
      cat(sprintf("\n\n MODEL %f\n", count))
      
      rf <- ranger(obs_tmp_k.x ~ ., # More efficient
                   data = train.data,
                   num.trees = 100, # Adjust this later
                   mtry = i, 
                   min.node.size = j,
                   sample.fraction = k,
                   num.threads = 18, # 20 threads on Alex machine
                   oob.error = TRUE) # use OOB error for cross validation
      
      # Get OOB
      error <- rf$prediction.error
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
  
print(paste0("saving metric data as ", filename, "..."))
saveRDS(metric.data, file = filename)

# Print out optimal hyperparameters.
metric.data1 <- metric.data
metric.data1 <- metric.data1[2:nrow(metric.data1),] #remove 0,0,0,0 row
which.min.error <- which.min(metric.data1$error)
best.parameters <- metric.data1[which.min.error,]
print(paste("The most ideal hyperparameters are mtry = ", best.parameters$mtry,
            ", nodesize = ", best.parameters$nodesize,
            ", bootstrap resample size = ", best.parameters$samplefrac,
            ", with an error of", best.parameters$error,
            ", vs qm error of", qm.mse))