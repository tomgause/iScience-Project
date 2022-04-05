### rf development scripts
# Tom Gause
# 4/5/2022

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
hindcast_data <- readRDS("./data/train_subset__2022-04-05_11-10-01.RDS") #TODO: UPDATE
test.data <- readRDS("./data/test_subset_2022-04-05_11-22-14.RDS") #AND THIS

# generate mse for qm predictions (for later analysis)
qm.mse <- mean((test.data$obs_tmp_k.x - test.data$fcst_qm_tmp_k)^2)


# I removed the "bias" column. We can figure out if we're making an over-
# or under-prediction downstream. Instead, I've predicted obx_tmp_k.x!
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

# clean up data. We don't need these columns for our analysis
training.data <- subset(hindcast_data, select = c(-forecast_timestamp,
                                                  -fcst_cell,
                                                  -obs_pr_m_day.x,
                                                  -forecast_target))

test.data <- subset(test.data, select = c(-forecast_timestamp,
                                          -fcst_cell,
                                          -obs_pr_m_day.x,
                                          -forecast_target,
                                          -fcst_qm_tmp_k,
                                          -fcst_qm_pr_m_day,
                                          -obs_cell))

#remove hindcast data to free memory
rm(hindcast_data)
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
# increase the model's performance. We define error here as the out-of-bag 
# mean squared error of the random forest's predictions.

metric.data <- data.frame(0,0,0,0)
colnames(metric.data) <- c("i", "j", "k", "error")
count <- 0

for (i in 1:20){ #range of mtry 
  for (j in c(10,100,1000,10000,100000)){ #range of nodesizes
    for (k in c(0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) { #range of sample fractions
      rf <- ranger(obs_tmp_k.x ~ ., # Used ranger function, much faster than randomforest
                   data = training.data,
                   num.trees = 250, # adjust this later
                   mtry = i, 
                   min.node.size = j,
                   sample.fraction = k,
                   num.threads = 16)
      
      pred <- predict(rf, data = training.data)
      error <- mean((test.data$obs_tmp_k.x-pred$predictions)^2)
      
      #error <- tail(rf$mse, 1) #this came from https://stats.stackexchange.com/questions/369134/random-forest-out-of-bag-rmse
      #rf$mse gives OOB mse for bagging 1:n trees, so last mse in this vector gives the OOB mse of the entire forest
      
      newdf <- data.frame(i,j,k,error)
      metric.data <- rbind(metric.data, newdf)
      
      # Get count
      count <- count + 1
      print(count)
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
print(paste0("saving as ", filename, "..."))
saveRDS(metric.data, file = filename)



#Print out optimal hyperparameters.
metric.data1 <- metric.data
metric.data1 <- metric.data1[2:nrow(metric.data1),] #remove 0,0,0,0 row
which.min.error <- which.min(metric.data1$error)
best.parameters <- metric.data1[which.min.error,]
print(paste("The most ideal hyperparameters are mtry = ", best.parameters$mtry,
            ", nodesize = ", best.parameters$nodesize,
            ", bootstrap resample size = ", best.parameters$resamplesize,
            ", with an error of", round(best.parameters$error)))

#To optimize the random forest hyperparameters, all values of mtry were tested (1:20), where 20 is the number of possible input variables. For node size, a logarithmic scale sequence was tested. For the bootstrap resample size, we chose to test a range of values up to the maximum number of rows in the data set. I assumed a resample smaller than ~50% of my data set would not be effective. 
#Next step: look at output of metric.data, see if we need to test more granlar bootstrap resample size, nodesize.



#_________________________________________________________________________________
#Now that we have our ideal mtry, node size, and resample size, all that's left is to choose ntree, or the number of trees. As noted earlier, more trees means less error, so let's see what a reasonable optimal ntree might be, considering run time. Let's compare the error from using ntree = 100 vs. ntree = 500.

#try ntree = 100 (what was done above)
#we know error = X, as was calculated above
# system.time(rf100 <- randomForest(ActivePower ~ .,
#                  data = before.wind.rf, mtry = 9, nodesize = 1,sampsize = 10636, ntree = 100))

#Ntree = 100 took X minutes to run, with an error of X.

#try ntree = 500
# rf500 <- randomForest(ActivePower ~ .,
#                  data = before.wind.rf, mtry = 9, nodesize = 1,sampsize = 10636, ntree = 500)
# 
# system.time(rf500 <- randomForest(ActivePower ~ .,
#                  data = before.wind.rf, mtry = 9, nodesize = 1,sampsize = 10636, ntree = 500))

# tail(rf500$mse,1)

#Ntree = 500 took X minutes to run, with an error of X.







