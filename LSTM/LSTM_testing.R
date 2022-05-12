### ML.R
# Tom Gause
# 5/12/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

packages <- c("tidyverse", "glue", "forcats", "timetk", "tidyquant",
              "tibbletime", "cowplot", "recipes", "rsample", "yardstick",
              "keras", "tensorflow")
install.packages(setdiff(packages, rownames(installed.packages()))) 

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)
# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)
# Visualization
library(cowplot)
# Preprocessing
library(recipes)
# Sampling / Accuracy
library(rsample)
library(yardstick) 
# Modeling
library(keras)
library(tensorflow)

setwd("/storage/tgause/iScience_tom/iScience_Project/LSTM")

# Make all the data
test <- readRDS("/storage/tgause/iScience_tom/iScience_Project/data/VT_test.RDS")


test <- test[order(test$forecast_target),]
gc()

# We'll be using a batch size of 32, so let's reduce this to 320.
#train.all.predictions <- train.all.predictions %>%
#  filter(index %in% train.months)

test.all.predictions <- test %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  mutate(value = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  dplyr::group_by(forecast_target, fcst_cell, lead) %>%
  mutate(value = mean(value)) %>%
  dplyr::ungroup() %>%
  select(c(value, index, fcst_cell, lead)) %>%
  as_tbl_time(index = index, lead = lead, fcst_cell = fcst_cell)
test.months <- length(unique(test.all.predictions$index))

# We have 24x duplicate data for each lead-cell-index.
# By taking the unique data only, we fix this issue.
test <- test.all.predictions %>% unique()

gc()

test %>% head(2)

############################################################################
# PREPROCESSING
# LSTM performs better if we center and scale our data.
# We normalize from 0 to 1.

max.test <- max(test$value)
min.test <- min(test$value)
test.scaled <- test %>%
  mutate(value = (value - min.test)/(max.test - min.test))

# This is necessary for the agent-based minibatching
unique.cells <- unique(test$fcst_cell)

# Same for test data...
test.matrix <- array(list(), dim=c(9, 51))

# We'll toss the date row--the data's already ordered!
for (i in c(1:9)) {
  for (j in c(1:length(unique.cells))) {
    test.matrix[[i, j]] <- as.matrix(test.scaled %>%
      dplyr::filter(lead == i, fcst_cell == unique.cells[j]))[,-2]
    class(test.matrix[[i,j]]) <- "numeric"
  }
}

# A generator is a special type of function that you call repeatedly to
# obtain a sequence of values. This generator function taakes the current
# array of data and yields batches of data from the recent past,
# along with a target value in the future. We'll generate samples on the
# fly using the original data.
# Arguments:
#   data: original array of normalized floating-point data
#   lookback: how many timesteps back the input data should go
#   delay: how many timesteps in the future the target should be
#   min.index and max.index: indices in data array that deliminit which
#     timesteps to draw from. Useful for keeping segment for val and test
#   shuffle: whether to shuffle samples or draw in chronological order
#   batch.size: number of samples per batch
#   step: period in timesteps at which you sample data
generator <- function(data, lookback, delay, min.index, max.index,
                      shuffle = FALSE, batch.size = 1, step = 1,
                      cell.sample = 16, lead.sample = 4) {
  if (is.null(max.index)) {
    max.index <- nrow(data[[1]]) - delay - 1
  }
  i <- min.index + lookback
  
  function() {
    # samples and targets for binding
    all.samples <- array(0,dim=c(cell.sample * lead.sample,
                                 lookback,
                                 3))
    all.targets <- array(0,dim=c(cell.sample * lead.sample,
                                 batch.size))
    
    # Agent-based random sampling
    lead.sample <- sample(c(1:9), size = lead.sample)
    cell.sample <- sample(c(1:51), size = cell.sample)
    
    count <- 1
    for (LS in lead.sample) {
      for (CS in cell.sample) {

        # grab relevant lead&cell combo
        temp <- train.matrix[[LS,CS]]
        
        if(shuffle) {
          rows <- sample(c((min.index + lookback):max.index), size = batch.size)
        } else {
          if (i + batch.size >= max.index)
            i <<- min.index + lookback
          rows <- c(i:min(i + batch.size-1, max.index))
          i <<- i + length(rows)
        }
        
        samples <- array(0, dim = c(lookback, 3)) #dim(temp)[[-2]]
        targets <- array(0, dim = c(length(rows)))

          
        indices <- seq(rows[[1]] - lookback, rows[[1]]-1,
                       length.out = dim(samples)[[1]])
        
        samples <- temp[indices,]
        targets <- temp[rows[[1]] + delay, 1]
        
        
        #print(LS * (CS-1) + CS)
        all.samples[count,,] <- samples
        all.targets[count,] <- targets
        count <- count + 1
      }
    }
    list(all.samples, all.targets)
  }
}

lookback <- 32 # Observations will go back 32 months
steps <- 1 # Observations will be sampled at 1 data point per month
delay <- 1 # Targets will be 1 months ahead
batch.size <- 1 # cell.sample and lead.sample are are our batching methods...
cell.sample <- 16 # We select 16 cells at a time for this experiment
lead.sample <- 4 # Select 4 lead times

# Let's use the abstract generator to instantiate three generators
#   Train: first 240 timesteps
#   Test: Next 60 timesteps
#   Validator: Remaining timesteps



test.gen = generator(
  test.matrix,
  lookback = lookback,
  delay = delay,
  min.index = 1,
  max.index = 150,
  step = step,
  batch.size = batch.size
)

# How many steps to draw from test.gen in order to see the entire test set
test.steps <- floor((150 - 1 - lookback) / batch.size)


# Create a new model instance
model <- load_model_tf("/storage/tgause/iScience_tom/iScience_Project/LSTM/model")
model2 <- load_model_tf("/storage/tgause/iScience_tom/iScience_Project/LSTM/model2")
model3 <- load_model_tf("/storage/tgause/iScience_tom/iScience_Project/LSTM/model3")

# Restore the weights
model %>% load_model_weights_tf('/storage/tgause/iScience_tom/iScience_Project/LSTM/checkpoints/checkpoint1')
model2 %>% load_model_weights_tf('/storage/tgause/iScience_tom/iScience_Project/LSTM/checkpoints/checkpoint2')
model3 %>% load_model_weights_tf('/storage/tgause/iScience_tom/iScience_Project/LSTM/checkpoints/checkpoint3')

# Get a brief summary of the 3 architectures...
summary(model)
summary(model2)
summary(model3)

# and EVALUATE!!

model %>% evaluate(
    test.gen,
    verbose=0
)
model2 %>% evaluate(
    test.gen,
    verbose=0
)
model3 %>% evaluate(
    test.gen,
    verbose=0
)


