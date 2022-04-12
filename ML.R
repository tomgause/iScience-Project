### ML.R
# Tom Gause
# 4/11/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

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

use_condaenv("keras-tf", required = T)

#set working directory to location of make_data_dev.R
setwd(dirname(getActiveDocumentContext()$path)) 

# Make all the data
train <- readRDS("../data/hindcast_subset_vermont_unedited.RDS")
#test <- readRDS("./data/test_vermont_2022-04-11_21-01-43.RDS")

train.filtered <- train %>%
  filter(lead == 1,
         fcst_cell == 259627,
         forecast_target <= ym(200501)) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k) %>%
  select(c(bias, fcst_tmp_k, forecast_target, target_month))

test.filtered <- train %>%
  filter(lead == 1,
         fcst_cell == 259627,
         forecast_target > ym(200801)) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k) %>%
  select(c(bias, fcst_tmp_k, forecast_target, target_month))
  
train.X <- train.filtered %>% select(-bias)
train.y <- train.filtered %>% select(bias)
test.X <- test.filtered %>% select(-bias)
test.y <- test.filtered %>% select(bias)

# Create LSTM
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_lstm(units = 32,return_sequences = TRUE) %>% 
  layer_lstm(units = 32,return_sequences = TRUE) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile. Include adam for better prediction
model %>%
  compile(optimizer = "adam",
          loss = "binary_crossentropy",
          metrics = c("acc"))

# Train model
history <- model %>% fit(train.x, train.y,
                         epochs = 25,
                         batch_size = 128,)
                         validation_split = 0.2

