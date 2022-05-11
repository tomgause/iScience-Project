### ML.R
# Tom Gause
# 4/11/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

packages <- c("tidyverse", "glue", "forcats", "timetk", "tidyquant",
              "tibbletime", "cowplot", "recipes", "rsample", "yardstick",
              "keras")
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

# Make all the data
train <- readRDS("./data/Vermont_train.RDS")
#test <- readRDS("./data/test_vermont_2022-04-11_21-01-43.RDS")

train <- train[order(train$forecast_target),]
#test <- test[order(test$forecast_target),]

data.all.predictions <- train %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  filter(fcst_cell == 259627) %>%
  mutate(value = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  select(c(value, index)) %>%
  as_tbl_time(index = index)
train.months <- length(unique(data$index)) # 335

############################################################################
# We haven't done any evaluation on the multiple predictions made
# each month. It's time to figure out how to deal with this data.
data <- data.all.predictions[1+24*(0:325),]
data %>% tail(10)
