### ML.R
# Tom Gause
# 4/11/2022

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
test.all.predictions <- test %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell, fcst_qm_tmp_k)) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k,
         qm_bias = fcst_qm_tmp_k - obs_tmp_k) %>%
  dplyr::group_by(forecast_target, fcst_cell, lead) %>%
  mutate(bias = mean(bias),
         qm_bias = mean(qm_bias)) %>%
  dplyr::ungroup() %>%
  select(c(bias, qm_bias, forecast_target))
test.months <- length(unique(test.all.predictions$forecast_target))

# We have 24x duplicate data for each lead-cell-index.
# By taking the unique data only, we fix this issue.
test <- test.all.predictions %>% unique()
test %>% head(2)

max.test <- max(test$bias)
min.test <- min(test$bias)
test.scaled <- test %>%
  mutate(bias = (bias - min.test)/(max.test - min.test),
         qm_bias = (qm_bias - min.test)/(max.test - min.test))

error <- test.scaled %>%
       mutate(RNN_error = (bias - 0.3916679)^2,
              qm_error = (bias - qm_bias)^2,
              bias = bias^2) %>%
       select(RNN_error, qm_error, bias)

print("BASE:")
print(mean(error$bias))
print("QM:")
print(mean(error$qm_error))
print("BEST MODEL:")
print(mean(error$RNN_error))
print(0.0142196)