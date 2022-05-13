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
  mutate(base = fcst_tmp_k - obs_tmp_k,
         qm = fcst_qm_tmp_k - obs_tmp_k) %>%
  dplyr::group_by(forecast_target, fcst_cell, lead) %>%
  mutate(base = mean(base),
         qm = mean(qm)) %>%
  dplyr::ungroup() %>%
  select(c(base, qm, forecast_target, fcst_cell, lead))
test.months <- length(unique(test.all.predictions$forecast_target))

# We have 24x duplicate data for each lead-cell-index.
# By taking the unique data only, we fix this issue.
test <- test.all.predictions %>% unique()
test %>% head(2)

max.test <- max(test$base)
min.test <- min(test$base)
test.scaled <- test %>%
  mutate(base = ((base - min.test)/(max.test - min.test))^2,
         qm = ((qm - min.test)/(max.test - min.test))^2)

print("BASE:")
print(mean(test.scaled$base))
print("QM:")
print(mean(test.scaled$qm))
print("BEST MODEL:")
print(0.0142196)