### rf testing
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 4/19/2022

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ranger)
library(lubridate)


# get RF results
params <- readRDS("/Users/tomgause/Desktop/iScience_Project/RF/rf_params_2022-04-25_00-51-43.RDS")
params <- params[-1,]

hist(params$best.mtry)

params %>%
  summarise(mtry = mean(best.mtry))
