### rf testing
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 4/19/2022

library(tidyverse)
library(ggplot2)
library(dplyr)
library(ranger)
library(lubridate)


# get RF results
params <- readRDS("/Users/tomgause/Desktop/iScience_Project/RF/error_Vermont_single.RDS")
params <- params[-1,]

hist(params$best.mtry)
hist(params$best.nodesize)
hist(params$best.samplefrac)

params %>%
  summarise(mtry = mean(best.mtry),
            nodesize = mean(best.nodesize),
            samplefrac = mean(best.samplefrac))
