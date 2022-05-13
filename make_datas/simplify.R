### make_train_data.R
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# last edited 4/15/2022

# # old data. Copy and past this inside iScience_Project/data
# curl https://wsim-datasets.s3.us-east-2.amazonaws.com/hindcasts_usa.ta --output hindcast_data.tar;
# tar -xvf hindcast_data.tar
# 
# # new data. Copy and paste these lines inside iScience_Project/data
# curl https://wsim-datasets.s3.us-east-2.amazonaws.com/forecasts_usa.tar --output new_hindcast_data.tar;
# tar -xvf new_hindcast_data.tar

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(terra)
library(rstudioapi)



################################################################################
# TEST DATA
################################################################################

# #set working directory to location of make_data_dev.R
# data <- readRDS("/Users/tomgause/Desktop/iScience_data/test_vermont_2022-04-11_21-01-43.RDS")
# data %>% head(1)
# data <- data %>%
#   select(forecast_target,
#          fcst_cell,
#          x,
#          y,
#          target_month,
#          lead,
#          fcst_pr_m_day,
#          fcst_tmp_k,
#          obs_pr_m_day.x,
#          obs_tmp_k.x,
#          fcst_qm_pr_m_day,
#          fcst_qm_tmp_k,
#          forecast_timestamp,
#          #bias.t,
#          #bias.p,
#          elevation)
# 
# data <- data %>% 
#   rename(
#     obs_tmp_k = obs_tmp_k.x,
#     obs_pr_m_day = obs_pr_m_day.x 
#   )
# 
# data <- data %>%
#   mutate(bias.t = obs_tmp_k - fcst_tmp_k,
#          bias.p = obs_pr_m_day - fcst_pr_m_day)
# 
# saveRDS(data, "/Users/tomgause/Desktop/iScience_data/VT_test.RDS")




################################################################################
# TRAIN DATA
################################################################################

#set working directory to location of make_data_dev.R
data <- readRDS("/Users/tomgause/Desktop/iScience_data/train_subset_Vermont_2022-04-16_14-55-49.RDS")
data %>% head(10)
data <- data %>%
  select(forecast_target,
         fcst_cell,
         x,
         y,
         target_month,
         lead,
         fcst_pr_m_day,
         fcst_tmp_k,
         obs_pr_m_day,
         obs_tmp_k,
         #fcst_qm_pr_m_day,
         #fcst_qm_tmp_k,
         forecast_timestamp,
         bias.t,
         bias.p,
         elevation)

data <- data %>% 
  rename(
    forecast_timestamp = forecast_timestamp_rm,
  )

data %>%
  group_by(forecast_timestamp) %>%
  



saveRDS(data, "/Users/tomgause/Desktop/iScience_data/VT_train.RDS")
