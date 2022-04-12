### gam_testing.R
# Tom Gause
# 4/11/2022

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(terra)
library(rstudioapi)
library(mgcv)
library(ggplot2)


########################################
#read and concat all data
########################################

#set working directory to location of make_data_dev.R
setwd(dirname(getActiveDocumentContext()$path)) 

train <- readRDS("./data/hindcast_subset_vermont_unedited.RDS")
test <- readRDS("./data/test_vermont_2022-04-11_21-01-43.RDS")

lead.1.cell.1 <- train %>%
  filter(lead == 1,
         fcst_cell == 259627,
         forecast_target <= ym(200801)) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k)

# Plot it!
lead.1.cell.1 %>% ggplot(mapping = aes(x=forecast_target, y=bias)) +
  geom_line(aes(group = fcst_cell))

# Take the rest of the data. We'll use this as our test set.
test.filtered <- train %>%
  filter(lead == 1,
         fcst_cell == 259627,
         forecast_target > ym(200501)) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k.x)

# Plot it!
lead.1.cell.1 %>% ggplot(mapping = aes(x=forecast_target, y=bias)) +
  geom_line(aes(group = fcst_cell))

mod_gam <- gam(bias ~ forecast_target +
                      fcst_tmp_k +
                      target_month, data=lead.1.cell.1)
summary(mod_gam)

fits <- predict(mod_gam, newdata=test.filtered, type='response', se=T)

predicts <- data.frame(test.filtered, fits) %>% 
  mutate(lower = fit - 1.96*se.fit,
         upper = fit + 1.96*se.fit)

plot_mod_gam_response <- ggplot(aes(x=(obs_tmp_k - fc),y=fit), data=predicts) +
  geom_ribbon(aes(ymin = lower, ymax=upper), fill='gray90') +
  geom_line(color='#00aaff')



# filter to only include lead 1
lead.1 <- train %>%
  filter(lead == 1)

lead.1.test <- lead.1 %>%
  filter(fcst_cell == 259627) #forecast_target < ym("198303"))

var.data <- lead.1.test %>%
  group_by(forecast_target, fcst_cell) %>%
  mutate(bias = mean(fcst_tmp_k - obs_tmp_k)) %>%
  ungroup()

test.data %>% ggplot(mapping = aes(x=forecast_target, y=bias)) +
  geom_line(aes(group = fcst_cell))