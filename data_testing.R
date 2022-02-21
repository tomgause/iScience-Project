# Tom Gause
# 2/21/22
# loading and fiddling with data sample

library(tidyverse)
library(lubridate)
library(sf)
library(hyfo)
filename <- file.choose()
data <- readRDS(filename)

View(data %>% head(100))

#get data summary
data %>%
  filter(x == -95.25, 
         y == 49.50) %>%
  summarize(meanTemp = mean(obs_tmp_k),
            sd = sd(obs_tmp_k),
            min = min(obs_tmp_k),
            max = max(obs_tmp_k))

tom.data <- data %>%
  filter(forecast_target == 198301)
print(length(tom.data$forecast_target))

tom.data %>%
  if
  mutate(date = as_date(c(forecast_target, "01"), format="%Y%m%d"))
View(tom.data %>% head(100))

#adjust forecast_target to get observed averages for different months
tom.data %>%
  filter(forecast_target == 200801) %>%
  group_by(x, y) %>%
  summarize(meanTemp = mean(obs_tmp_k)) %>%
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = meanTemp))

#find forecast means
tom.data %>%
  filter(forecast_target == 198301) %>%
  group_by(x, y) %>%
  summarize(mean_obs_Temp = mean(obs_tmp_k),
    mean_pred_Temp = mean(fcst_tmp_k),
    mse_Temp = mean(mean_obs_Temp - mean_pred_Temp)^2) %>%
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = mse_Temp))

corrected.data <- NULL
corrected.data <- as.data.frame(corrected.data)
  
biasCorrect(
  
  #frc
  tom.data %>%
    filter(forecast_target == 198301) %>%
    group_by(x, y) %>%
    summarize(mean_obs_Temp = mean(obs_tmp_k),
              mean_pred_Temp = mean(fcst_tmp_k)) %>%
  mean_pred_Temp,
  
  #obs
  mean_obs_Temp,
  
  #method
  scaling
)


