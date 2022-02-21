# Tom Gause
# 2/21/22
# loading and fiddling with data sample

library(tidyverse)
library(lubridate)
library(sf)

filename <- file.choose()
data <- readRDS(filename)

#Evil!!!
#View(data)

View(data %>% head(100))

#get data summary
data %>%
  filter(x == -95.25, 
         y == 49.50) %>%
  summarize(meanTemp = mean(obs_tmp_k),
            sd = sd(obs_tmp_k),
            min = min(obs_tmp_k),
            max = max(obs_tmp_k))

#broken
tom.data <- data %>%
  mutate(time = as_datetime(forecast_target,
                            format = "%Y%m"))
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

tom.data %>%
  group_by(x, y) %>%
  summarize(meanTemp = mean(obs_tmp_k)) %>%
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = meanTemp))

tom.data %>%
  filter(time < as_datetime("2000-12-03")) %>%
  group_by(x, y) %>%
  summarize(meanTemp = mean(obs_tmp_k),
            sdTemp = sd(obs_tmp_k)) %>%
  arrange(sdTemp)

tom.data %>%
  filter(x == -95.25, 
         y == 49.50) %>%
  summarize(meanTemp = mean(obs_tmp_k),
            sd = sd(obs_tmp_k),
            min = min(obs_tmp_k),
            max = max(obs_tmp_k))