# Tom Gause
# 2/21/22
# loading and fiddling with data sample

library(tidyverse)
library(lubridate)
library(dplyr)
library(hyfo)
filename <- file.choose()
data <- readRDS(filename)

View(data %>% head(100))

#Dallas: 32.8, -96.8
data %>%
  filter(x == -97, 
         y == 33) %>%
  summarize(meanTemp = mean(obs_tmp_k),
            sd = sd(obs_tmp_k),
            min = min(obs_tmp_k),
            max = max(obs_tmp_k))

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

#tom.data %>%
 # mutate(date = as_date(c(forecast_target, "01"), format="%Y%m%d"))
View(tom.data %>% head(100))

data %>%
  head(10) %>%
  mutate(time = as_datetime(forecast_timestamp, format = "%Y%m%d%H")) %>%
  pull(time)

#adjust forecast_target to get observed averages for different months
tom.data %>%
  #filter(forecast_target == 200801) %>%
  group_by(x, y) %>%
  summarize(meanTemp = mean(obs_tmp_k)) %>%
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = meanTemp))

#find forecast means
x <- data %>%
  filter(forecast_target == 198401) %>%
  group_by(x, y) %>%
  summarize(mean_obs_Temp = mean(obs_tmp_k),
    mean_pred_Temp = mean(fcst_tmp_k),
    mse_Temp = mean(mean_obs_Temp - mean_pred_Temp)^2) %>%
  ggplot() +
  geom_point(aes(x = x,
                 y = y,
                 color = mean_obs_Temp))

ggsave("pred_1983.png", plot = x)

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

##Acadia Hegedus
##2/24/22

#Data Exploration
acadia.data <- data[sample(nrow(data), 5000),] #random sample without replacement

#Creating our target variable
acadia.data <- acadia.data %>%
  mutate(error_temp = obs_tmp_k-fcst_tmp_k)%>%
  mutate(error_precip = obs_pr_m_day -fcst_pr_m_day)

options(scipen = 1000)
hist(acadia.data$error_precip)
hist(acadia.data$error_temp)

print(mean(acadia.data$errorsq_precip))
print(mean(acadia.data$errorsq_temp)) 

#CFSv2 tends to overpredict both precipitation and temperature

#Exploring a single pixel
data%>%
  filter(x == -103.50, y == 29.25)%>%
  ggplot(mapping = aes(x = forecast_target, y = fcst_tmp_k)) +
  geom_point()

#look at mean of 24 forecasts
mean.one.pixel.data <- data%>%
  filter(x == -103.50, y == 29.25)%>%
  group_by(forecast_target)%>%
  mutate(mean_temp = mean(fcst_tmp_k))%>%
  mutate(mean_precip = mean(fcst_pr_m_day))

#graph mean temp over time for one pixel
mean.one.pixel.data%>%
  ggplot(mapping = aes(x = forecast_target, y = mean_temp)) +
  geom_point()

#graph mean precip over time for one pixel
mean.one.pixel.data%>%
  ggplot(mapping = aes(x = forecast_target, y = mean_precip)) +
  geom_point()

data%>%
  filter(x == -103.50, y == 29.25, forecast_target == 200501)%>%
  group_by(forecast_timestamp)
#why am I only seeing 24 predictions at a single pixel/forecast_target?
#I thought we would be seeing 28?

#checking for NAs
sum(is.na(data))
#isnt it fabulous to have no missing data?

data%>%
  filter(x == -103.50, y == 29.25, forecast_target == 200501)

## test hiiiii

## my name is Acadia
