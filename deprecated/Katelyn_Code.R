---
title: "Katelyn_Code"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

```{r}
library(tidyverse)
library(lubridate)

x <- hindcasts_month_1_lead_1$forecast_timestamp[1]
as_datetime(x, )

hindcasts_month_1_lead_1 %>%
  head(10) %>%
  mutate(time = as_datetime(forecast_timestamp, format = "%Y%m%d%H")) %>%
  pull(time)

hindcasts_month_1_lead_1 %>%
  mutate(time = as_datetime(forecast_timestamp, format = "%Y%m%d%H")) %>%
  filter(time == as_datetime("2000-12-22")) %>%
  ggplot() +
  geom_point(aes(x = x, y = y, color = obs_tmp_k))

hindcasts_month_1_lead_1 %>%
  mutate(time = as_datetime(forecast_timestamp, format = "%Y%m%d%H")) %>%
  count(forecast_timestamp)

###Data Validation ### 

hindcast_data<- hindcasts_month_1_lead_1 %>% 
  mutate(time = as_datetime(forecast_timestamp, format = "%Y%m%d%H"))

###All location  
hindcast_data %>%  
  #filter (cell <240000) %>% 
  ggplot()+
  geom_line(aes(x = time, 
                y = obs_tmp_k,
                color = as.factor(cell)),
            show.legend = FALSE)+
  theme_bw()+
  ylab("Temperature")

###Function to generate time series graph### 
time_series_map <- function(location){
  hindcast_data %>%  
    filter(cell==location) %>% 
    ggplot()+
    geom_line(aes(x = time, 
                  y = obs_tmp_k))+
    theme_bw()+
    ylim(0,300)+
    ylab("Temperature")+
    ggtitle(as.character(location))
}

time_series_map(239331) 

##test

hindcast_data %>% 
  filter(x==-95,y == 29.76)


```

