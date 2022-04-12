### ML.R
# Tom Gause
# 4/11/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

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

# Make all the data
train <- readRDS("./data/hindcast_subset_vermont_unedited.RDS")
test <- readRDS("./data/test_vermont_2022-04-11_21-01-43.RDS")

train <- train[order(train$forecast_target),]
test <- test[order(test$forecast_target),]

train.filtered <- train %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  filter(lead == 1,
         fcst_cell == 259627) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  select(c(bias, index)) %>%
  as_tbl_time(index = index)
train.months <- length(unique(train.filtered$index)) # 335
unique(train.filtered$index) %>% tail(10)

test.filtered <- test %>%
  filter(lead == 1,
         fcst_cell == 259627) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k.x,
         index = as_date(forecast_target)) %>%
  select(c(bias, index)) %>%
  as_tbl_time(index = index)
test.months <- length(unique(test.filtered$index)) # 116
unique(test.filtered$index) %>% head(10)


# train.X <- train.filtered %>% select(-bias)
# train.y <- train.filtered %>% select(bias)
# test.X <- test.filtered %>% select(-bias)
# test.y <- test.filtered %>% select(bias)



############################################################################
# Let's explore the data a little bit, get an idea of what things look like!

p1 <- data %>%
  ggplot(aes(index, bias)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "1982-2010"
  )

p2 <- data %>%
  filter_time("start" ~ "1986") %>%
  ggplot(aes(index, bias)) +
  geom_line(color = palette_light()[[1]], alpha = 0.5) +
  geom_point(color = palette_light()[[1]]) +
  geom_smooth(method = "loess", span = 0.2, se = FALSE) +
  theme_tq() +
  labs(
    title = "1982 - 1986 (Zoomed In To Show Cycles)",
  )

p_title <- ggdraw() + 
  draw_label("CFSv2 Residuals", size = 18, fontface = "bold", colour = palette_light()[[1]])
plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))
  


############################################################################
# Let's evaluate the ACF and see if LSTM model is a good approach.
# Autocorrelation function, relation between time series of interest in 
# lagged versions of itself.

# This function takes tidy time series, extracts values, and returns
# ACF values in a tibble format.
tidy_acf <- function(data, value, lags = 0:20) {
  
  value_expr <- enquo(value)
  
  acf_values <- data %>%
    pull(value) %>%
    acf(lag.max = tail(lags, 1), plot = FALSE) %>%
    .$acf %>%
    .[,,1]
  
  ret <- tibble(acf = acf_values) %>%
    rowid_to_column(var = "lag") %>%
    mutate(lag = lag - 1) %>%
    filter(lag %in% lags)
  
  return(ret)
}

# Get con
alpha <- 0.95
max_lag <- 12 * 27
conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(max_lag)

# Let's plot our values with confidence
train.filtered %>%
  tidy_acf("bias", lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_hline(yintercept = conf.lims[2], size = 1, color = palette_light()[[5]]) + 
  geom_hline(yintercept = conf.lims[1], size = 1, color = palette_light()[[5]]) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "10 Year Mark", x = 130, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  annotate("text", label = "Confidence", x = 210, y = 0.18, 
           color = palette_light()[[5]], size = 4, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: CFSv2 Bias")

train.filtered %>%
  tidy_acf("bias", lags = 110:150) %>%
  ggplot(aes(lag, acf)) +
  #geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  #annotate("text", label = "10 Year Mark", x = 121, y = 0.8, 
           #color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: CFSv2 Bias",
       subtitle = "Zoomed in on Lags 110 to 150")

# Pull Optimal Lag
optimal_lag_setting <- train.filtered %>%
  tidy_acf("bias", lags = 110:150) %>%
  filter(acf == max(acf)) %>%
  pull(lag)
optimal_lag_setting # 281!



############################################################################
# Let's evaluate the ACF and see if LSTM model is a good approach.
# Autocorrelation function, relation between time series of interest in 
# lagged versions of itself.


# Create LSTM
model <- keras_model_sequential()
model %>%
  layer_embedding(input_dim = 500, output_dim = 32) %>%
  layer_lstm(units = 32,return_sequences = TRUE) %>% 
  layer_lstm(units = 32,return_sequences = TRUE) %>%
  layer_lstm(units = 32) %>%
  layer_dense(units = 1, activation = "sigmoid")

# Compile. Include adam for better prediction
model %>%
  compile(optimizer = "adam",
          loss = "binary_crossentropy",
          metrics = c("acc"))

# Train model
history <- model %>% fit(train.x, train.y,
                         epochs = 25,
                         batch_size = 128,)
                         validation_split = 0.2

