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
#test <- readRDS("./data/test_vermont_2022-04-11_21-01-43.RDS")

train <- train[order(train$forecast_target),]

train.filtered <- train %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  filter(lead == 1,
         fcst_cell == 259627,
         forecast_target <= ym(200501)) %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  select(c(bias, index)) %>%
  as_tbl_time(index = index)

test.filtered <- train %>%
  filter(lead == 1,
         fcst_cell == 259627,
         forecast_target > ym(200801)) %>%
  tk_tbl() %>%
  mutate(bias = fcst_tmp_k - obs_tmp_k) %>%
  select(c(bias, fcst_tmp_k, forecast_target, target_month)) %>%
  as_tbl_time(index = forecast_target)

train.X <- train.filtered %>% select(-bias)
train.y <- train.filtered %>% select(bias)
test.X <- test.filtered %>% select(-bias)
test.y <- test.filtered %>% select(bias)



############################################################################
# Let's explore the data a little bit, get an idea of what things look like!

p1 <- train.filtered %>%
  ggplot(aes(index, bias)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "1982-2008"
  )

p2 <- train.filtered %>%
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

max_lag <- 12 * 50
# Let's plot our values.
train.filtered %>%
  tidy_acf("bias", lags = 0:max_lag) %>%
  ggplot(aes(lag, acf)) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  annotate("text", label = "10 Year Mark", x = 130, y = 0.8, 
           color = palette_light()[[2]], size = 6, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: CFSv2 Bias")

train.filtered %>%
  tidy_acf("bias", lags = 1:200) %>%
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
       subtitle = "Zoomed in on Lags 10 to 100")

sun_spots %>%
  tidy_acf(value, lags = 115:135) %>%
  ggplot(aes(lag, acf)) +
  geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
  geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
  geom_point(color = palette_light()[[1]], size = 2) +
  geom_label(aes(label = acf %>% round(2)), vjust = -1,
             color = palette_light()[[1]]) +
  annotate("text", label = "10 Year Mark", x = 121, y = 0.8, 
           color = palette_light()[[2]], size = 5, hjust = 0) +
  theme_tq() +
  labs(title = "ACF: Sunspots",
       subtitle = "Zoomed in on Lags 115 to 135")


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

