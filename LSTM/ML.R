### ML.R
# Tom Gause
# 4/11/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

## Default repo
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org" 
       options(repos=r)
})

packages <- c("tidyverse", "glue", "forcats", "timetk", "tidyquant",
              "tibbletime", "cowplot", "recipes", "rsample", "yardstick",
              "keras")
install.packages(setdiff(packages, rownames(installed.packages()))) 

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
train <- readRDS("/storage/tgause/iScience_tom/iScience_Project/data/Vermont_train.RDS")
#test <- readRDS("./data/test_vermont_2022-04-11_21-01-43.RDS")

train <- train[order(train$forecast_target),]
#test <- test[order(test$forecast_target),]

data.all.predictions <- train %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  filter(lead == 1,
         fcst_cell == 259627) %>%
  mutate(value = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  select(c(value, index)) %>%
  as_tbl_time(index = index)
train.months <- length(unique(data$index)) # 335

# To start, we'll just pick the first prediction made for each month
data <- data.all.predictions[1+24*(0:325),]
data %>% tail(10)

# test.filtered <- test %>%
#   filter(lead == 1,
#          fcst_cell == 259627) %>%
#   mutate(bias = fcst_tmp_k - obs_tmp_k.x,
#          index = as_date(forecast_target)) %>%
#   select(c(bias, index)) %>%
#   as_tbl_time(index = index)
# test.months <- length(unique(test.filtered$index)) # 116
# unique(test.filtered$index) %>% head(10)


# train.X <- train.filtered %>% select(-bias)
# train.y <- train.filtered %>% select(bias)
# test.X <- test.filtered %>% select(-bias)
# test.y <- test.filtered %>% select(bias)



############################################################################
# Let's explore the data a little bit, get an idea of what things look like!

p1 <- data %>%
  ggplot(aes(index, value)) +
  geom_point(color = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  labs(
    title = "1982-2010"
  )

p2 <- data %>%
  filter_time("start" ~ "1984") %>%
  ggplot(aes(index, value)) +
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
data %>%
  tidy_acf("value", lags = 0:max_lag) %>%
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

data %>%
  tidy_acf("value", lags = 40:60) %>%
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
       subtitle = "Zoomed in on Lags 40 to 60")

# Pull Optimal Lag
optimal_lag_setting <- data %>%
  tidy_acf("value", lags = 40:60) %>%
  filter(acf == max(acf)) %>%
  pull(lag)
optimal_lag_setting # 47



############################################################################
# Let's create a cross validation plan by offsetting the window
# used to select sequential sub-samples. We're essentially dealing
# with the fact that there's no future test data available by creating
# multiple synthetic "fututures"--this process is called "backtesting"
# We'll use a procedure that uses the rolling_origin() function



nrow(data) # 326, 216 for training and 108 for validation
periods_train <- 100
periods_test  <- 1
skip_span     <- 18 # splits samples into 6 sets that span history

rolling_origin_resamples <- rsample::rolling_origin(
  data,
  initial    = periods_train,
  assess     = periods_test,
  cumulative = FALSE,
  skip       = skip_span
)


rolling_origin_resamples

# These custom functions will visualize our resamples. Plot_split()
# plots one of the resampling units. We'll use these to visualize
# all plots together.


# Plotting function for a single split
plot_split <- function(split, expand_y_axis = TRUE, 
                       alpha = 1, size = 1, base_size = 14) {
  # Manipulate data
  train_tbl <- training(split) %>%
    add_column(key = "training") 
  
  test_tbl  <- testing(split) %>%
    add_column(key = "testing") 
  
  data_manipulated <- bind_rows(train_tbl, test_tbl) %>%
    as_tbl_time(index = index) %>%
    mutate(key = fct_relevel(key, "training", "testing"))
  
  # Collect attributes
  train_time_summary <- train_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  test_time_summary <- test_tbl %>%
    tk_index() %>%
    tk_get_timeseries_summary()
  
  # Visualize
  g <- data_manipulated %>%
    ggplot(aes(x = index, y = value, color = key)) +
    geom_line(size = size, alpha = alpha) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    labs(
      title    = glue("Split: {split$id}"),
      subtitle = glue("{train_time_summary$start} to ", 
                      "{test_time_summary$end}"),
      y = "", x = ""
    ) +
    theme(legend.position = "none") 
  
  if (expand_y_axis) {
    
    data_time_summary <- data %>% 
      tk_index() %>% 
      tk_get_timeseries_summary()
    
    g <- g +
      scale_x_date(limits = c(data_time_summary$start, 
                              data_time_summary$end))
  }
  
  g
}

# Let's look at our first slice and make sure things are working...
rolling_origin_resamples$splits[[1]] %>%
  plot_split(expand_y_axis = TRUE) + 
  theme(legend.position = "bottom")



# The second function, plot_sampling_plan(), scales the plot_split()
# function to all of the samples using purrr and cowplot.

# Plotting function that scales to all splits 
plot_sampling_plan <- function(sampling_tbl, expand_y_axis = TRUE, 
                               ncol = 3, alpha = 1, size = 1, base_size = 14, 
                               title = "Sampling Plan") {
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map(splits, plot_split, 
                          expand_y_axis = expand_y_axis,
                          alpha = alpha, base_size = base_size))
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  p_title <- ggdraw() + 
    draw_label(title, size = 14, fontface = "bold", 
               colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, 
                 rel_heights = c(0.05, 1, 0.05))
  
  g
  
}

# Finally, we're ready to plot everything.
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = T, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Rolling Origin Sampling Plan")

# Let's say y_axis false and zoom in on our samples.
rolling_origin_resamples %>%
  plot_sampling_plan(expand_y_axis = F, ncol = 3, alpha = 1, size = 1, base_size = 10, 
                     title = "Backtesting Strategy: Zoomed In")




############################################################################
# Let's develop an LSTM model on a single sample (the most recent)
# slice. We'll apply this model to all samples to investigate
# performance.

split    <- rolling_origin_resamples$splits[[12]]
split_id <- rolling_origin_resamples$id[[12]]
nrow(example_split)

plot_split(split, expand_y_axis = FALSE, size = 0.5) +
  theme(legend.position = "bottom") +
  ggtitle(glue("Split: {split_id}"))

# First, let’s combine the training and testing data sets into a 
# single data set with a column key that specifies what set they came 
# from (either “training” or “testing)”.
df_trn <- training(split)
df_tst <- testing(split)
df <- bind_rows(
  df_trn %>% add_column(key = "training"),
  df_tst %>% add_column(key = "testing")
) %>% 
  as_tbl_time(index = index)
df



##########################
# PREPROCESSING with recipes
# LSTM performs better if we center and scale our data.
# We'll use recipes to center and scale, as well as using 
# step_sqrt to reduce variance and remove outliers.

rec_obj <- recipe(value ~ ., df) %>%
  step_normalize(value) %>%
  prep()

df_sqrt <- df %>%
  mutate(value = sign(value) * sqrt(abs(value)))
df_processed_tbl <- bake(rec_obj, df)


# Let's capture the original center and scale so we can 
# invert the steps after modelling. We can undo the square
# root step by squaring the back-transformed data.
center_history <- rec_obj$steps[[1]]$means["value"]
scale_history  <- rec_obj$steps[[1]]$sds["value"]

##########################
# LSTM Plan
# 
# Tensor Format:
#   -Predictors (X) must be 3D array with dimensions; [samples, timesteps, features]
#    First dimension is length of values, second is number of lags,
#    and third is number of predictors (1, univariate).
#   -Targets(Y) must be 2D array with dimensions ; [samples, timesteps]
# 
# Training/Testing:
#   -Training and testing length must be evenly divisible
#
# Batch Size:
#   -Number of training examples in 1 forward/backward pass of RNN before weight update
#   -Must be evenly divisible into both training and testing lengths
# 
# Time Steps:
#   -Number of lags included in training/testing set
#
# Epochs:
#   -Total number of forward/backward pass iterations
#   -Typically improves performance unless overfitting occurs
#
# We'll select a prediction window of 12 months. We'll select a batch size of
# 24 units, which evenly divides into number of testing and training observations.
# We'll select time_steps=12, ie 12 lags, for now. We'll do 100 epochs.
lag_setting  <- 1 # = nrow(df_tst)
batch_size   <- 1
train_length <- 101
tsteps       <- 1
epochs       <- 300

nrow(df_tst)

# Now we can set up the training and testing sets in the correct formats.
# Training Set
lag_train_tbl <- df_processed_tbl %>%
  mutate(value_lag = lag(value, n = lag_setting)) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "training") %>%
  tail(train_length)
x_train_vec <- lag_train_tbl$value_lag
x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
y_train_vec <- lag_train_tbl$value
y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
# Testing Set
lag_test_tbl <- df_processed_tbl %>%
  mutate(
    value_lag = lag(value, n = lag_setting)
  ) %>%
  filter(!is.na(value_lag)) %>%
  filter(key == "testing")
x_test_vec <- lag_test_tbl$value_lag
x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
y_test_vec <- lag_test_tbl$value
y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))


################3
# Time to build the model.
# We'll use keras_model_sequential and add layers like stacking bricks.
# We're use two LSTM layers with 50 units each.
model <- keras_model_sequential()
model %>%
  layer_lstm(units            = 50, 
             input_shape      = c(tsteps, 1), 
             batch_size       = batch_size,
             return_sequences = TRUE, 
             stateful         = TRUE) %>% 
  layer_lstm(units            = 50, 
             return_sequences = FALSE, 
             stateful         = TRUE) %>% 
  layer_dense(units = 1)
model %>% 
  compile(loss = 'mae', optimizer = 'adam')
model


##############
# Time to fit! We'll use a for loop.
for (i in 1:epochs) {
  model %>% fit(x          = x_train_arr, 
                y          = y_train_arr, 
                batch_size = batch_size,
                epochs     = 1, 
                verbose    = 1, 
                shuffle    = FALSE)
  
  model %>% reset_states()
  cat("Epoch: ", i)
  
}

##############
# Now we can make predictions on the test set, x_test_arr, using the predict()
# function. We can retransform our predictions and combine the predictions
# with the original data!

# Make Predictions
pred_out <- model %>% 
  predict(x_test_arr, batch_size = batch_size) %>%
  .[,1] 
# Retransform values
pred_tbl <- tibble(
  index   = lag_test_tbl$index,
  value   = (pred_out * scale_history + center_history)^2
) 
# Combine actual data with predictions
tbl_1 <- df_trn %>%
  add_column(key = "actual")
tbl_2 <- df_tst %>%
  add_column(key = "actual")
tbl_3 <- pred_tbl %>%
  add_column(key = "predict")
# Create time_bind_rows() to solve dplyr issue
time_bind_rows <- function(data_1, data_2, index) {
  index_expr <- enquo(index)
  bind_rows(data_1, data_2) %>%
    as_tbl_time(index = !! index_expr)
}
ret <- list(tbl_1, tbl_2, tbl_3) %>%
  reduce(time_bind_rows, index = index) %>%
  arrange(key, index) %>%
  mutate(key = as_factor(key))
ret

##############
# Let's assess the performance on this single split
#We can use the yardstick package to assess performance using the rmse()
#function, which returns the root mean squared error (RMSE).
calc_rmse <- function(prediction_tbl) {
  
  rmse_calculation <- function(data) {
    data %>%
      spread(key = key, value = value) %>%
      select(-index) %>%
      filter(!is.na(predict)) %>%
      rename(
        truth    = actual,
        estimate = predict
      ) %>%
      rmse(truth, estimate)
  }
  
  safe_rmse <- possibly(rmse_calculation, otherwise = NA)
  
  safe_rmse(prediction_tbl)
  
}
calc_rmse(ret) # not useful yet. We need to visualize.

# Setup single plot function
plot_prediction <- function(data, id, alpha = 1, size = 2, base_size = 14) {
  
  rmse_val <- calc_rmse(data)
  print(rmse_val)
  
  g <- data %>%
    ggplot(aes(index, value, color = key)) +
    geom_point(alpha = alpha, size = size) +
    theme_tq(base_size = base_size) +
    scale_color_tq() +
    theme(legend.position = "none") +
    labs(
      #title = glue("{id}, RMSE: {round(rmse_val, digits = 1)}"),
      x = "", y = ""
    )

  return(g)
}

ret %>% 
  plot_prediction(id = split_id, alpha = 0.65) +
  theme(legend.position = "bottom")




############################################################################
# Backtesting on all 12 samples.
# Let's create a prediction function that can be mapped to the sampling plan 
# data contained in rolling_origin_resamples.

# This function looks terrifying, but it's actually pretty simple.
# I just copied the setup, processing, planning, splitting, modeling,
# fitting, predicting, and retransformation steps into a loop.
# We basically want to see how robust our model is.
predict_keras_lstm <- function(split, epochs = 300, ...) {
  
  lstm_prediction <- function(split, epochs, ...) {
    
    # Data Setup
    df_trn <- training(split)
    df_tst <- testing(split)
    
    df <- bind_rows(
      df_trn %>% add_column(key = "training"),
      df_tst %>% add_column(key = "testing")
    ) %>% 
      as_tbl_time(index = index)
    
    # Preprocessing
    rec_obj <- recipe(value ~ ., df) %>%
      step_normalize(value) %>%
      prep()
    
    df_sqrt <- df %>%
      mutate(value = sign(value) * sqrt(abs(value)))
    df_processed_tbl <- bake(rec_obj, df)
    
    center_history <- rec_obj$steps[[1]]$means["value"]
    scale_history  <- rec_obj$steps[[1]]$sds["value"]
    
    # LSTM Plan
    lag_setting  <- 1 # = nrow(df_tst)
    batch_size   <- 1
    train_length <- 101
    tsteps       <- 1
    epochs       <- epochs
    
    # Train/Test Setup
    lag_train_tbl <- df_processed_tbl %>%
      mutate(value_lag = lag(value, n = lag_setting)) %>%
      filter(!is.na(value_lag)) %>%
      filter(key == "training") %>%
      tail(train_length)
    
    x_train_vec <- lag_train_tbl$value_lag
    x_train_arr <- array(data = x_train_vec, dim = c(length(x_train_vec), 1, 1))
    
    y_train_vec <- lag_train_tbl$value
    y_train_arr <- array(data = y_train_vec, dim = c(length(y_train_vec), 1))
    
    lag_test_tbl <- df_processed_tbl %>%
      mutate(
        value_lag = lag(value, n = lag_setting)
      ) %>%
      filter(!is.na(value_lag)) %>%
      filter(key == "testing")
    
    x_test_vec <- lag_test_tbl$value_lag
    x_test_arr <- array(data = x_test_vec, dim = c(length(x_test_vec), 1, 1))
    
    y_test_vec <- lag_test_tbl$value
    y_test_arr <- array(data = y_test_vec, dim = c(length(y_test_vec), 1))
    
    # LSTM Model
    model <- keras_model_sequential()
    
    model %>%
      layer_lstm(units            = 50, 
                 input_shape      = c(tsteps, 1), 
                 batch_size       = batch_size,
                 return_sequences = TRUE, 
                 stateful         = TRUE) %>% 
      layer_lstm(units            = 50, 
                 return_sequences = FALSE, 
                 stateful         = TRUE) %>% 
      layer_dense(units = 1)
    
    model %>% 
      compile(loss = 'mae', optimizer = 'adam')
    
    # Fitting LSTM
    for (i in 1:epochs) {
      model %>% fit(x          = x_train_arr, 
                    y          = y_train_arr, 
                    batch_size = batch_size,
                    epochs     = 1, 
                    verbose    = 1, 
                    shuffle    = FALSE)
      
      model %>% reset_states()
      cat("Epoch: ", i)
      
    }
    
    # Predict and Return Tidy Data
    # Make Predictions
    pred_out <- model %>% 
      predict(x_test_arr, batch_size = batch_size) %>%
      .[,1] 
    
    # Retransform values
    pred_tbl <- tibble(
      index   = lag_test_tbl$index,
      value   = (pred_out * scale_history + center_history)^2
    ) 
    
    # Combine actual data with predictions
    tbl_1 <- df_trn %>%
      add_column(key = "actual")
    
    tbl_2 <- df_tst %>%
      add_column(key = "actual")
    
    tbl_3 <- pred_tbl %>%
      add_column(key = "predict")
    
    # Create time_bind_rows() to solve dplyr issue
    time_bind_rows <- function(data_1, data_2, index) {
      index_expr <- enquo(index)
      bind_rows(data_1, data_2) %>%
        as_tbl_time(index = !! index_expr)
    }
    
    ret <- list(tbl_1, tbl_2, tbl_3) %>%
      reduce(time_bind_rows, index = index) %>%
      arrange(key, index) %>%
      mutate(key = as_factor(key))
    
    return(ret)
    
  }
  
  safe_lstm <- possibly(lstm_prediction, otherwise = NA)
  
  safe_lstm(split, epochs, ...)
  
}

# Let's make the predictions.
sample_predictions_lstm_tbl <- rolling_origin_resamples %>%
  mutate(predict = map(splits, predict_keras_lstm, epochs = 300))

# Save!
save(sample_predictions_lstm_tbl,
     file = "/Users/tomgause/Desktop/iScience_data/sample_predictions_lstm_tbl.RDS")

# Let's get the sample RMSE
sample_rmse_tbl <- sample_predictions_lstm_tbl %>%
  mutate(rmse = map(predict, calc_rmse)) %>%
  select(id, rmse)
sample_rmse_tbl <- sample_rmse_tbl %>% unnest(rmse) %>%
  mutate(rmse = .estimate) %>%
  select(id, rmse)

# Let's assess the RMSE with a table
sample_rmse_tbl %>%
  ggplot(aes(rmse)) +
  geom_histogram(aes(y = ..density..), fill = palette_light()[[1]], bins = 16) +
  geom_density(fill = palette_light()[[1]], alpha = 0.5) +
  theme_tq() +
  ggtitle("Histogram of RMSE")

# And summary...
sample_rmse_tbl %>%
  summarize(
    mean_rmse = mean(rmse),
    sd_rmse   = sd(rmse)
  )

# Let's visualize everything!
plot_predictions <- function(sampling_tbl, predictions_col, 
                             ncol = 3, alpha = 1, size = 2, base_size = 14,
                             title = "Backtested Predictions") {
  
  predictions_col_expr <- enquo(predictions_col)
  
  # Map plot_split() to sampling_tbl
  sampling_tbl_with_plots <- sampling_tbl %>%
    mutate(gg_plots = map2(!! predictions_col_expr, id, 
                           .f        = plot_prediction, 
                           alpha     = alpha, 
                           size      = size, 
                           base_size = base_size)) 
  
  # Make plots with cowplot
  plot_list <- sampling_tbl_with_plots$gg_plots 
  
  p_temp <- plot_list[[1]] + theme(legend.position = "bottom")
  legend <- get_legend(p_temp)
  
  p_body  <- plot_grid(plotlist = plot_list, ncol = ncol)
  
  
  
  p_title <- ggdraw() + 
    draw_label(title, size = 18, fontface = "bold", colour = palette_light()[[1]])
  
  g <- plot_grid(p_title, p_body, legend, ncol = 1, rel_heights = c(0.05, 1, 0.05))
  
  return(g)
  
}

sample_predictions_lstm_tbl %>%
  plot_predictions(predictions_col = predict, alpha = 0.5, size = 1, base_size = 10,
                   title = "Keras Stateful LSTM: Backtested Predictions")

############################################################################
# Save experiment plots

plots.directory <- "./plots"
# Get all plots
plots.dir.path <- list.files(tempdir(), pattern="rs-graphics", full.names = TRUE); 
plots.png.paths <- list.files(plots.dir.path, pattern=".png", full.names = TRUE)

# Copy all plots to directory of choice
file.copy(from=plots.png.paths, to=plots.directory)

# All of the plots have ugly auto-generated names, so we want to rename them
# with integers in the order they were generated
plots.png.detials <- file.info(plots.png.paths)
plots.png.detials <- plots.png.detials[order(plots.png.detials$mtime),]
sorted.png.names <- gsub(plots.dir.path, plots.directory, row.names(plots.png.detials), fixed=TRUE)
numbered.png.names <- paste0(plots.directory, "/", 1:length(sorted.png.names), ".png")

# Rename all the .png files as: 1.png, 2.png, 3.png, and so on.
file.rename(from=sorted.png.names, to=numbered.png.names)