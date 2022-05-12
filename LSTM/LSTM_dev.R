### ML.R
# Tom Gause
# 4/11/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

packages <- c("tidyverse", "glue", "forcats", "timetk", "tidyquant",
              "tibbletime", "cowplot", "recipes", "rsample", "yardstick",
              "keras", "tensorflow")
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
library(tensorflow)

setwd("/storage/tgause/iScience_tom/iScience_Project/LSTM")

# Make all the data
#train <- readRDS("/Users/tomgause/Desktop/iScience_tom/iScience_Project/data/VT_train.RDS")
#test <- readRDS("/Users/tomgause/Desktop/iScience_tom/iScience_Project/data/VT_test.RDS")
#train <- readRDS("C:/Users/tgause/iScience_Project/data/train_subset_Vermont_2022-04-16_14-55-49.RDS")
train <- readRDS("/storage/tgause/iScience_tom/iScience_Project/data/VT_train.RDS")
test <- readRDS("/storage/tgause/iScience_tom/iScience_Project/data/VT_test.RDS")

train <- train[order(train$forecast_target),]
test <- test[order(test$forecast_target),]
gc()

train.all.predictions <- train %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  mutate(value = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  dplyr::group_by(forecast_target, fcst_cell, lead) %>%
  mutate(value = mean(value)) %>%
  dplyr::ungroup() %>%
  select(c(value, index, fcst_cell, lead)) %>%
  as_tbl_time(index = index, lead = lead, fcst_cell = fcst_cell)
train.months <- length(unique(train.all.predictions$index))# 335 before filter

# We'll be using a batch size of 32, so let's reduce this to 320.
#train.all.predictions <- train.all.predictions %>%
#  filter(index %in% train.months)

test.all.predictions <- test %>%
  select(c(fcst_tmp_k, obs_tmp_k, forecast_target, lead, fcst_cell)) %>%
  mutate(value = fcst_tmp_k - obs_tmp_k,
         index = as_date(forecast_target)) %>%
  dplyr::group_by(forecast_target, fcst_cell, lead) %>%
  mutate(value = mean(value)) %>%
  dplyr::ungroup() %>%
  select(c(value, index, fcst_cell, lead)) %>%
  as_tbl_time(index = index, lead = lead, fcst_cell = fcst_cell)
test.months <- length(unique(test.all.predictions$index))

# We have 24x duplicate data for each lead-cell-index.
# By taking the unique data only, we fix this issue.
train <- train.all.predictions %>% unique()
test <- test.all.predictions %>% unique()

rm(train.all.predictions)#, test.all.predictions)
gc()

train %>% head(2)
test %>% head(2)

# ############################################################################
# ############################################################################
# # Let's take a look at the data!

# p1 <- train %>%
#   filter (lead == 1, fcst_cell == 259627) %>%
#   ggplot(aes(index, value)) +
#   geom_point(color = palette_light()[[1]], alpha = 0.5) +
#   theme_tq() +
#   labs(
#     title = "1982-2010, lead=1, fcst_cell=259627"
#   )

# p2 <- train %>%
#   filter_time("start" ~ "1984") %>%
#   filter(lead == 1, fcst_cell == 259627) %>%
#   ggplot(aes(index, value)) +
#   geom_line(color = palette_light()[[1]], alpha = 0.5) +
#   geom_point(color = palette_light()[[1]]) +
#   geom_smooth(method = "loess", span = 0.2, se = FALSE) +
#   theme_tq() +
#   labs(
#     title = "1982 - 1986 (Zoomed In To Show Cycles), lead=1, fcst_cell=259627",
#   )

# p_title <- ggdraw() +
#   draw_label("CFSv2 Residuals", size = 18, fontface = "bold", colour = palette_light()[[1]])
# plot_grid(p_title, p1, p2, ncol = 1, rel_heights = c(0.1, 1, 1))


# ############################################################################
# # Let's evaluate the ACF and see if LSTM model is a good approach.
# # Autocorrelation function, relation between time series of interest in
# # lagged versions of itself.

# # This function takes tidy time series, extracts values, and returns
# # ACF values in a tibble format.

# tidy_acf <- function(data, value, lags = 0:20) {
  
#   value_expr <- enquo(value)
  
#   acf_values <- data %>%
#     pull(value) %>%
#     acf(lag.max = tail(lags, 1), plot = FALSE) %>%
#     .$acf %>%
#     .[,,1]
  
#   ret <- tibble(acf = acf_values) %>%
#     rowid_to_column(var = "lag") %>%
#     mutate(lag = lag - 1) %>%
#     filter(lag %in% lags)
  
#   return(ret)
# }

# # Get confidence
# alpha <- 0.95
# max_lag <- 12 * 27
# conf.lims <- c(-1,1)*qnorm((1 + alpha)/2)/sqrt(max_lag)

# # Let's plot our values with confidence
# train %>%
#   filter(lead == 1, fcst_cell == 259627) %>%
#   tidy_acf("value", lags = 0:max_lag) %>%
#   ggplot(aes(lag, acf)) +
#   geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
#   geom_hline(yintercept = conf.lims[2], size = 1, color = palette_light()[[5]]) +
#   geom_hline(yintercept = conf.lims[1], size = 1, color = palette_light()[[5]]) +
#   geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
#   annotate("text", label = "10 Year Mark", x = 130, y = 0.8,
#            color = palette_light()[[2]], size = 6, hjust = 0) +
#   annotate("text", label = "Confidence", x = 210, y = 0.18,
#            color = palette_light()[[5]], size = 4, hjust = 0) +
#   theme_tq() +
#   labs(title = "ACF: CFSv2 Bias, lead=1, fcst_cell=259627")

# train %>%
#   filter(lead == 1, fcst_cell == 259627) %>%
#   tidy_acf("value", lags = 40:60) %>%
#   ggplot(aes(lag, acf)) +
#   #geom_vline(xintercept = 120, size = 3, color = palette_light()[[2]]) +
#   geom_segment(aes(xend = lag, yend = 0), color = palette_light()[[1]]) +
#   geom_point(color = palette_light()[[1]], size = 2) +
#   geom_label(aes(label = acf %>% round(2)), vjust = -1,
#              color = palette_light()[[1]]) +
#   #annotate("text", label = "10 Year Mark", x = 121, y = 0.8,
#   #color = palette_light()[[2]], size = 5, hjust = 0) +
#   theme_tq() +
#   labs(title = "ACF: CFSv2 Bias, lead=1, fcst_cell=259627",
#        subtitle = "Zoomed in on Lags 40 to 60")

# # Pull Optimal Lag
# optimal_lag_setting <- data %>%
#   filter(lead == 1, fcst_cell == 259627) %>%
#   tidy_acf("value", lags = 50:150) %>%
#   filter(acf == max(acf)) %>%
#   pull(lag)
# optimal_lag_setting # 72

####################################################
# LSTM Plan
#
# Tensor Format:
#   -Predictors (X) must be 3D array with dimensions; [samples, timesteps, features]
#    First dimension is length of values, second is number of lags,
#    and third is number of predictors (3, multivariate).
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
# Let's create a training plan.
# We'll make the lead time and forecast cell features and train on all data.
# We'll train on 4 leads and 16 cells at a time, running through the time-series 
# from start to finish for all 64 lead/cell combos, then select a new mini-batch.
# For each batch, we run a slice of 32 timesteps, backprop, keep the states,
# then run the next 32 timesteps starting from those end states.

############################################################################
# PREPROCESSING
# LSTM performs better if we center and scale our data.
# We normalize from 0 to 1.

max.train <- max(train$value)
min.train <- min(train$value)
train.scaled <- train %>%
  mutate(value = (value - min.train)/(max.train - min.train))
max.test <- max(test$value)
min.test <- min(test$value)
test.scaled <- test %>%
  mutate(value = (value - min.test)/(max.test - min.test))

# This is necessary for the agent-based minibatching
unique.cells <- unique(train$fcst_cell)

# We're going to subset all of the data into train.matrix. This will make
# Minibatching MUCH faster!
train.matrix <- array(list(), dim=c(9, 51))

# We'll toss the date row--the data's already ordered!
for (i in c(1:9)) {
  for (j in c(1:length(unique.cells))) {
    train.matrix[[i, j]] <- as.matrix(train.scaled %>%
      dplyr::filter(lead == i, fcst_cell == unique.cells[j]))[,-2]
    class(train.matrix[[i,j]]) <- "numeric"
  }
}

# Same for test data...
test.matrix <- array(list(), dim=c(9, 51))

# We'll toss the date row--the data's already ordered!
for (i in c(1:9)) {
  for (j in c(1:length(unique.cells))) {
    test.matrix[[i, j]] <- as.matrix(test.scaled %>%
      dplyr::filter(lead == i, fcst_cell == unique.cells[j]))[,-2]
    class(test.matrix[[i,j]]) <- "numeric"
  }
}

# A generator is a special type of function that you call repeatedly to
# obtain a sequence of values. This generator function taakes the current
# array of data and yields batches of data from the recent past,
# along with a target value in the future. We'll generate samples on the
# fly using the original data.
# Arguments:
#   data: original array of normalized floating-point data
#   lookback: how many timesteps back the input data should go
#   delay: how many timesteps in the future the target should be
#   min.index and max.index: indices in data array that deliminit which
#     timesteps to draw from. Useful for keeping segment for val and test
#   shuffle: whether to shuffle samples or draw in chronological order
#   batch.size: number of samples per batch
#   step: period in timesteps at which you sample data
generator <- function(data, lookback, delay, min.index, max.index,
                      shuffle = FALSE, batch.size = 1, step = 1,
                      cell.sample = 16, lead.sample = 4) {
  if (is.null(max.index)) {
    max.index <- nrow(data[[1]]) - delay - 1
  }
  i <- min.index + lookback
  
  function() {
    # samples and targets for binding
    all.samples <- array(0,dim=c(cell.sample * lead.sample,
                                 lookback,
                                 3))
    all.targets <- array(0,dim=c(cell.sample * lead.sample,
                                 batch.size))
    
    # Agent-based random sampling
    lead.sample <- sample(c(1:9), size = lead.sample)
    cell.sample <- sample(c(1:51), size = cell.sample)
    
    count <- 1
    for (LS in lead.sample) {
      for (CS in cell.sample) {

        # grab relevant lead&cell combo
        temp <- train.matrix[[LS,CS]]
        
        if(shuffle) {
          rows <- sample(c((min.index + lookback):max.index), size = batch.size)
        } else {
          if (i + batch.size >= max.index)
            i <<- min.index + lookback
          rows <- c(i:min(i + batch.size-1, max.index))
          i <<- i + length(rows)
        }
        
        samples <- array(0, dim = c(lookback, 3)) #dim(temp)[[-2]]
        targets <- array(0, dim = c(length(rows)))

          
        indices <- seq(rows[[1]] - lookback, rows[[1]]-1,
                       length.out = dim(samples)[[1]])
        
        samples <- temp[indices,]
        targets <- temp[rows[[1]] + delay, 1]
        
        
        #print(LS * (CS-1) + CS)
        all.samples[count,,] <- samples
        all.targets[count,] <- targets
        count <- count + 1
      }
    }
    list(all.samples, all.targets)
  }
}

lookback <- 32 # Observations will go back 32 months
steps <- 1 # Observations will be sampled at 1 data point per month
delay <- 1 # Targets will be 1 months ahead
batch.size <- 1 # cell.sample and lead.sample are are our batching methods...
cell.sample <- 16 # We select 16 cells at a time for this experiment
lead.sample <- 4 # Select 4 lead times

# Let's use the abstract generator to instantiate three generators
#   Train: first 240 timesteps
#   Test: Next 60 timesteps
#   Validator: Remaining timesteps


train.gen = generator(
  train.matrix,
  lookback = lookback,
  delay = delay,
  min.index = 1,
  max.index = 240,
  shuffle = TRUE,
  step = step, 
  batch.size = batch.size
)

val.gen = generator(
  train.matrix,
  lookback = lookback,
  delay = delay,
  min.index = 241,
  max.index = 300,
  step = step,
  batch.size = batch.size
)

test.gen = generator(
  test.matrix,
  lookback = lookback,
  delay = delay,
  min.index = 1,
  max.index = 150,
  step = step,
  batch.size = batch.size
)

# How many steps to draw from val.gen in order to see the entire validation set
val.steps <- floor((300 - 241 - lookback) / batch.size)

# How many steps to draw from test.gen in order to see the entire test set
test.steps <- floor((150 - 1 - lookback) / batch.size)



####################################################
# Before jumping in, let's try a common-sense approach.
# To confirm that our machine learning method is effective, we'll
# compare it against a "lookahead 1 year" method.

evaluate.naive.method <- function() {
  batch.maes <- c()
  for (step in 1:val.steps) {
    c(samples, targets) %<-% val.gen()
    preds <- samples[,dim(samples)[[2]],1]
    mae <- mean(abs(preds - targets))
    batch.maes <- c(batch.maes, mae)
  }
  print(mean(batch.maes))
}

evaluate.naive.method()




################################################################################
# Let's try a GRU model!

model <- keras_model_sequential() %>% 
  layer_gru(units = 32, input_shape = list(NULL, 3)) %>% 
  layer_dense(units = 1)

model %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

model %>% fit (
  train.gen,
  verbose=2,
  steps_per_epoch = 241 - lookback,
  epochs = 20,
  validation_data = val.gen,
  validation_steps = val.steps
)

# Let's save the weights!
model %>% save_model_tf("model")
model %>% save_model_weights_tf("checkpoints/cp.ckpt")



################################################################################
# Add a dropout

model2 <- keras_model_sequential() %>% 
  layer_gru(units = 32, dropout = 0.2, recurrent_dropout = 0.2,
            input_shape = list(NULL, 3)) %>% 
  layer_dense(units = 1)

model2 %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

model2 %>% fit(
  train.gen,
  verbose=2,
  steps_per_epoch = 241 - lookback,
  epochs = 40,
  validation_data = val.gen,
  validation_steps = val.steps
)

# Let's save the weights!
model %>% save_model_tf("model2")
model %>% save_model_weights_tf("checkpoints/cp2.ckpt")



################################################################################
# Next, let's try stacking layers

model3 <- keras_model_sequential() %>% 
  layer_gru(units = 32, 
            dropout = 0.1, 
            recurrent_dropout = 0.5,
            return_sequences = TRUE,
            input_shape = list(NULL, 3)) %>% 
  layer_gru(units = 64, activation = "relu",
            dropout = 0.1,
            recurrent_dropout = 0.5) %>% 
  layer_dense(units = 1)

model3 %>% compile(
  optimizer = optimizer_rmsprop(),
  loss = "mae"
)

model3 %>% fit(
  train.gen,
  verbose=2,
  steps_per_epoch = 241 - lookback,
  epochs = 40,
  validation_data = val.gen,
  validation_steps = val.steps
)

# Let's save the weights!
model2 %>% save_model_tf("model3")
model2 %>% save_model_weights_tf("checkpoints/cp3.ckpt")

# Get a brief summary of the 3 architectures...
summary(model)
summary(model2)
summary(model3)

# and EVALUATE!!

model %>% evaluate(
    test.gen,
    verbose=0
)
model2 %>% evaluate(
    test.gen,
    verbose=0
)
model3 %>% evaluate(
    test.gen,
    verbose=0
)


