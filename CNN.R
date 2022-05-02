install.packages("keras")
install.packages("tfruns")

library(keras)
library(tfruns)
library(tensorflow)

####insert training /test data 
train_data <- sample.x.train
train_targets <- sample.y.train[,,,5]

####insert validation data 
val_data <- NULL
val_targets <- NULL

##Standardize/normalize the data 
# mean <- apply(train_data, 2, mean)
# std <- apply(train_data, 2, sd)
# train_data <- scale(train_data, center = mean, scale = std)
# test_data <- scale(test_data, center = mean, scale = std)


##Build the model 
# we need to write a function because we need to run the model multiple times 
build_model <- function() {
   model <- keras_model_sequential() %>%
     layer_conv_2d(filters = 32,
                   kernel_size = c(3,3),
                   dilation_rate = 2,
                   activation = "relu") %>%
     layer_max_pooling_2d(pool_size = c(2,2)) %>%
     layer_conv_2d(filters = 64,
                   kernel_size = c(3,3),
                   dilation_rate = 1) %>%
     layer_max_pooling_2d(pool_size = c(2,2)) %>%
     layer_conv_2d(filters = 128,
                   kernel_size = c(3,3),
                   dilation_rate = 1) %>%
     layer_upsampling_2d(size = c(2,2)) %>%
     layer_conv_2d(filters = 64,
                  kernel_size = c(3,3),
                  dilation_rate = 1) %>%
     layer_upsampling_2d(size = c(2,2)) %>%
     layer_conv_2d(filters = 32,
                  kernel_size = c(3,3),
                  dilation_rate = 2) %>%
     layer_dense(units = 64, activation = "relu") %>%
     layer_dense(units = 1) #a single unit and no activation (it will be a linear layer) is
                            #a typical setup for scalar regression (a regression where you're
                            #trying to predict a single continuous value).

  
#select optimization,loss functions 
   model %>% compile(
     optimizer = "adam",
     loss = "mse", # "mse" a widely used loss function for regression problems.
     metrics = c("mae") #mean absolute error: absolute value of the difference between the predictions and the targets
   )
 }

###Train the model 

num_epochs <- 10
all_mae_histories <-NULL

build_model()

history <- model %>% fit(
    train_data, 
    train_targets,
    validation_data = list(train_data, train_targets),
    epochs = num_epochs, 
    batch_size = 1, 
    verbose = 0
  )
  mae_history <- history$metrics$val_mean_absolute_error
  all_mae_histories <- rbind(all_mae_histories, mae_history)
  
#plot mae history 
library(ggplot2)
ggplot(average_mae_history, aes(x = epoch, y = validation_mae)) + geom_line()
  

#***********************Tuning*************************************
###consider dropout 
#layer_output <- layer_output * sample(0:1, length(layer_output),
                                   # replace = TRUE)
# model <- keras_model_sequential() %>%
#   layer_dense(units = 16, activation = "relu", input_shape = c(10000)) %>%
#   layer_dropout(rate = 0.5) %>%
#   layer_dense(units = 16, activation = "relu") %>%
#   layer_dropout(rate = 0.5) %>%
#   layer_dense(units = 1, activation = "sigmoid")


