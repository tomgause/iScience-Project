#Practice with CNN using TensorFlow
#Code taken from https://keras.rstudio.com/articles/examples/mnist_cnn.html 

#3/15/22

library(tensorflow)
library(keras)
library(tidyverse)

#Load Train Data
new_x_train <- readRDS("x_train.RDS")
new_y_train <- readRDS("y_train.RDS")

#Load Test Data
new_x_test <- readRDS("x_test.RDS")
new_y_test <- readRDS("y_test.RDS")

#Set Parameters 
new_img_rows <- 10
new_img_cols <- 10
new_input_shape <- c(10, 10, 5)
batch_size <- 200
epochs <-7
# Define model
build_model <- function(input_shape,batch_size,epochs){
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh',
                  input_shape = new_input_shape, padding = "same") %>%
    layer_max_pooling_2d(pool_size = c(2,2))%>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh') %>%
    layer_upsampling_2d(size = c(4,4))%>%
    layer_dropout(rate = 0.5) %>% 
    layer_conv_2d(filters = 1, kernel_size = c(3,3), activation = 'linear')
  

  # Compile model
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adam(),
    metrics = c('mse')
  )
  
  # Train model
  model %>% fit(
    new_x_train, new_y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 0.3 #fraction of training data to be used for validation
  )
}

##predict on test data  
model.prediction <- model %>% predict(new_x_test)


##Turn the array back to a data frame
CNN.prediction.dataframe <- as.data.frame.table(model.prediction)
y_test_dataframe <- as.data.frame.table(new_y_test)


#build_model(input_shape = new_input_shape,batch_size = 64,epochs = 12) #min ~0.36?
#build_model(input_shape = new_input_shape,batch_size = 200,epochs = 12)# pretty good 
#build_model(input_shape = new_input_shape,batch_size = 200,epochs = 20)#CNN-3 - mse min
#build_model(input_shape = new_input_shape,batch_size = 100,epochs = 12)# this batch size is not very good
#build_model(input_shape = new_input_shape,batch_size = 150,epochs = 12)# this batch size not as good as 200

build_model(input_shape = new_input_shape,batch_size = 200,epochs = 7)
##It seems like epoch 9 is pretty optimal) 




model %>% summary()
##looks like epoch = 8 is optimal

test.scores <- model %>% evaluate(
  new_x_test, new_y_test, verbose = 0
)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test mse:', scores[[2]], '\n')

########################################Trying 2nd architecture###########################
####reduce filter size and added another conv_2d layer after the first one

build_model2 <- function(input_shape,batch_size,epochs){
  model2 <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'tanh',
                  input_shape = new_input_shape) %>%
    layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'tanh') %>%
    layer_max_pooling_2d(pool_size = c(2,2))%>%
    #layer_dropout(rate = 0.5) %>% 
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh') %>%
    layer_upsampling_2d(size = c(6,6))%>%
    #layer_dropout(rate = 0.5) %>% 
    layer_conv_2d(filters = 1, kernel_size = c(3,3), activation = 'linear')
  
  #Layer Notes
  #filter = number of filters used -> number of resultant feature maps
  #kernel_size = how large each filter is
  # downsampling = max pooling = decreases dimensions
  # upsampling = increases dimensions
  # pool size: factor to which downsize input data
  # prevent overfitting
  #layer_dropout #prevent overfitting
  #layer_flatten #turns cube into list
  
  # Compile model
  model %>% compile(
    loss = "mse",
    optimizer = optimizer_adadelta(), #how to choose?
    metrics = c('mse')
  )
  
  # Train model
  model %>% fit(
    new_x_train, new_y_train,
    batch_size = batch_size,
    epochs = epochs,
    validation_split = 0.3 #fraction of training data to be used for validation
  )
}

build_model2(input_shape = new_input_shape,batch_size = 200,epochs = 12) #pretty good learning curve

build_model2(input_shape = new_input_shape,batch_size = 64,epochs = 12) # not so good 
build_model2(input_shape = new_input_shape,batch_size = 250,epochs = 12) #okay learning curve
build_model2(input_shape = new_input_shape,batch_size = 300,epochs = 12)# CNN5 
