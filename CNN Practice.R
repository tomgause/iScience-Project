#Practice with CNN using TensorFlow
#Code taken from https://keras.rstudio.com/articles/examples/mnist_cnn.html 

#3/15/22

library(tensorflow)
library(keras)

# #Define constants
# batch_size <- 10 #number of samples to compute gradient with, and take average
# #makes back propagation more efficient and accurate
# num_classes <- 10 #number of target categories
# epochs <- 12 #number of training iterations
# img_rows <- 28
# img_cols <- 28

#Data preparation

#Read in data set
# letter.data <- dataset_mnist()
# letter.data2 <- letter.data
# #data set is a list of length 2; with train in index 1 and test in index 2
# #already split into training and testing
# x_train <- letter.data2$train$x
# y_train <- letter.data2$train$y
# x_test <- letter.data2$test$x
# y_test <- letter.data2$test$y

# Redefine  dimension of train/test inputs
# x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
# x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
# input_shape <- c(img_rows, img_cols, 1)
# 
# # Transform RGB values into [0,1] range
# x_train <- x_train / 255
# x_test <- x_test / 255
# 
# cat('x_train_shape:', dim(x_train), '\n')
# cat(nrow(x_train), 'train samples\n')
# cat(nrow(x_test), 'test samples\n')

# Convert class vectors to binary class matrices
# convert outputs to binary (10 rows)
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)

#replace mnist data with our data\
# x_train <- readRDS("C:/Users/bestbuy/AppData/Local/Packages/microsoft.windowscommunicationsapps_8wekyb3d8bbwe/LocalState/Files/S0/120/Attachments/sample.x.train[7437].RDS")
# y_train <- readRDS("C:/Users/bestbuy/AppData/Local/Packages/microsoft.windowscommunicationsapps_8wekyb3d8bbwe/LocalState/Files/S0/120/Attachments/sample.y.train[7438].RDS")
#
# y_train <- y_train[,,,5]
#
# input_shape <- c(10, 10, 5)

#replace x_train with our data
new_x_train <- readRDS(file.choose())
new_y_train <- readRDS(file.choose())
new_img_rows <- 10
new_img_cols <- 10
new_input_shape <- c(10, 10, 5)
batch_size <-10
epochs <-12
# Define model

build_model <- function(input_shape,batch_size,epochs){
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh',
                  input_shape = new_input_shape) %>%
    layer_max_pooling_2d(pool_size = c(2,2))%>%
    #layer_dropout(rate = 0.5) %>% 
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh') %>%
    layer_upsampling_2d(size = c(6,6))%>%
    layer_dropout(rate = 0.5) %>% 
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
    optimizer = optimizer_adam(), #how to choose?
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


build_model(input_shape = new_input_shape,batch_size = 64,epochs = 12) #min ~0.36?
build_model(input_shape = new_input_shape,batch_size = 200,epochs = 12)# pretty good 
build_model(input_shape = new_input_shape,batch_size = 200,epochs = 20)#CNN-3 - mse min
build_model(input_shape = new_input_shape,batch_size = 100,epochs = 12)# this batch size is not very good
build_model(input_shape = new_input_shape,batch_size = 150,epochs = 12)# this batch size not as good as 200

build_model(input_shape = new_input_shape,batch_size = 200,epochs = 12)#added new dropout layer: not very good




model %>% summary()
##looks like epoch = 8 is optimal

scores <- model %>% evaluate(
  x_test, y_test, verbose = 0
)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')

########################################Trying 2nd architecture###########################
####reduce filter size and added another conv_2d layer after the first one

build_model2 <- function(input_shape,batch_size,epochs){
  model <- keras_model_sequential() %>%
    layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'tanh',
                  input_shape = new_input_shape) %>%
    layer_conv_2d(filters = 32, kernel_size = c(2,2), activation = 'tanh') %>%
    layer_max_pooling_2d(pool_size = c(2,2))%>%
    #layer_dropout(rate = 0.5) %>% 
    layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh') %>%
    layer_upsampling_2d(size = c(6,6))%>%
    layer_dropout(rate = 0.5) %>% 
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
    optimizer = optimizer_adam(), #how to choose?
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
