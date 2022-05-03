#Practice with CNN using TensorFlow
#Code taken from https://keras.rstudio.com/articles/examples/mnist_cnn.html 

#3/15/22

library(tensorflow)
library(keras)

#Define constants
batch_size <- 2 #number of samples to compute gradient with, and take average
#makes back propagation more efficient and accurate
num_classes <- 10 #number of target categories
epochs <- 12 #number of training iterations
img_rows <- 28
img_cols <- 28

#Data preparation

#Read in data set
letter.data <- dataset_mnist()
letter.data2 <- letter.data
#data set is a list of length 2; with train in index 1 and test in index 2
#already split into training and testing
x_train <- letter.data2$train$x
y_train <- letter.data2$train$y
x_test <- letter.data2$test$x
y_test <- letter.data2$test$y

# Redefine  dimension of train/test inputs
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
x_test <- array_reshape(x_test, c(nrow(x_test), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, 1)

# Transform RGB values into [0,1] range
x_train <- x_train / 255
x_test <- x_test / 255

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

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
new_x_train <- readRDS("C:/Users/bestbuy/AppData/Local/Packages/microsoft.windowscommunicationsapps_8wekyb3d8bbwe/LocalState/Files/S0/120/Attachments/sample.x.train[7437].RDS")
new_y_train <- readRDS("C:/Users/bestbuy/AppData/Local/Packages/microsoft.windowscommunicationsapps_8wekyb3d8bbwe/LocalState/Files/S0/120/Attachments/sample.y.train[7438].RDS")
new_img_rows <- 10
new_img_cols <- 10
new_input_shape <- c(10, 10, 1)

new_x_train <- new_x_train[,,,5]
new_x_train <- array_reshape(new_x_train, c(nrow(new_x_train), new_img_rows, new_img_cols, 1))
new_y_train <- new_y_train[,,,5]
new_y_train <- array_reshape(new_y_train, c(nrow(new_y_train), new_img_rows, new_img_cols, 1))

# Define model
model <- keras_model_sequential() %>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh',
                input_shape = new_input_shape) %>%
  layer_max_pooling_2d(pool_size = c(2,2))%>%
  layer_conv_2d(filters = 32, kernel_size = c(3,3), activation = 'tanh') %>%
  layer_upsampling_2d(size = c(6,6))%>%
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
  validation_split = 0.2 #fraction of training data to be used for validation
)

##looks like epoch = 8 is optimal

scores <- model %>% evaluate(
  x_test, y_test, verbose = 0
)

# Output metrics
cat('Test loss:', scores[[1]], '\n')
cat('Test accuracy:', scores[[2]], '\n')
