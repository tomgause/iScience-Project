## process_data_CNN
# Acadia Hegedus
# 5/10/22

#Load in libraries
library(dplyr)
library(ggplot2)
library(tensorflow)
library(keras)

#set working directory
setwd("C:/Users/tgause/iScience_Project")

#Define constants
img_rows <- 10
img_cols <- 10
nfeatures <- 5 #predictor variables: elevation, fcst_tmp_k, lead, target_month,
#forecast_target

#Read in data sets
train.data <- readRDS("./data/train_subset_Vermont_CNN_2022-05-04_19-42-38.RDS")
test.data <- readRDS("./data/test_subset_CNN2022-05-10_15-08-36.RDS")
train.data1 <- train.data
test.data1 <- test.data


#####Create x_train_array and y_train_array

x.train <- train.data1 %>%
  dplyr::select(forecast_target, x, y, target_month, lead, fcst_tmp_k, 
                elevation, forecast_timestamp)

y.train <- train.data1%>%
  dplyr::select(forecast_target, x, y, target_month, lead, 
                elevation, forecast_timestamp, bias.t)

#unlisting does not work on datetime objects
#i.e. forecast_target, when unlisted later on, gives random integer values
#to fix this, let's change forecast_target to just be the year
#we don't lose information by doing this -- target_month has the month!

x.train$forecast_target <- as.numeric(substring(x.train$forecast_target,1,4))
y.train$forecast_target <- as.numeric(substring(y.train$forecast_target,1,4))

#find dimensions for array
n_train_images <- x.train%>% #number of train images
  dplyr::select(forecast_target, lead,forecast_timestamp)%>%
  unique()%>%
  nrow()

## x_train: make array of 69454 images x 10 img_rows x 10 img_cols x 5 features
x_train_data <- x.train%>%
  arrange(x,-y,forecast_target,target_month,lead,forecast_timestamp)%>%
  select(-x, -y,-forecast_timestamp)%>%
  scale() #scale to have mean = 0, sd = 1

x_train_array <- array(unlist(x_train_data), 
                       dim = c(n_train_images, img_rows, img_cols, 5), 
                       dimnames = list(image_number = 1:n_train_images, 
                                       y  = 1:img_rows, #img_rows
                                       x  = 1:img_cols, #img_cols
                                       features = colnames(x_train_data)))

## y_train: make array of 69454 images x 10 img_rows x 10 img_cols x 1 feature (bias.t)
y_train_data <- y.train%>%
  arrange(x,-y,forecast_target,target_month,lead,forecast_timestamp)%>%
  ungroup()%>%
  select(bias.t)

y_train_array <- array(unlist(y_train_data), 
                       dim = c(n_train_images, img_rows, img_cols, 1), 
                       dimnames = list(image_number = 1:n_train_images, 
                                       y  = 1:img_rows, #img_rows
                                       x  = 1:img_cols, #img_cols
                                       feature = "bias.t"))

#sanity checks
final_sanity <- y.train%>%
  filter(forecast_target == 1982)%>%
  filter(lead == 1)%>%
  filter(target_month == 2)%>%
  filter(forecast_timestamp == "1982010100")%>%
  arrange(x,-y,forecast_timestamp)

# Redefine  dimension of train/test inputs
x_train_array <- array_reshape(x_train_array, c(nrow(x_train_array), img_rows, img_cols, 5))
y_train_array <- array_reshape(y_train_array, c(nrow(y_train_array), img_rows, img_cols, 1))

cat('x_train_shape:', dim(x_train_array), '\n')
cat('y_train_shape:', dim(y_train_array), '\n')


#####Create x_test_array and y_test_array

x.test <- test.data1 %>%
  dplyr::select(forecast_target, x, y, target_month, lead, fcst_tmp_k, 
                elevation, forecast_timestamp)

y.test <- test.data1%>%
  dplyr::select(forecast_target, x, y, target_month, lead, 
                elevation, forecast_timestamp, bias.t)

#unlisting does not work on datetime objects
#i.e. forecast_target, when unlisted later on, gives random integer values
#to fix this, let's change forecast_target to just be the year
#we don't lose information by doing this -- target_month has the month!

x.test$forecast_target <- as.numeric(substring(x.test$forecast_target,1,4))
y.test$forecast_target <- as.numeric(substring(y.test$forecast_target,1,4))

#find dimensions for array
n_test_images <- x.test%>% #number of test images
  dplyr::select(forecast_target, lead,forecast_timestamp)%>%
  unique()%>%
  nrow()

## x_test: make array of 27216 images x 10 img_rows x 10 img_cols x 5 features
x_test_data <- x.test%>%
  arrange(x,-y,forecast_target,target_month,lead,forecast_timestamp)%>%
  select(-x, -y,-forecast_timestamp)%>%
  scale() #scale to have mean = 0, sd = 1

x_test_array <- array(unlist(x_test_data), 
                      dim = c(n_test_images, img_rows, img_cols, 5), 
                      dimnames = list(image_number = 1:n_test_images, 
                                      y  = 1:img_rows, #img_rows
                                      x  = 1:img_cols, #img_cols
                                      features = colnames(x_test_data)))

# y_test: make array of 27216 images x 10 img_rows x 10 img_cols x 1 feature (bias.t)
y_test_data <- y.test%>%
  arrange(x,-y,forecast_target,target_month,lead,forecast_timestamp)%>%
  ungroup()%>%
  select(bias.t)

y_test_array <- array(unlist(y_test_data), 
                      dim = c(n_test_images, img_rows, img_cols, 1), 
                      dimnames = list(image_number = 1:n_test_images, 
                                      y  = 1:img_rows, #img_rows
                                      x  = 1:img_cols, #img_cols
                                      feature = "bias.t"))

#sanity checks
final_sanity <- y.test%>%
  arrange(x,-y,forecast_target,target_month,lead,forecast_timestamp)%>%
  filter(forecast_target == 2012)%>%
  filter(lead == 1)%>%
  filter(target_month == 2)%>%
  filter(forecast_timestamp == "2012012500")

# Redefine  dimension of train/test inputs
x_test_array <- array_reshape(x_test_array, c(nrow(x_test_array), img_rows, img_cols, 5))
y_test_array <- array_reshape(y_test_array, c(nrow(y_test_array), img_rows, img_cols, 1))

cat('x_test_shape:', dim(x_test_array), '\n')
cat('y_test_shape:', dim(y_test_array), '\n')

#and save!
saveRDS(x_train_array,file = "./data/x_train.RDS")
saveRDS(y_train_array,file = "./data/y_train.RDS")
saveRDS(x_test_array,file = "./data/x_test.RDS")
saveRDS(y_test_array,file = "./data/y_test.RDS")