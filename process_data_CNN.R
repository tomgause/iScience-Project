##process_data_CNN
# Acadia Hegedus
# 5/4/22

#Load in libraries
library(dplyr)
library(ggplot2)
library(tensorflow)
library(keras)

#set working dir
setwd("C:/Users/tgause/iScience_Project")

#Define constants
img_rows <- 10
img_cols <- 10
nfeatures <- 5 #predictor variables: elevation, fcst_tmp_k, lead, target_month,
#forecast_target

#Data preparation

#Read in data set
#train.data <- readRDS("./data/train_subset_Vermont_CNN_2022-04-25_21-00-04.RDS")
train.data <- readRDS("./data/train_subset_Vermont_2022-05-04_16-49-06.RDS")
train.data1 <- train.data

x.train <- train.data1 %>%
  dplyr::select(forecast_target, x, y, target_month, lead, fcst_tmp_k, 
                elevation)%>%
  group_by(x,y,lead,forecast_target, target_month,elevation)%>%
  summarize(mean_fcst = mean(fcst_tmp_k)) #take mean fcsts!

y.train <- train.data1%>%
  dplyr::select(forecast_target, x, y, target_month, lead, 
                elevation, bias.t)%>%
  group_by(x,y,lead,forecast_target, target_month,elevation)%>%
  summarize(mean_bias = mean(bias.t)) #take mean biases!

#unlisting does not work on datetime objects
#i.e. forecast_target, when unlisted later on, gives random integer values
#to fix this, let's change forecast_target to just be the year
#we don't lose information by doing this -- target_month has the month!

x.train$forecast_target <- as.numeric(substring(x.train$forecast_target,1,4))
y.train$forecast_target <- as.numeric(substring(y.train$forecast_target,1,4))

#graph data
x.train%>%
  filter(forecast_target == "1982")%>%
  ggplot(mapping = aes(x = x, y = y), color = bias.t)+
  geom_point()

#find dimensions for array
nimages <- x.train%>% #number of images
  dplyr::select(forecast_target, lead)%>%
  unique()%>%
  nrow()

###PRACTICE ARRAY CODE

#creating sample.x.train

##make array of 1 image x 10 x 10 x 1 feature
simple.data <- x.train%>%
  filter(forecast_target == 1983)%>%
  filter(target_month == 1)%>%
  filter(lead == 3)%>%
  dplyr::select(mean_fcst,x,y)%>%
  arrange(x,-y)%>% #sort to make lats and longs line up with array indices
  dplyr::select(-x,-y)#remove x,y as they will be encoded in array index

simple.array <- array(unlist(simple.data), 
                      dim = c(1, img_rows, img_cols, 1), 
                      dimnames = list(image_number = "img_number", 
                                      y  = 1:10, #img_rows
                                      x  = 1:10, #img_cols
                                      features = "mean_fcst"))

#sanity check
sanity <- x.train%>%
  filter(forecast_target == 1983)%>%
  filter(target_month == 1)%>%
  filter(lead == 3)%>%
  filter(y == 42.75)

#above code works!

##make array of 1 image x 10 x 10 x 5 features
#get mean_fcst for all images
simple.data2 <- x.train%>%
  filter(lead == 3)%>%
  filter(forecast_target == 1983)%>%
  filter(target_month == 1)%>%
  arrange(x,-y)%>%
  dplyr::select(-x,-y)

simple.array2 <- array(unlist(simple.data2), 
                       dim = c(1, img_rows, img_cols, 5), 
                       dimnames = list(image_number = "img_number", 
                                       y  = 1:10, #img_rows
                                       x  = 1:10, #img_cols
                                       features = colnames(simple.data2)))

#sanity check
sanity2 <- x.train%>%
  filter(forecast_target == 1983)%>%
  filter(lead == 3)%>%
  filter(target_month == 1)%>%
  filter(y == 42.75)

#the above code seems to work!

## make array of 3 images x 10 img_rows x 10 img_cols x 5 features
simple.data3 <- x.train%>%
  filter(lead %in% c(1,2,3))%>%
  filter(forecast_target == 1983)%>%
  filter(target_month == 1)%>%
  arrange(x,-y,lead) %>%
  select(-x, -y)

simple.array3 <- array(unlist(simple.data3), 
                       dim = c(3, img_rows, img_cols, 5), 
                       dimnames = list(image_number = 1:3, 
                                       y  = 1:img_rows, #img_rows
                                       x  = 1:img_cols, #img_cols
                                       features = colnames(array.data)))

#sanity check
sanity3 <- x.train%>%
  filter(forecast_target == 1983)%>%
  filter(lead %in% c(1,2,3))%>%
  filter(target_month == 1)%>%
  arrange(forecast_target,target_month,lead,x,-y)

saveRDS(simple.array3,"sample.x.train.RDS")
#the above code works!

#creating sample.y.train
simple.data4 <- y.train%>%
  filter(lead %in% c(1,2,3))%>%
  filter(forecast_target == 1983)%>%
  filter(target_month == 1)%>%
  arrange(x,-y,lead) %>%
  ungroup()%>%
  select(-x, -y)%>%
  select(mean_bias)

simple.array4 <- array(unlist(simple.data4), 
                       dim = c(3, img_rows, img_cols, 1), 
                       dimnames = list(image_number = 1:3, 
                                       y  = 1:img_rows, #img_rows
                                       x  = 1:img_cols, #img_cols
                                       features = "mean_bias"))

#sanity check
sanity4 <- y.train%>%
  filter(forecast_target == 1983)%>%
  filter(lead %in% c(1,2,3))%>%
  filter(target_month == 1)%>%
  arrange(forecast_target,target_month,lead,x,-y)

saveRDS(simple.array4, "sample.y.train.RDS")

###############################################################################

####FULL ARRAY CODE
## x_train: make array of 2979 images x 10 img_rows x 10 img_cols x 5 features
x_train_data <- x.train%>%
  arrange(x,-y,forecast_target,target_month,lead)%>%
  select(-x, -y)%>%
  scale() #scale to have mean = 0, sd = 1

x_train_array <- array(unlist(x_train_data), 
                       dim = c(nimages, img_rows, img_cols, 5), 
                       dimnames = list(image_number = 1:nimages, 
                                       y  = 1:img_rows, #img_rows
                                       x  = 1:img_cols, #img_cols
                                       features = colnames(x_train_data)))

# y_train: make array of 2979 images x 10 img_rows x 10 img_cols x 1 feature (mean_bias)
y_train_data <- y.train%>%
  arrange(x,-y,forecast_target,target_month,lead)%>%
  ungroup()%>%
  select(mean_bias)%>%
  scale()

y_train_array <- array(unlist(y_train_data), 
                       dim = c(nimages, img_rows, img_cols, 1), 
                       dimnames = list(image_number = 1:nimages, 
                                       y  = 1:img_rows, #img_rows
                                       x  = 1:img_cols, #img_cols
                                       feature = "mean_bias"))

#sanity checks
final_sanity <- y.train%>%
  filter(forecast_target == 1984)%>%
  filter(lead == 4)%>%
  filter(target_month == 8)%>%
  arrange(forecast_target,target_month,lead,x,-y)

# Redefine  dimension of train/test inputs
x_train_array <- array_reshape(x_train_array, c(nrow(x_train_array), img_rows, img_cols, 5))
y_train_array <- array_reshape(y_train_array, c(nrow(y_train_array), img_rows, img_cols, 1))

cat('x_train_shape:', dim(x_train_array), '\n')
cat('y_train_shape:', dim(y_train_array), '\n')
cat(nrow(x_train_array), 'train samples\n')
#cat(nrow(x_test_array), 'test samples\n')

####################################################################################

#All of the above code works, and gets us the bias for each image, averaged across all 24 forecasts
#Next, let's recreate the above code except instead of having the average bias, 
#let's keep forecast_timestamp as a variable so we can have actual fcsts and actual bias

explore.x.train <- train.data1 %>%
  filter(x == -73)%>%
  filter(y == 45)%>%
  filter(lead == 1)%>%
  filter(target_month == 3)%>%
  filter(forecast_target == "1987-03-01")

#forecast_timestamp_rm does not seem to be unique among forecasts... hm
#we need the original forecast_timestamps to make this happen.

#####Create x_test_array and y_test_array


