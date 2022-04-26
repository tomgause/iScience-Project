##process_data_CNN
# Acadia Hegedus
# 4/26/22

#Load in libraries
library(dplyr)
library(ggplot2)

#set working dir
setwd("C:/Users/tgause/iScience_Project")

#Define constants
img_rows <- 10
img_cols <- 10
nfeatures <- 5 #predictor variables: elevation, fcst_tmp_k, lead, target_month,
#forecast_target

#Data preparation

#Read in data set
train.data <- readRDS("./data/train_subset_Vermont_CNN_2022-04-25_21-00-04.RDS")
train.data1 <- train.data

x.train <- train.data1 %>%
  dplyr::select(forecast_target, x, y, target_month, lead, fcst_tmp_k, 
                elevation)
y.train <- train.data1%>%
  dplyr::select(bias.t)

#graph data
x.train%>%
  filter(forecast_target == "1982-02-01")%>%
  ggplot(mapping = aes(x = x, y = y), color = bias.t)+
  geom_point()

#find dimensions for array
nimages <- x.train%>% #number of images
  dplyr::select(forecast_target, lead)%>%
  unique()%>%
  nrow()

#convert dataframe to array of nimages x 10 x 10 x 5 dimensions
arr <- array(unlist(x.train), 
             dim = c(nimages, img_rows, img_cols, nfeatures), 
             dimnames = list(image_number = 1:nimages, 
                             x  = 1:10, 
                             y  = 1:10, 
                             features = c(colnames(x.train)[1],
                                          colnames(x.train)[4:7])))

# Redefine  dimension of train/test inputs
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, nfeatures)

# Scale and normalize values
x_train <- x_train / 255
x_test <- x_test / 255

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')