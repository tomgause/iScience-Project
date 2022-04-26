##process_data_CNN
# Acadia Hegedus
# 4/26/22

#Define constants
img_rows <- 10
img_cols <- 10
nfeatures <- 5 #predictor variables: elevation, fcst_tmp_k, lead, target_month,
                                  #forecast_target

#Data preparation

#Read in data set
train.data <- readRDS(file.choose()) #TO DO: Select correct data set
train.data1 <- train.data[1:1000,] #TO DO: delete

x.train <- train.data1 %>%
  dplyr::select(forecast_target, x, y, target_month, lead, fcst_tmp_k, 
                obs_tmp_k) #TO DO: select correct variables needed
y.train <- train.data1%>%
  dplyr::select(bias_tmp)

#find dimensions for array
nimages <- x.train%>% #number of images
  dplyr::select(x,y)%>%
  unique()%>%
  nrow()

#convert dataframe to array of X dimensions
arr <- array(unlist(x.train), 
             dim = c(nimages, img_rows, img_cols, nfeatures), 
             dimnames = list(image_number = 1:nimages, 
                             x  = 1:10, 
                             y  = 1:10, 
                             features = colnames(x.train)))

# Redefine  dimension of train/test inputs
x_train <- array_reshape(x_train, c(nrow(x_train), img_rows, img_cols, 1))
input_shape <- c(img_rows, img_cols, nfeatures)

# Scale and normalize values
x_train <- x_train / 255
x_test <- x_test / 255

cat('x_train_shape:', dim(x_train), '\n')
cat(nrow(x_train), 'train samples\n')
cat(nrow(x_test), 'test samples\n')

# Convert class vectors to binary class matrices
# convert outputs to binary (10 rows)
y_train <- to_categorical(y_train, num_classes)
y_test <- to_categorical(y_test, num_classes)