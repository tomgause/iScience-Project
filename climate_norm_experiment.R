### Climate Norm Experiment
# Acadia Hegedus
# 4/22/22


#For the state of Vermont, I found that taking the climate norm of a 
#pixel gave a lower MSE than all other methods (including original CFSv2 
#predictions, quantile matching method, and RF). 

#How does the climate norm compare to quantile matching and original CFSv2 
#predictions for the entire US?

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(terra)
library(rstudioapi)

########################################
#read and concat all data
########################################

#set working directory to location of make_data_dev.R
setwd(dirname(getActiveDocumentContext()$path)) 

#get paths to all training data
df <- c(list.files(path = "./data/old",
                   pattern = "hindcasts.*.rds",
                   full.names = TRUE))

# load in dataset
print("concatenating hindcast subsets...")
N <- length(df)
tmp <- NULL
tmpL <- list()
for(i in 1:N) {#(N/2)) {
  print(i)
  tmp <- readRDS(df[i])
  tmpL[[i]] <- tmp
  gc() #clear garbage memory
}

# concatenate
rm(tmp)
train.data <- dplyr::bind_rows(tmpL)
#rm(tmpL)


#get paths to all testing data
df <- c(list.files(path = "./data/new",
                   pattern = "forecasts.*.rds",
                   full.names = TRUE))

#concat all data
print("concatenating forecast subsets...")
N <- length(df)
tmp <- NULL
tmpL <- list()
for(i in 1:N) {
  print(i)
  tmp <- readRDS(df[i])
  tmpL[[i]] <- tmp
  gc()
}

test.data <- dplyr::bind_rows(tmpL)
#rm(tmpL)
#rm(tmp)

#Generate climate norm data set
#climate.norm method 2: only use train data for climate norm calculation!
climate.norm2 <- train.data%>%
  dplyr::select(target_month, obs_tmp_k, fcst_cell, forecast_target)%>%
  unique()

climate.norm2 <- climate.norm2%>%
  group_by(fcst_cell, target_month)%>%
  summarize(mean_temp = mean(obs_tmp_k))

# Select and store all forecast cells
sample.cells <- test.data %>%
  dplyr::select(fcst_cell) %>%
  unique()

# Create DF to store mse from quantile matching, base, and climate norm
cell.error <- data.frame(0,0,0)
colnames(cell.error) <- c("qm.mse", "base.mse",
                          "climate.norm.mse")

for (cell in sample.cells[,1]) {
  
  # Select a single point
  cell.data <- test.data %>%
    filter(fcst_cell == cell)
  
  # Add on climate norm predictions
  climate.norm.cell <- climate.norm2%>%
    filter(fcst_cell == cell)
  climate.norm.preds <- left_join(cell.data, climate.norm.cell, 
                                  by = c("fcst_cell", "target_month"))
  
  # Generate mse for qm, base, and climate.norm against obs
  qm.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_qm_tmp_k)^2)
  base.mse <- mean((cell.data$obs_tmp_k - cell.data$fcst_tmp_k)^2)
  climate.norm.mse <- mean((climate.norm.preds$obs_tmp_k - 
                              climate.norm.preds$mean_temp)^2)
  
  cell.error <- rbind(cell.error, data.frame(qm.mse,base.mse,
                                             climate.norm.mse))
}

#remove 0 row
cell.error <- cell.error[2:nrow(cell.error),]
mean.errors <- data.frame(colMeans(cell.error[,]))
mean.errors <- mean.errors %>%
  mutate(bias.correction.method = rownames(mean.errors))
colnames(mean.errors)[1] = "MSE"
mean.errors$bias.correction.method <- c("Quantile Matching","Base","Climate Norm")

#generate plot
mean.errors %>%
  ggplot(mapping = aes(x = bias.correction.method, y = MSE))+
  geom_bar(stat = 'identity',fill="#b4eeb4",color = "#b4eeb4")+
  xlab("Bias Correction Method")+
  theme_test()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2014-2021 US Data")


