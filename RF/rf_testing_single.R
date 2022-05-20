### rf testing
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# 4/19/2022

## Default repo
local({r <- getOption("repos")
       r["CRAN"] <- "http://cran.r-project.org"
       options(repos=r)
})
packages <- c("tidyverse", "tidytext", "ggplot2", "ranger", "lubridate")
install.packages(setdiff(packages, rownames(installed.packages()))) 

library(tidyverse)
library(tidytext)
library(ggplot2)
library(ranger)
library(lubridate)

#set working directory to file location
#setwd(dirname(getActiveDocumentContext()$path)) 

# Load in our RFs and our test data; and train data for climate norm calculations
#to do: check to see that loading in RF works
#rf.all <- final.rf # this doesn't work: load("finalVTrf.allpixels.RData")
train <- readRDS("/storage/tgause/iScience_tom/iScience_Project/data/VT_train.RDS")
test <- readRDS("/storage/tgause/iScience_tom/iScience_Project/data/VT_test.RDS")


#remove data before March 2014 from the test set to make testing fair
test <- test%>%
  filter(forecast_target > "2014-02-01")

# Select and store all forecast cells
sample.cells <- test %>%
  dplyr::select(fcst_cell) %>%
  unique()

# Create DF to store mse from RF, quantile matching, and base
cell.error <- data.frame(0,0,0,0,0)
colnames(cell.error) <- c("qm.mse", "base.mse","rf.mse", "x", "y")

#rf.identifier <- 1
for (cell in sample.cells[,1]) {
  # Select a single point
  cell.test.data <- test %>%
    filter(fcst_cell == cell)

  cell.train.data <- train %>%
    filter(fcst_cell == cell) %>%
    dplyr::select(x, y, bias.t, fcst_tmp_k, fcst_cell, 
          elevation, lead, target_month, forecast_target)
  
  # Generate mse for qm, base, and climate.norm against obs
  qm.mse <- mean((cell.test.data$obs_tmp_k - cell.test.data$fcst_qm_tmp_k)^2)
  base.mse <- mean((cell.test.data$obs_tmp_k - cell.test.data$fcst_tmp_k)^2)
\
  rf.cell <- ranger(bias.t ~ ., # More efficient
               data = cell.train.data,
               num.trees = 2000,
               mtry = 8, 
               min.node.size = 3,
               sample.fraction = 1,
               num.threads = 32, # 20 threads on Alex machine
               oob.error = TRUE)
  
  # Generate predictions to find rf mse against obs
  pred <- predict(rf.cell, data = cell.test.data)
  rf.bias.predictions <- fcst_tmp_k + pred$predictions
  rf.mse <- mean((cell.test.data$obs_tmp_k - rf.bias.predictions)^2)
  
  cell.error <- rbind(cell.error, data.frame(qm.mse,
                                             base.mse,
                                             rf.mse,
                                             cell.train.data[1,]$x,
                                             cell.train.data[1,]$y))
  
  # Method for cycling through RFs
  #rf.identifier <- rf.identifier + 1
}

#remove 0 row
cell.error <- cell.error[2:nrow(cell.error),]
mean.errors <- data.frame(colMeans(cell.error[,]))
mean.errors <- mean.errors %>%
  mutate(bias.correction.method = rownames(mean.errors))
colnames(mean.errors)[1] = "MSE"
mean.errors$bias.correction.method <- c("Quantile Matching","Base","RF All Pixels", "x", "y")

#generate plot
mean.errors %>%
  ggplot(mapping = aes(x = bias.correction.method, y = MSE))+
  geom_bar(stat = 'identity',fill="#b4eeb4",color = "#b4eeb4")+
  xlab("Bias Correction Method")+
  theme_test()+
  theme(plot.title = element_text(hjust = 0.5))+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2014-2021 Data")


# Save results of testing
# First, grab and format the current time
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)
filename <- paste0("/storage/tgause/iScience_tom/iScience_Project/RF/", "rf_single_test_results_", currentTime, ".RDS")

print("Saving results...")

# And save!
saveRDS(cell.error, file = filename)