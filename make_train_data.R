### make_data.R
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# last edited 4/2/2022

# # old data. Copy and past this inside iScience_Project/data
# curl https://wsim-datasets.s3.us-east-2.amazonaws.com/hindcasts_usa.ta --output hindcast_data.tar;
# tar -xvf hindcast_data.tar
# 
# # new data. Copy and paste these lines inside iScience_Project/data
# curl https://wsim-datasets.s3.us-east-2.amazonaws.com/forecasts_usa.tar --output new_hindcast_data.tar;
# tar -xvf new_hindcast_data.tar

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

#get paths to all data
df <- c(list.files(path = "./data/old",
                   pattern = "hindcasts.*.rds",
                   full.names = TRUE))#,
#list.files(path = "./data/new",
#pattern = "forecasts.*.rds",
#full.names = TRUE))

#concat all data
#we have to do it in 2 parts because it's too big!!!
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

#concatenate batch 1
#print("saving batch 1...")
hindcast_all <- dplyr::bind_rows(tmpL)
rm(tmpL)
rm(tmp)

# #concatenate batch 2
# tmp <- NULL
# tmpL <- NULL
# for(i in 1:(N/2)) {
#   print(i+N/2)
#   tmp <- readRDS(df[i+N/2])
#   tmpL[[i]] <- tmp
#   gc() #clear garbage memory
# }
# 
# #save batch 2
# print("saving batch 2...")
# hindcast_all_2 <- dplyr::bind_rows(tmpL)
# #free list memory, we're done with it now!
# rm(tmpL)
# rm(tmp)

#try to concatenate everything...
#hindcast_all <- rbind(hindcast_all_1, hindcast_all_2)

#free unused memory
#rm(hindcast_all_1, hindcast_all_2)

#choose 10% of pixels at random
#read in hindcast_month_1_lead_1.rds
sample_data <- readRDS(file = df[1])

all_pixels <- sample_data%>%
  dplyr::select(fcst_cell)%>%
  unique()

pixel_subset <- all_pixels[sample(nrow(all_pixels), nrow(all_pixels)*0.1), ]

hindcast_subset <- hindcast_all%>%
  filter(fcst_cell %in% pixel_subset)



########################################
#add on temperature lags for months 1-12
########################################

#convert date columns to YearMonth objects
hindcast_subset$forecast_timestamp <- substring(hindcast_subset$forecast_timestamp, 1, 6)
hindcast_subset$forecast_timestamp <- ym(hindcast_subset$forecast_timestamp)
hindcast_subset$forecast_target <- ym(hindcast_subset$forecast_target)

#add lag years to join by
hindcast_subset <- hindcast_subset%>%
  mutate(lag1 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-1)),
         lag2 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-2)),
         lag3 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-3)),
         lag4 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-4)),
         lag5 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-5)),
         lag6 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-6)),
         lag7 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-7)),
         lag8 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-8)),
         lag9 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-9)),
         lag10 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-10)),
         lag11 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-11)),
         lag12 = add_with_rollback(hindcast_subset$forecast_timestamp, months(-12)))

#create observed data set (lag.data)
lag.data <- hindcast_subset%>%
  dplyr::select(forecast_target,obs_pr_m_day,obs_tmp_k, fcst_cell)%>%
  unique()

colnames(lag.data)[1] <- "date"

#left_join hindcast_subset with lag.data for each lag column, changing lagi to observed temp
hindcast_subset_temp_lags <- hindcast_subset
for (i in (1:12)){
  mylag <- paste0("lag",i)
  hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, lag.data, by = 
                                           c("fcst_cell", setNames("date", mylag)))
  hindcast_subset_temp_lags[mylag] <- hindcast_subset_temp_lags[,26] #grab observed temp column
  hindcast_subset_temp_lags <- hindcast_subset_temp_lags[,1:24]
}



########################################
#append elevation data for all cells
########################################

#get complete elevation data
elev <- rast('./data/gmted2010_ERA5_quarter_degree.tif')
elev_df <- as.data.frame(elev, xy=TRUE)
hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, elev_df, 
                                       by = c("x", "y"))
colnames(hindcast_subset_temp_lags)[25] <- "elevation" 



########################################
#save subset
########################################

#get and format current datetime for filename
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)

#format filename as:
# hindcast_subset_, number of selected rows, _, current date and time
filename <- paste0("./data/", "train_subset_", nrow(pixel_subset), "_", currentTime, ".RDS")

print(paste0("saving as ", filename, "..."))
saveRDS(hindcast_subset_temp_lags, file = filename)


