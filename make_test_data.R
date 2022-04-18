### make_data.R
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# last edited 4/17/2022

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

#get relevant cells from train data
train_cells <- readRDS("./data/train_vermont_2022-04-11_20-54-09.RDS") %>%
  dplyr::select(fcst_cell) %>%
  unique()


#get paths to all data
df <- c(list.files(path = "./data/new",
                   pattern = "forecasts.*.rds",
                   full.names = TRUE))

#concat all data
print("concatenating hindcast subsets...")
N <- length(df)
tmp <- NULL
tmpL <- list()
for(i in 1:N) {
  print(i)
  tmp <- readRDS(df[i])
  tmpL[[i]] <- tmp
  gc()
}

hindcast_all <- dplyr::bind_rows(tmpL)
rm(tmpL)
rm(tmp)

#choose relevant cells
hindcast_subset <- hindcast_all%>%
  filter(fcst_cell %in% train_cells$fcst_cell)

#remove this column bc we don't know what it does
hindcast_subset <- subset(hindcast_subset, select = c(-obs_cell))

########################################
#add on temperature bias lags for months 1-12
########################################

#convert date columns to YearMonth objects
hindcast_subset <- hindcast_subset %>% 
  mutate(forecast_timestamp_rm =  substring(hindcast_subset$forecast_timestamp, 1, 6))

hindcast_subset$forecast_timestamp_rm<- ym(hindcast_subset$forecast_timestamp_rm)
hindcast_subset$forecast_target <- ym(hindcast_subset$forecast_target)

gc()
rm(hindcast_all)

# Create Bias columns. We'll be predicting for this (these) valaue
hindcast_subset <- hindcast_subset %>%
  mutate(bias.t = obs_tmp_k - fcst_tmp_k,
         bias.p = obs_pr_m_day - fcst_pr_m_day)

#add lag years to join by
hindcast_subset <- hindcast_subset%>%
  mutate(lag1.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-1)),
         lag2.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-2)),
         lag3.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-3)),
         lag4.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-4)),
         lag5.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-5)),
         lag6.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-6)),
         lag7.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-7)),
         lag8.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-8)),
         lag9.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-9)),
         lag10.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-10)),
         lag11.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-11)),
         lag12.t = add_with_rollback(hindcast_subset$forecast_timestamp_rm, months(-12)))

#create bias data set (lag.data)
#added forecast_timestamp here because we have a different bias for each timestamp in each cell
lag.data.t <- hindcast_subset%>%
  group_by(forecast_target,fcst_cell) %>% 
  summarise(average_monthly_bias = mean(bias.t))

colnames(lag.data.t)[1] <- "date"


#left_join hindcast_subset with lag.data.t for each lag column, changing lagi to temp bias
hindcast_subset_temp_lags <- hindcast_subset %>% dplyr::select(-forecast_timestamp)
for (i in (1:12)){
  mylag <- paste0("lag",i,".t")
  hindcast_subset_temp_lags_joined <- left_join(hindcast_subset_temp_lags, lag.data.t, 
                                                by = c("fcst_cell", setNames("date", mylag)))
  
  hindcast_subset_temp_lags[mylag] <- hindcast_subset_temp_lags_joined[,28] #grab bias temp column
  #hindcast_subset_temp_lags <- hindcast_subset_temp_lags[,1:24]
}
gc()

#remove NAs
hindcast_subset_temp_lags <- na.omit(hindcast_subset_temp_lags)

########################################
#append elevation data for all cells
########################################

#get complete elevation data
elev <- rast('./data/gmted2010_ERA5_quarter_degree.tif')
elev_df <- as.data.frame(elev, xy=TRUE)
hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, elev_df, 
                                       by = c("x", "y"))
colnames(hindcast_subset_temp_lags)[28] <- "elevation" 



########################################
#save subset
########################################

#get and format current datetime for filename
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)

#format filename as:
# hindcast_subset_, number of selected rows, _, current date and time
filename <- paste0("./data/", "test_subset_", currentTime, ".RDS")

print(paste0("saving as ", filename, "..."))
saveRDS(hindcast_subset_temp_lags, file = filename)