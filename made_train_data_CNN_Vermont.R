### make_train_data_CNN.R
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# last edited 4/25/2022

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
hindcast_all <- dplyr::bind_rows(tmpL)
#rm(tmpL)
rm(tmp)

pixels <- length(unique(hindcast_all$fcst_cell))
# Grab subset of unique pixels
unique.pixels <- hindcast_all[1:pixels,c('x','y','fcst_cell')]

#Select the square area around Vermont for CNN 
state.subset<-"Vermont"
selected.cells <- unique.pixels %>%
  filter(x > -73.5 & x < -70.75 & y > 42.5 & y < 45.25)

# Select all pixels in Vermont
hindcast_subset <- hindcast_all%>%
  filter(fcst_cell %in% selected.cells$fcst_cell)

#convert date columns to YearMonth objects
#keep exact timestamp here because we need it to do left-join of bias.t and bias.p! 
hindcast_subset <- hindcast_subset %>% 
  mutate(forecast_timestamp_rm =  substring(hindcast_subset$forecast_timestamp, 1, 6))

hindcast_subset$forecast_timestamp_rm<- ym(hindcast_subset$forecast_timestamp_rm)
hindcast_subset$forecast_target <- ym(hindcast_subset$forecast_target)

gc()
rm(hindcast_all)


########################################
#add on temperature bias lags for months 1-12
########################################

# Create Bias columns. We'll be predicting for this (these) valaue
hindcast_subset <- hindcast_subset %>%
  mutate(bias.t = obs_tmp_k - fcst_tmp_k,
         bias.p = obs_pr_m_day - fcst_pr_m_day)

#add lag years to join by
#lagn.t -->average predicted bias n month(s) ago
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

#rename forecast_target to be "date"
colnames(lag.data.t)[1] <- "date"


#left_join hindcast_subset with lag.data for each lag column, changing lag to temp bias
hindcast_subset_temp_lags <- hindcast_subset %>% select(-forecast_timestamp)
for (i in (1:12)){
  mylag <- paste0("lag",i,".t")
  hindcast_subset_temp_lags_joined <- left_join(hindcast_subset_temp_lags, lag.data.t, 
                                                by = c("fcst_cell", setNames("date", mylag)))
  
  hindcast_subset_temp_lags[mylag] <- hindcast_subset_temp_lags_joined[,27] #grab bias temp column
  #hindcast_subset_temp_lags <- hindcast_subset_temp_lags[,1:24]
}
gc()



#add in bias_precip bias LATER if we want
# hindcast_subset_all_lags <- hindcast_subset_temp_lags %>%
#   mutate(lag1.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-1)),
#         lag2.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-2)),
#         lag3.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-3)),
#         lag4.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-4)),
#         lag5.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-5)),
#         lag6.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-6)),
#         lag7.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-7)),
#         lag8.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-8)),
#         lag9.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-9)),
#         lag10.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-10)),
#         lag11.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-11)),
#         lag12.p = add_with_rollback(hindcast_subset$forecast_timestamp, months(-12)))
# 
# 
# # do the same thing but for precipitation bias
# hindcast_subset_all_lags <- hindcast_subset_temp_lags
# for (i in (1:12)){
#   mylag <- paste0("lag",i,".p")
#   hindcast_subset_all_lags <- left_join(hindcast_subset_all_lags, lag.data, by = 
#                                            c("fcst_cell", setNames("date", mylag)))
#   hindcast_subset_all_lags[mylag] <- hindcast_subset_all_lags[,40] #grab bias precip column
#   hindcast_subset_all_lags <- hindcast_subset_all_lags[,1:38]
# }
# gc()


########################################
#append elevation data for all cells
########################################

#get complete elevation data
elev <- rast('./data/gmted2010_ERA5_quarter_degree.tif')
elev_df <- as.data.frame(elev, xy=TRUE)
hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, elev_df, 
                                       by = c("x", "y"))
colnames(hindcast_subset_temp_lags)[27] <- "elevation" 



########################################
#save subset
########################################

#get and format current datetime for filename
currentTime <- Sys.time()
currentTime <- gsub(" ", "_", currentTime)
currentTime <- gsub(":", "-", currentTime)

#format filename as:
# hindcast_subset_, number of selected rows, _, current date and time
filename <- paste0("./data/", "train_subset_", state.subset, "_CNN_", currentTime, ".RDS")

print(paste0("saving as ", filename, "..."))
saveRDS(hindcast_subset_temp_lags, file = filename)
