### make_data.R
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# last edited 4/1/2022

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(terra)

#get paths to all data
df <- list.files(path = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data",
                 pattern = "hindcasts.*.rds",
                 full.names = TRUE)


print("Concatenating All Data...")
#concat all data
N <- length(df)
tmp <- NULL
tmpL <- list()
for(i in 1:N) {
  print(i)
  tmp <- readRDS(df[i])
  tmpL[[i]] <- tmp
}
hindcast_all <- dplyr::bind_rows(tmpL)

#choose 10% of pixels at random
#read in hindcast_month_1_lead_1.rds
sample_data <- readRDS(file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcasts_month_1_lead_1.rds") 

all_pixels <- sample_data%>%
  dplyr::select(fcst_cell)%>%
  unique()

pixel_subset <- all_pixels[sample(nrow(all_pixels), nrow(all_pixels)*0.1), ]

hindcast_subset <- hindcast_all%>%
  filter(fcst_cell %in% pixel_subset)

#add on temperature lags

#converted date columns to YearMonth objects
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

#get complete elevation data
elev <- rast('./data/gmted2010_ERA5_quarter_degree.tif')
elev_df <- as.data.frame(elev, xy=TRUE)

print("Mutating elevation data...")
#now we mutate our new column...
hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, elev_df, 
                                       by = c("x", "y"))

colnames(hindcast_subset_temp_lags)[25] <- "elevation" 

print("Saving...")
#and save!
saveRDS(hindcast_subset_temp_lags, file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcast_subset_temp_lags_elev.rds")


