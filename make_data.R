### make_data.R
# Tom Gause, Acadia Hegedus, and Katelyn Mei
# last edited 3/29/22

# system("cd data;
#          curl https://wsim-datasets.s3.us-east-2.amazonaws.com/hindcasts_usa.tar;
#          tar -xvf hindcasts_usa.tar;
#          cd ..",
#         intern = TRUE)

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)

#get paths to all data
df <- list.files(path = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data",
                 pattern = "hindcasts.*.rds",
                 full.names = TRUE)


print("Concatenating All Data...")
#concat all data. Did it the smart way, not the stupid way!
#test
N <- length(df)
tmp <- NULL
tmpL <- list()
for(i in 1:N) {
  print(i)
  tmp <- readRDS(df[i])
  tmpL[[i]] <- tmp
}
hindcast_all <- dplyr::bind_rows(tmpL)

#saveRDS(hindcast_all, file = "C:\\Users\\tgause\\iScience_Project\\data")
#don't save yet as this will break R

#choose 10% of pixels at random
#read in hindcast_month_1_lead_1.rds
sample_data <- readRDS(file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcasts_month_1_lead_1.rds") 

all_pixels <- sample_data%>%
  dplyr::select(fcst_cell)%>%
  unique()

pixel_subset <- all_pixels[sample(nrow(all_pixels), nrow(all_pixels)*0.1), ]

hindcast_subset <- hindcast_all%>%
  filter(fcst_cell %in% pixel_subset)

#Add on temperature lags

hindcast_subset$forecast_timestamp <- substring(hindcast_subset$forecast_timestamp, 1, 6)
hindcast_subset$forecast_timestamp <- ym(hindcast_subset$forecast_timestamp)

#create lag time identifier function
#input: timestamp, lag time wanted
#output: Observed YearMonth value needed
find_lag_id <- function(timestamp, lag){
  new_month <- month(timestamp) - lag
  if (new_month < 1) {
    output_year <- year(timestamp) - 1
    output_month <- 12 + new_month
  }
  else {
    output_year <- year(timestamp)
    output_month <- new_month
  }
  if (output_month < 10){
    output_month <- paste0("0",output_month)
  }
  output <- paste0(output_year,output_month)
  return(output)
}

hindcast_subset <- hindcast_subset%>%
  mutate(lag1 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 1),
         lag2 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 2),
         lag3 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 3),
         lag4 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 4),
         lag5 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 5),
         lag6 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 6),
         lag7 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 7),
         lag8 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 8),
         lag9 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 9),
         lag10 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 10),
         lag11 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 11),
         lag12 = find_lag_id(timestamp = hindcast_subset$forecast_timestamp, lag = 12))
#this works but gives a weird error?

#Create observed data set (Lag Data)
lag.data <- hindcast_subset%>%
  dplyr::select(forecast_target,obs_pr_m_day,obs_tmp_k, fcst_cell)%>%
  unique()

colnames(lag.data)[1] <- "date"

#left_join data set with lag.data for each lag column, changing lagi to observed temp
hindcast_subset_temp_lags <- hindcast_subset
for (i in (1:12)){
  mylag <- paste0("lag",i)
  hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, lag.data, by = c("fcst_cell", 
                                                                                     setNames("date", mylag)))
  hindcast_subset_temp_lags[mylag] <- hindcast_subset_temp_lags[,26] #grab observed temp column
  hindcast_subset_temp_lags <- hindcast_subset_temp_lags[,1:24]
}

#save!
saveRDS(hindcast_subset_temp_lags, file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcast_subset_temp_lags.rds")

#read in above saved file so we don't have to run it again:
hindcast_subset_temp_lags <- readRDS(file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcast_subset_temp_lags.rds")

elev_cells <- NULL
#check for elevation data and generate if it doesn't exist
if (!file.exists("./data/elev_cells.rds")) {
  #there's definitely a better way but this is functional
  #TODO: ask in next meeting "how to do this in a not dumb way..."
  cells <- data.frame(
    x <- hindcast_all$x[1:14056],
    y <- hindcast_all$y[1:14056])
  
  elev <- rast('./data/gmted2010_ERA5_quarter_degree.tif')
  elev_df <- as.data.frame(elev, xy=TRUE)
  
  for (i in 1:14056) {
    is_cell <- (elev_df$x == cells$x[i]) & (elev_df$y == cells$y[i])
    elev_cells[i] <- elev_df$gmted2010_ERA5_quarter_degree[is_cell]
  }
  
  saveRDS(elev_cells, file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\elev_cells.rds")
} else {
  elev_cells <- readRDS("C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\elev_cells.rds")
}

#make elev_cells into a data frame
elev_df <- data.frame(fcst_cell = sample_data$fcst_cell, x = sample_data$x, 
                      y = sample_data$y, elevation = elev_cells)

#sanity check: looks right!
# elev_df%>%
#   ggplot(mapping = aes(x = x, y = y, color = elevation))+
#   geom_point()

#remove x,y in elevation data after we graph to make joining easier
elev_df2 <- elev_df%>%
  dplyr::select(-c(x,y))

print("Appending elevation data...")
#now we append our new column...
hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, elev_df2, 
                                       by = c("fcst_cell"))

#why is this giving me an error? "cannot allocate vector of size 235 Gb"

print("Converting forecast_target to Date...")
#convert forecast_target to Date object with day=1 for downstream tasks.
hindcast_subset_temp_lags <- hindcast_subset_temp_lags %>%
  mutate(forecast_target = as.Date(paste0(hindcast_subset_temp_lags, "01"),
                                   format = "%Y%m%d",
                                   tz = "EST"))

print("Saving...")
#and save!
saveRDS(hindcast_subset_temp_lags, file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcast_subset_temp_lags_elev.rds")



