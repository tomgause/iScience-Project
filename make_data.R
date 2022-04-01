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

#pick one pixel
hindcast_one <- hindcast_subset%>%
  filter(fcst_cell == "236396")

# #create lag time identifier function
# #input: timestamp, lag time wanted
# #output: Observed YearMonth value needed
# find_lag_id <- function(timestamp, lag){
#   new_month <- month(timestamp) - lag
#   if (new_month < 1) {
#     output_year <- year(timestamp) - 1
#     output_month <- 12 + new_month
#   }
#   else {
#     output_year <- year(timestamp)
#     output_month <- new_month
#   }
#   if (output_month < 10){
#     output_month <- paste0("0",output_month)
#   }
#   output <- paste0(output_year,output_month)
#   return(output)
# }

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

#Create observed data set (Lag Data)
lag.data <- hindcast_subset%>%
  dplyr::select(forecast_target,obs_pr_m_day,obs_tmp_k, fcst_cell)%>%
  unique()

colnames(lag.data)[1] <- "date"

#left_join data set with lag.data for each lag column, changing lagi to observed temp
hindcast_subset_temp_lags <- hindcast_subset
for (i in (1:12)){
  mylag <- paste0("lag",i)
  hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, lag.data, by = 
                                           c("fcst_cell", setNames("date", mylag)))
  hindcast_subset_temp_lags[mylag] <- hindcast_subset_temp_lags[,26] #grab observed temp column
  hindcast_subset_temp_lags <- hindcast_subset_temp_lags[,1:24]
}

#save!
saveRDS(hindcast_subset_temp_lags, file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcast_subset_temp_lags.rds")

#read in above saved file so we don't have to run it again:
hindcast_subset_temp_lags <- readRDS(file = "C:\\Users\\ahegedus\\Documents\\iScience_Project\\data\\hindcast_subset_temp_lags.rds")

#get complete elevation data
elev <- rast('./data/gmted2010_ERA5_quarter_degree.tif')
elev_df <- as.data.frame(elev, xy=TRUE)

#sanity check: looks right!
# elev_df%>%
#   ggplot(mapping = aes(x = x, y = y, color = elevation))+
#   geom_point()

print("Appending elevation data...")
#now we append our new column...
hindcast_subset_temp_lags <- left_join(hindcast_subset_temp_lags, elev_df, 
                                       by = c("x", "y"))

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



