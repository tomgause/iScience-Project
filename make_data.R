### make_data.R
# Tom Gause
# last edited 3/7/22

system("mkdir data;
        curl https://wsim-datasets.s3.us-east-2.amazonaws.com/hindcasts_usa.tar;
        tar -xvf hindcasts_usa.tar ", intern = TRUE)


if (!require("Dict")) { install.packages("Dict") }
if (!require("zoo")) { install.packages("zoo") }
if (!require("elevatr")) { install.packages("elevatr") }
library(Dict)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(elevatr)

#change path to /data/
df <- list.files(path = "/Users/tomgause/Desktop/iScience_data/hindcasts_usa/",
                 pattern = ".rds",
                 full.names = TRUE)

#concat all data
hindcast_all <- NULL
N <- length(df)
for(i in 1:N) {
  sprintf("Reading %d/%d", i, N)
  hindcast_all <- rbind(hindcast_all, readRDS(df[2]) %>%
    mutate(lag1 = substring(forecast_timestamp, 1, 6)))
}

#temporal ordering by forecast timestamp (lag1 for more efficiency)
hindcast_all <- hindcast_all[order(hindcast_all$lag1,)]

#make dictionary from %Y%m to temperature (for efficiency)
map_tt <- NULL
map_tt <- dict(
  "sample" = "test", #do this too keep dplyr from breaking Dict library :(
  .class = "any",
  .overwrite = FALSE
)
#generate mappings
for (x in unique(hindcast_all$lag1)) {
  map_tt["198301"] <- hindcast_all[match("198301", hindcast_all$forecast_target, nomatch=-1),]$obs_tmp_k
}

#make the lags with dplyr. Turns out forecast_timestamp was useful after all!
hindcast_all <- hindcast_all %>%
  mutate(lag1 = c(rep(map_tt["198301"], nrow(hindcast_all))),
         #for each extra lag, we need to shift back 1 month.
         #monthly, there are unique 14056 cells, 24 forecasts, and 9 lead times,
         #so each lag needs to be 14056 * 24 * 9 = 3036096 rows
         lag2 = lag(lag1, 3036096 * 1),
         lag3 = lag(lag1, 3036096 * 2),
         lag4 = lag(lag1, 3036096 * 3),
         lag5 = lag(lag1, 3036096 * 4),
         lag6 = lag(lag1, 3036096 * 5),
         lag7 = lag(lag1, 3036096 * 6),
         lag8 = lag(lag1, 3036096 * 7),
         lag9 = lag(lag1, 3036096 * 8),
         lag10 = lag(lag1, 3036096 * 9),
         lag11 = lag(lag1, 3036096 * 10))

#view(head(hindcast_all, 100))
#view(tail(hindcast_all, 100))

saveRDS(hindcast_all, file = "hindcast_all.rds")

## Not run: 
mt_wash <- data.frame(x = -71.3036, y = 44.2700)
mt_mans <- data.frame(x = -72.8145, y = 44.5438)
mts <- rbind(mt_wash,mt_mans)
ll_prj <- "EPSG:4326"
get_elev_point(locations = mt_wash, prj = ll_prj)

