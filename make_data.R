### make_data.R
# Tom Gause
# last edited 3/9/22

system("curl https://wsim-datasets.s3.us-east-2.amazonaws.com/hindcasts_usa.tar;
        tar -xvf hindcasts_usa.tar;
        rm hindcasts_usa.tar
       ", intern = TRUE)

if (!require("Dict")) { install.packages("Dict") }
library(Dict)
library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(raster)
library(rgdal)
library(terra)
library(proj4)

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
  map_tt[x] <- hindcast_all[match(x, hindcast_all$forecast_target, nomatch=-1),]$obs_tmp_k
}

#make the lags with dplyr. Turns out forecast_timestamp was useful after all!
hindcast_all <- hindcast_all %>%
  mutate(lag1 = map_tt[lag1],
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

elev_cells <- NULL
#check for elevation data and generate if it doesn't exist
if (!file.exists("/data/elev_cells.rds")) {
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

  saveRDS(elev_cells, file = "./data/elev_cells.rds")
} else {
  elev_cells <- readRDS("./data/elev_cells.rds")
}

#now we append our new column...
hindcast_all <- hindcast_all %>%
  mutate(elev = c(rep(elev_cells, nrow(hindcast_all)/14056)))

#and save!
saveRDS(hindcast_all, file = "./data/hindcast_all.rds")



