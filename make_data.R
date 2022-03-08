### make_data.R
# Tom Gause
# last edited 3/7/22

system("mkdir data;
        curl https://wsim-datasets.s3.us-east-2.amazonaws.com/hindcasts_usa.tar;
        tar -xvf hindcasts_usa.tar ", intern = TRUE)


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


#read elevation data
files <- dir("data/", recursive=TRUE, full.names=TRUE, pattern=".tif$")
dfr <- do.call(rbind, lapply(files, raster::values))

y = readGDAL(system.file("gmted2010_ERA5_quarter_degree.tif", package = "rgdal")[1])
head(y@data)

# Using raster to create stack from individual bands and coerce to SpatialGridDataFrame
y <- stack( raster(system.file("pictures/Rlogo.jpg", package = "rgdal")[1], band=1),
            raster(system.file("pictures/Rlogo.jpg", package = "rgdal")[1], band=2),
            raster(system.file("pictures/Rlogo.jpg", package = "rgdal")[1], band=3))
class(y)

y <- as(y, "SpatialGridDataFrame")
class(y)

# do something to the data and write raster to disk  
y@data <- y@data * 0.01  
writeGDAL(y, "corrected.tif", drivername="GTiff", type="Float32") 

saveRDS(hindcast_all, file = "/data/hindcast_all.rds")



