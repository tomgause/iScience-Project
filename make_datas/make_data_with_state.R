### make_data.R
# Tom Gause
# last edited 4/11/2022

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

#concatenate
hindcast_all <- dplyr::bind_rows(tmpL)
rm(tmpL)
rm(tmp)

## Source:  https://stackoverflow.com/questions/8751497/latitude-longitude-coordinates-to-state-code-in-r
## pointsDF: A data.frame whose first column contains longitudes and
##           whose second column contains latitudes.
##
## states:   An sf MULTIPOLYGON object with 50 states plus DC.
##
## name_col: Name of a column in `states` that supplies the states'
##           names.
lonlat_to_state <- function(pointsDF,
                            states = spData::us_states,
                            name_col = "NAME") {
  ## Convert points data.frame to an sf POINTS object
  pts <- st_as_sf(pointsDF, coords = 1:2, crs = 4326)
  
  ## Transform spatial data to some planar coordinate system
  ## (e.g. Web Mercator) as required for geometric operations
  states <- st_transform(states, crs = 3857)
  pts <- st_transform(pts, crs = 3857)
  
  ## Find names of state (if any) intersected by each point
  state_names <- states[[name_col]]
  ii <- as.integer(st_intersects(pts, states))
  state_names[ii]
}

pixels <- length(unique(hindcast_all$fcst_cell))
# Grab subset of unique pixels
unique.pixels <- hindcast_all[1:pixels,c('x','y','fcst_cell')]
unique.pixels.states <- lonlat_to_state(unique.pixels[,c('x','y')])
unique.pixels <- unique.pixels %>%
  mutate(state = unique.pixels.states)
vermont.cells <- unique.pixels %>%
  filter(state == "Vermont") %>%
  select(fcst_cell)

# Select all pixels in Vermont
hindcast_subset <- hindcast_all%>%
  filter(fcst_cell %in% vermont.cells$fcst_cell)

#convert date columns to YearMonth objects
hindcast_subset$forecast_timestamp <- substring(hindcast_subset$forecast_timestamp, 1, 6)
hindcast_subset$forecast_timestamp <- ym(hindcast_subset$forecast_timestamp)
hindcast_subset$forecast_target <- ym(hindcast_subset$forecast_target)

gc()
rm(hindcast_all)

