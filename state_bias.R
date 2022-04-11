### get_state_bias.R
# Tom Gause
# last edited 4/11/2022

library(tidyverse)
library(lubridate)
library(zoo)
library(dplyr)
library(sf)
library(terra)
library(rstudioapi)
library(spData)
library(ggplot2)



########################################
#read and concat all data
########################################

#set working directory to location of make_data_dev.R
setwd(dirname(getActiveDocumentContext()$path)) 

#get paths to all data
df <- c(list.files(path = "./data/old",
                   pattern = "hindcasts.*.rds",
                   full.names = TRUE))#,

# Concat all data
print("concatenating hindcast subsets...")
N <- length(df)
tmp <- NULL
tmpL <- list()
for(i in 1:N) {
  print(i)
  tmp <- readRDS(df[i])
  tmpL[[i]] <- tmp
  gc() #clear garbage memory
}
hindcast.all <- dplyr::bind_rows(tmpL)
rm(tmpL)
rm(tmp)

# Reduce dataset size by 90%
hindcast.all <- sample_frac(hindcast.all, size = 0.1)
gc()
cell.samples <- hindcast.all %>%
  group_by(fcst_cell) %>%
  dplyr::slice_sample(n=100)


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

# Get cells of all samples
cell.samples.states <- lonlat_to_state(cell.samples[,c('x','y')])
cell.samples.states <- data.frame(state = cell.samples.states)

# Don't forget to ungroup...
cell.samples <- cell.samples %>% ungroup()
# Mutate
cell.samples <- cell.samples %>%
  mutate(state = cell.samples.states$state,
         bias = abs(obs_tmp_k - fcst_tmp_k)) #TODO: get actual variable names

# Summarize data
f <- cell.samples %>%
  dplyr::group_by(state) %>%
  summarize(mean = mean(bias), n = n())
view(f)

cell.states <- cell.samples %>%
  group_by(fcst_cell)