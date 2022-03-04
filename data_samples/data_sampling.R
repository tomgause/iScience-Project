library(tidyverse)

df <- list.files(path = "/Users/tomgause/Desktop/iScience_data/hindcasts_usa/",
                      pattern = ".rds",
                      full.names = TRUE)

#generates 5 random samples from each month and lead time
sample_size <- 5
N <- length(df)
index <- NULL
temp <- NULL

hindcast_sample_5_from_each <- NULL
hindcast_sample_100_from_each <- NULL
hindcast_sample_1_cell_1_forecast <- NULL
hindcast_sample_1_cell_all_forecaasts <- NULL
hindcast_sample_100_cells_all_forecasts <- NULL
hindcast_sample_all_cells_1_forecast <- NULL

for(i in 1:N) {
  print(i)
  temp <- readRDS(df[i]) %>%
    select("target_month", "x", "y", "cell", "target_month",
           "lead", "fcst_tmp_k", "obs_tmp_k")

  index1 <- sample(nrow(temp), 5, replace=FALSE)
  index2 <- sample(nrow(temp), 100, replace=FALSE)
  
  cell1 <-temp %>%
    filter(cell == temp$cell[sample(nrow(temp), 1, replace=FALSE),]) %>%

  
  
  hindcast_sample_5_from_each <- rbind(hindcast_data_set, temp[index1,])
  hindcast_sample_100_from_each <- rbind(hindcast_data_set, temp[index2,])
  
}
view(hindcast_data_set)
saveRDS(hindcast_sample_5_from_each, file = "hindcast_sample_5_from_each.rds")