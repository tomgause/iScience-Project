library(tidyverse)

df <- list.files(path = "/Users/tomgause/Desktop/iScience_data/hindcasts_usa/",
                      pattern = ".rds",
                      full.names = TRUE)

#generates 5 random samples from each month and lead time
#sample_size <- 5
#N <- length(df)

#first 9 lead times
N <- 9

index <- NULL
temp <- NULL

hindcast_sample_5_from_each <- NULL
hindcast_sample_100_from_each <- NULL
hindcast_sample_1_cell_1_forecast <- NULL
hindcast_sample_1_cell_all_forecaasts <- NULL
hindcast_sample_100_cells_all_forecasts <- NULL
hindcast_sample_all_cells_1_month_1_lead <- NULL
hindcast_sample_all_cells_1_month_9_leads <- NULL

#"target_month", "x", "y", "fcst_cell", "target_month",
#"lead", "fcst_tmp_k", "obs_tmp_k", "forecast_timestamp"

for(i in 1:N) {
  print(i)
  temp <- readRDS(df[1]) %>% #14056
    filter(target_month == 1,
           as.integer(forecast_timestamp) %% 1000000 == 120200)
  
  
  test <- temp[index,]
  
  if (i == 1) {
    hindcast_sample_all_cells_1_month_1_lead <- temp_all
  }
  
  
  index1 <- sample(nrow(temp), 5, replace=FALSE)
  index2 <- sample(nrow(temp), 100, replace=FALSE)
  
  cell1 <-temp %>%
    filter(cell == temp$cell[sample(nrow(temp), 1, replace=FALSE),]) %>%

  
  
  hindcast_sample_5_from_each <- rbind(hindcast_data_set, temp[index1,])
  hindcast_sample_100_from_each <- rbind(hindcast_data_set, temp[index2,])
  
}
view(hindcast_data_set)
#saveRDS(hindcast_sample_5_from_each, file = "sample_5_from_each.rds")
#saveRDS(hindcast_sample_100_from_each, file="sample_100_from_each.rds")
saveRDS(hindcast_sample_all_cells_1_month_1_lead, file="sample_all_cells_1_month_1_lead")
saveRDS(hindcast_sample_all_cells_1_month_9_leads, file="sample_all_cells_1_month_9_leads")




