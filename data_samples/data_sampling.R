library(tidyverse)



df <- list.files(path = "/Users/tomgause/Desktop/iScience_data/hindcasts_usa/",
                      pattern = ".rds",
                      full.names = TRUE)

sample_size <- 6
N <- length(df)
hindcast_data_set <- NULL
index <- NULL
temp <- NULL

for(i in 1:3) {
  print(i)
  temp <- readRDS(df[i]) %>%
    select("target_month", "x", "y", "target_month", "lead", "fcst_tmp_k", "obs_tmp_k")

  index <- sample(nrow(temp), 6, replace=FALSE)
  print("GOT HERE")
  hindcast_data_set <- rbind(hindcast_data_set, temp[index,])
}
view(hindcast_data_set)
saveRDS(hindcast_data_set, file = "hindcast_sample_5_from_each.rds")