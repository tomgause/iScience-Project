### results_visualizations
# Acadia Hegedus
# 5/16/22

library(ggplot2)
library(tidyverse)
library(viridis)

#read in results 
#cell errors for rf, qm, base, climate.norm
cell.error1 <- readRDS("./data/cell_errors_test_results_2022-05-12_14-27-13.RDS")
#cell errors for cnn
cell.error2 <- readRDS("./data/cell_errors_cnn_2022-05-13.RDS")
#cell errors for rfs single pixel
cell.error3 <- readRDS("./data/cell_errors_rf_single_test_results_2022-05-16_10-53-38.RDS")

#read in test data to get correct fcst_cells for Vermont
test <- readRDS("./data/test_subset_2022-04-18_10-27-26.RDS")

# Select and store all forecast cells
sample.cells <- test %>%
  dplyr::select(fcst_cell,x,y) %>%
  unique()

#filter cnn results for only pixels in Vermont
cell.error2 <- left_join(cell.error2,sample.cells, by = c("x","y"))
cell.error2 <- na.omit(cell.error2)
cell.error2 <- cell.error2[,1:3]

cell.error <- left_join(cell.error1, cell.error2, by = c("x","y"))
colnames(cell.error)[7] <- "CNN"
cell.error <- subset(cell.error, select = -`Climate Norm`)
cell.error <- cell.error%>%
  mutate


mean.errors <- data.frame(colMeans(cell.error[,c("Quantile Matching","Base",
                                                 "RF All Pixels", "CNN")]))
mean.errors <- mean.errors %>%
  mutate(bias.correction.method = rownames(mean.errors))
colnames(mean.errors)[1] = "MSE"
mean.errors$MSE <- round(mean.errors$MSE,3)

#generate plot of rf vs. quantile matching vs base
mean.errors %>%
  filter(bias.correction.method != "CNN")%>%
  ggplot(mapping = aes(x = bias.correction.method, y = MSE, 
                       label = sprintf("%0.2f", round(MSE, digits = 3)),
                       fill = bias.correction.method))+
  geom_bar(stat = 'identity')+
  labs(fill = "Bias Correction Method")+
  geom_text(size = 6)+
  xlab("Bias Correction Method")+
  theme_test()+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2014-2021 Data")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text = element_text(size = 15), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))


#generate plot of all mean errors
mean.errors %>%
  ggplot(mapping = aes(x = bias.correction.method, y = MSE, 
                       label = sprintf("%0.2f", round(MSE, digits = 3)),
                       fill = bias.correction.method))+
  geom_bar(stat = 'identity')+
  labs(fill = "Bias Correction Method")+
  geom_text(size = 6)+
  xlab("Bias Correction Method")+
  theme_test()+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2014-2021 Data")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), axis.text = element_text(size = 15), axis.title.x = element_text(size = 15), axis.title.y = element_text(size = 15))

#create plot of MSE for Vermont
cell.error.pivoted <- cell.error%>%
  pivot_longer(cols = c("Quantile Matching","Base","RF All Pixels","CNN"),
               names_to = c("bias_correction_technique"),
               values_to = c("MSE"))

cell.error.pivoted%>%
  ggplot(mapping = aes(x = x, y = y, color = MSE))+
  geom_point(size = 6)+
  theme_test()+
  ggtitle("MSE for Various Bias Correction Methods, \n Tested on 2014-2021 Data")+
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size = 15), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15))+
  scale_color_viridis()+
  facet_wrap(~bias_correction_technique)
