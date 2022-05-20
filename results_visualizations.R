### results_visualizations
# Acadia Hegedus
# 5/20/22

library(ggplot2)
library(tidyverse)
library(viridis)

#read in results 
#cell errors for rf, qm, base, climate.norm
cell.error1 <- readRDS("./data/FINAL_cell_errors_test_results_2022-05-20_11-26-56.RDS")
#cell errors for cnn
cell.error.cnn <- readRDS("./data/cnn_mse.RDS")

cell.error2 <- cell.error.cnn%>%
  group_by(x,y)%>%
  summarize(CNN = mean(row_mse))

cell.error <- left_join(cell.error1, cell.error2, by = c("x","y"))
colnames(cell.error) <- c("x","y","Quantile Matching","Climate Norm","Base","RF","CNN")

#create plot of MSE for Vermont
cell.error.pivoted <- cell.error%>%
  pivot_longer(cols = c("Quantile Matching","Climate Norm","Base","RF","CNN"),
               names_to = c("bias_correction_technique"),
               values_to = c("MSE"))

cell.error.pivoted%>%
  filter(bias_correction_technique != 'Base')%>%
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

#read in mean errors for rf, qm, base, climate.norm
mean.errors1 <- readRDS("./data/FINAL_mean_errors_test_results_2022-05-20_11-26-56.RDS")
mean.errors <- mean.errors1%>%
  mutate(cnn.mse = mean(cell.error.cnn$row_mse)) %>% 
  pivot_longer(cols = c(qm.mse,climate.mse,rf.mse,base.mse,cnn.mse),
               names_to = "bias.correction.method",
               values_to = "MSE")

mean.errors$bias.correction.method<- c("Quantile Matching","Climate Norm",
                                       "RF","Base","CNN")


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


