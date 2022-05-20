### ML.R
# Tom Gause
# 4/11/2022

# LSTM experiment inspired by:
# https://www.r-bloggers.com/2018/04/time-series-deep-learning-forecasting-sunspots-with-keras-stateful-lstm-in-r/

## Default repo
local({r <- getOption("repos")
r["CRAN"] <- "http://cran.r-project.org"
options(repos=r)
})

# Install any packages that are missing
packages <- c("tidyverse", "glue", "forcats", "timetk", "tidyquant",
              "tibbletime", "cowplot", "ggplot2", "viridis")
install.packages(setdiff(packages, rownames(installed.packages()))) 

# Core Tidyverse
library(tidyverse)
library(glue)
library(forcats)
# Time Series
library(timetk)
library(tidyquant)
library(tibbletime)
# Visualization
library(cowplot)
# Plotting
library(ggplot2)
library(viridis)

mean.errors <- data.frame(MSE = c(0.0433, 0.0443),
                          method = c("RNN", "QM"))


#remove 0 row
mean.errors <- mean.errors %>%
  mutate(bias.correction.method = rownames(mean.errors))
colnames(mean.errors)[1] = "MSE"
# First, we'll make a simple bar chart for model performance

mean.errors %>%
  ggplot(mapping = aes(y = MSE, x = method, 
                       label = sprintf("%0.2f", MSE))) +
  geom_bar(stat = 'identity', aes(fill = bias.correction.method))+
  xlab("Bias Correction Method") + 
  geom_text(aes(label=MSE), vjust=-0.3, size=5) +
  theme_minimal()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(size = 15), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15)) + 
  ggtitle("MSE for Bias Correction Methods, \n Tested on 2014-2021 Data")

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
  theme(plot.title = element_text(hjust = 0.5, size = 18), 
        axis.text = element_text(size = 15), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15))





