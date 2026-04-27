rm(list=ls())

library(dplyr)
library(boot)

#Whittles data pitcher data into something a lot more useful

#desktop 
#setwd("C:/Users/Sean's Desktop/Documents/GitHub/Math-447/ABS Project")

pitch_data <- read.csv("pitch_data_current.csv")
challenged <-  pitch_data$challenge

challenged_subset <- pitch_data %>% filter(challenge == "True")
