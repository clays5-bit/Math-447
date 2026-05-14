library(dplyr)
library(fastDummies)
library(glmnet)
library(car)
rm(list=ls())

setwd("C:/Users/Sean's Desktop/Documents/GitHub")
pitch_data <- read.csv("pitch_data_current.csv")
pitch_data <- pitch_data %>% select(-X)

pitch_data_logistic<- pitch_data %>% select(hasReview,
                                            balls,
                                            strikes,
                                            outs,
                                            score_diff,
                                            inning,
                                            topInning,
                                            runOn1,
                                            runOn2,
                                            runOn3,
                                            pitchTypeCode,
                                            breakHorizontal,
                                            spinRate,
                                            spinDirection,
                                            plateTime
                                            )

fit.glm <- glm(hasReview ~ ., data = pitch_data_logistic, family = 'binomial')

summary(fit.glm)
vif(fit.glm)

#setwd("C:/Users/Sean's Desktop/Documents/GitHub/Math-447/ABS Project")