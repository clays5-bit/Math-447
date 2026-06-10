library(dplyr)
library(fastDummies)
library(glmnet)
library(car)
library(FactoMineR)
library(factoextra)
rm(list=ls())

setwd("C:/Users/Sean's Desktop/Documents/GitHub")
#setwd("C:/Users/clays/Documents/GitHub")
pitch_data_raw <- read.csv("pitch_data_current.csv")
pitch_data_raw <- pitch_data_raw %>% select(-X)
pitch_data_raw <- filter(pitch_data_raw, !(pitchTypeCode == ""))

pitch_data_raw$isStrikeout <- as.logical(pitch_data_raw$isStrikeout)
pitch_data_raw$isWalk <- as.logical(pitch_data_raw$isWalk)

pitch_data_raw <- pitch_data_raw %>% 
  filter(tolower(description) 
         %in% (c("ball", 
                 "called strike"
                 )))
pitch_data_raw <- filter(pitch_data_raw, !(balls >= 5), !(strikes >= 4))

pitch_data_raw$hasReview <- as.logical(pitch_data_raw$hasReview)
pitch_data_raw$fielder_challenge <- as.logical(pitch_data_raw$fielder_challenge)
pitch_data_raw$batter_challenge <- as.logical(pitch_data_raw$batter_challenge)

pitch_data_raw$count <- relevel(factor(paste(pitch_data_raw$balls, pitch_data_raw$strikes, sep = '-')), ref = '1-1')
pitch_data_raw$inning <- factor(pitch_data_raw$inning)
pitch_data_raw$pitchTypeCode <- relevel(factor(pitch_data_raw$pitchTypeCode), ref = 'FF')

pitch_data_raw$batter_challenge[pitch_data_raw$hasReview == FALSE & 
                                pitch_data_raw$batter_challenge == TRUE] <- FALSE
pitch_data_raw$fielder_challenge[pitch_data_raw$hasReview == FALSE & 
                                 pitch_data_raw$fielder_challenge == TRUE] <- FALSE

pitch_data_logistic<- pitch_data_raw %>% select(hasReview,
                                                count,
                                                outs,
                                                score_diff,
                                                inning,
                                                runOn1,
                                                runOn2,
                                                runOn3,
                                                #pX,
                                                #pZ,
                                                pitchTypeCode
)

pitch_data_l_field<- pitch_data_raw %>% select(fielder_challenge,
                                               count,
                                               outs,
                                               score_diff,
                                               inning,
                                               runOn1,
                                               runOn2,
                                               runOn3,
                                               pitchTypeCode
)

pitch_data_l_batter<- pitch_data_raw %>% select(batter_challenge,
                                                count,
                                                outs,
                                                score_diff,
                                                inning,
                                                runOn1,
                                                runOn2,
                                                runOn3,
                                                pitchTypeCode
)

pitch_data_hasReview = model.matrix(hasReview ~ . -1, data = pitch_data_logistic)
pitch_data_fielder = model.matrix(fielder_challenge ~ . -1, data = pitch_data_l_field)
pitch_data_batter = model.matrix(batter_challenge ~ . -1, data = pitch_data_l_batter)