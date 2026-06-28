library(dplyr)
library(glmnet)
rm(list=ls())

#pitch_data_raw <- read.csv("pitch_data_current.csv")
pitch_data_raw <- read.csv("pitch_data_test.csv")
pitch_data_raw <- pitch_data_raw %>% select(-X)
pitch_data_raw <- filter(pitch_data_raw, !(pitchTypeCode == ""))

pitch_data_raw$isStrikeout <- as.logical(pitch_data_raw$isStrikeout)
pitch_data_raw$isWalk <- as.logical(pitch_data_raw$isWalk)

pitch_data_raw <- pitch_data_raw %>% 
  filter(tolower(description) 
         %in% (c("ball", 
                 "called strike"
                 )))

#get rid of pitches that have inaccurate data
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
                                               inning,
                                               runOn1,
                                               runOn2,
                                               runOn3,
                                               pitchTypeCode
)

pitch_data_l_batter<- pitch_data_raw %>% select(batter_challenge,
                                                count,
                                                outs,
                                                inning,
                                                runOn1,
                                                runOn2,
                                                runOn3,
                                                pitchTypeCode
)

pitch_data_hasReview = model.matrix(hasReview ~ . -1, data = pitch_data_logistic)
pitch_data_fielder = model.matrix(fielder_challenge ~ . -1, data = pitch_data_l_field)
pitch_data_batter = model.matrix(batter_challenge ~ . -1, data = pitch_data_l_batter)

#-------------------------------------------------------------------------------------------------------#
fit.glm <- glm(hasReview ~ ., data = pitch_data_logistic, family = 'binomial')
fit.glm.f <- glm(fielder_challenge ~ ., data = pitch_data_l_field, family = 'binomial')
fit.glm.b <- glm(batter_challenge ~ ., data = pitch_data_l_batter, family = 'binomial')

summary(fit.glm)
summary(fit.glm.f)
summary(fit.glm.b)
vif(fit.glm)
vif(fit.glm.f)
vif(fit.glm.b)

#----------------------------------------------------------------------------------------------------#

lasso_model_hasReview <- cv.glmnet(
  pitch_data_hasReview, 
  pitch_data_logistic$hasReview, 
  alpha = 1, 
  family = "binomial"
)

lasso_model_fielder <- cv.glmnet(
  pitch_data_fielder, 
  pitch_data_l_field$fielder_challenge, 
  alpha = 1, 
  family = "binomial"
)

lasso_model_batter <- cv.glmnet(
  pitch_data_batter, 
  pitch_data_l_batter$batter_challenge, 
  alpha = 1,
  family = "binomial"
)

coef(lasso_model_hasReview, s = lasso_model_hasReview$lambda.1se)
coef(lasso_model_fielder, s = lasso_model_fielder$lambda.1se)
coef(lasso_model_batter, s = lasso_model_batter$lambda.1se)

coef(lasso_model_hasReview, s = lasso_model_hasReview$lambda.min)
coef(lasso_model_fielder, s = lasso_model_fielder$lambda.min)
coef(lasso_model_batter, s = lasso_model_batter$lambda.min)

predict_hasReview <- predict(lasso_model_hasReview, s = lasso_model_hasReview$lambda.1se, newx = pitch_data_hasReview, type = 'response')
predict_hasReview <- predict(lasso_model_hasReview, s = lasso_model_hasReview$lambda.1se, newx = pitch_data_hasReview, type = 'response')
