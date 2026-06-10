library(dplyr)
library(fastDummies)
library(glmnet)
library(car)
library(FactoMineR)
library(factoextra)

#----------------------------------------------------------------------------------------------------#

pitch_data_hasReview = model.matrix(hasReview ~ . -1, data = pitch_data_logistic)
pitch_data_fielder = model.matrix(fielder_challenge ~ . -1, data = pitch_data_l_field)
pitch_data_batter = model.matrix(batter_challenge ~ . -1, data = pitch_data_l_batter)

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

coef(lasso_model_hasReview, s = "lambda.1se")
coef(lasso_model_fielder, s = "lambda.1se")
coef(lasso_model_batter, s = "lambda.1se")

probabilities_hasReview <- predict(lasso_model_hasReview, newx = pitch_data_hasReview, s = "lambda.1se", type = "response")
probabilities_fielder <- predict(lasso_model_fielder, newx = pitch_data_fielder, s = "lambda.1se", type = "response")
probabilities_batter <- predict(lasso_model_batter, newx = pitch_data_batter, s = "lambda.1se", type = "response")

predicted_hasReview <- ifelse(probabilities_hasReview >= .07, 1, 0)
predicted_fielder <- ifelse(probabilities_fielder >= .07, 1, 0)
predicted_batter <- ifelse(probabilities_batter >= .07, 1, 0)

