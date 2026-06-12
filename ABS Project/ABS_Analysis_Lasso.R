library(dplyr)
library(glmnet)

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
coef(lasso_model_batter, s = lasso_model_fielder$lambda.min)

probabilities_hasReview <- predict(lasso_model_hasReview, newx = pitch_data_hasReview, s = lasso_model_hasReview$lambda.1se, type = "response")
probabilities_fielder <- predict(lasso_model_fielder, newx = pitch_data_fielder, s = lasso_model_fielder$lambda.1se, type = "response")
probabilities_batter <- predict(lasso_model_batter, newx = pitch_data_batter, s = lasso_model_batter$lambda.1se, type = "response")