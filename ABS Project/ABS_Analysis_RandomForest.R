library(dplyr)
library(glmnet)
library(randomForest)

random_forest_hasReview <- randomForest(as.factor(hasReview) ~ ., data = pitch_data_logistic, ntree = 500, importance = TRUE)
random_forest_fielderReview <- randomForest(as.factor(fielder_challenge) ~ ., data = pitch_data_l_field, ntree = 500, importance = TRUE)
random_forest_batterReview <- randomForest(as.factor(batter_challenge) ~ ., data = pitch_data_l_batter, ntree = 500, importance = TRUE)

varImpPlot(random_forest_hasReview)
varImpPlot(random_forest_fielderReview)
varImpPlot(random_forest_batterReview)

