library(dplyr)
library(glmnet)

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