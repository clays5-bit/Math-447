library(dplyr)
library(fastDummies)
library(glmnet)
library(car)
library(FactoMineR)
library(factoextra)
rm(list=ls())

#setwd("C:/Users/Sean's Desktop/Documents/GitHub")
pitch_data_raw <- read.csv("pitch_data_current.csv")
pitch_data_raw <- pitch_data_raw %>% select(-X)

pitch_data_33 <- filter(pitch_data_raw, balls == 4, strikes == 2)
pitch_data_42 <- filter(pitch_data_raw, balls == 3, strikes == 3)

pitch_data_logistic<- pitch_data_raw %>% select(hasReview,
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

pitch_data_l_field<- pitch_data_raw %>% select(fielder_challenge,
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

pitch_data_l_batter<- pitch_data_raw %>% select(batter_challenge,
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

pitch_data_logistic <- filter(pitch_data_logistic, !(balls >= 5), !(strikes >= 4))
pitch_data_l_field <- filter(pitch_data_l_field, !(balls >= 5), !(strikes >= 4))
pitch_data_l_batter <- filter(pitch_data_l_batter, !(balls >= 5), !(strikes >= 4))

pitch_data_logistic$count <- relevel(factor(paste(pitch_data_logistic$balls, pitch_data_logistic$strikes, sep = '-')), ref = '1-1')
pitch_data_l_field$count <- relevel(factor(paste(pitch_data_l_field$balls, pitch_data_l_field$strikes, sep = '-')), ref = '1-1')
pitch_data_l_batter$count <- relevel(factor(paste(pitch_data_l_batter$balls, pitch_data_l_batter$strikes, sep = '-')), ref = '1-1')

pitch_data_logistic <- pitch_data_logistic %>% select(-balls, -strikes)
pitch_data_l_field <- pitch_data_l_field %>% select(-balls, -strikes)
pitch_data_l_batter <- pitch_data_l_batter %>% select(-balls, -strikes)

pitch_data_logistic %>% filter(count == "3-3") %>% count(hasReview)

pitch_data_logistic %>% filter(count == "4-2") %>% count(hasReview)

pitch_data_logistic$inning <- factor(pitch_data_logistic$inning)
pitch_data_l_field$inning <- factor(pitch_data_l_field$inning)
pitch_data_l_batter$inning <- factor(pitch_data_l_batter$inning)


fit.glm <- glm(hasReview ~ ., data = pitch_data_logistic, family = 'binomial')
fit.glm.f <- glm(fielder_challenge ~ ., data = pitch_data_l_field, family = 'binomial')
fit.glm.b <- glm(batter_challenge ~ ., data = pitch_data_l_batter, family = 'binomial')

summary(fit.glm)
summary(fit.glm.f)
summary(fit.glm.b)
vif(fit.glm)

count_table <- pitch_data_logistic %>%
  group_by(count) %>%
  summarise(
    review_rate = mean(hasReview),
    n = n()
  )

print(count_table, n = Inf)



#pitch_data_mca<- pitch_data_logistic %>% select(count,
#                                                inning,
#                                                runOn1,
#                                                runOn2,
#                                                runOn3,
#                                                pitchTypeCode
#                                                )
#pitch.mca <- MCA(pitch_data_mca, graph = TRUE)
#summary(pitch.mca)
#fviz_mca_var(pitch.mca, repel = TRUE, ggtheme = theme_minimal())
#fviz_contrib(pitch.mca, choice = 'var', axes=1)

#setwd("C:/Users/Sean's Desktop/Documents/GitHub/Math-447/ABS Project")