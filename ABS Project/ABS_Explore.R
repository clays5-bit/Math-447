library(dplyr)
library(fastDummies)
library(glmnet)
library(car)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(caret)

batter_counts <- pitch_data_l_batter %>%
  filter(batter_challenge == 1) %>%
  group_by(count) %>%
  summarise(n = n()) %>%
  mutate(challenger = "Batter")

fielder_counts <- pitch_data_l_field %>%
  filter(fielder_challenge == 1) %>%
  group_by(count) %>%
  summarise(n = n()) %>%
  mutate(challenger = "Fielder")

# Combine
challenge_counts <- bind_rows(batter_counts, fielder_counts)

#Plot
ggplot(challenge_counts, aes(x = count, y = n, fill = challenger)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Challenges by Count and Challenger Side",
       x = "Count",
       y = "Number of Challenges",
       fill = "Challenger") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pitch_data_l_field %>%
#  filter(fielder_challenge == 1, strikes == 3, strikeout == TRUE) %>%
# select(date, batting_team, pitching_team, balls, strikes, description, isOverturned) %>%
#  head(20)

#pitch_data_challenges <- filter(pitch_data_raw, batting_team == "Kansas City Royals")
#pitch_data_challenges <- filter(pitch_data_challenges, date == "2026-05-05")
#pitch_data_challenges <- filter(pitch_data_challenges, hasReview == "True")

