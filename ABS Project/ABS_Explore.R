library(dplyr)
library(fastDummies)
library(glmnet)
library(car)
library(FactoMineR)
library(factoextra)
library(ggplot2)
library(caret)

#batter_counts <- pitch_data_l_batter %>%
#  filter(batter_challenge == 1) %>%
#  group_by(count) %>%
#  summarise(n = n()) %>%
#  mutate(challenger = "Batter")

#fielder_counts <- pitch_data_l_field %>%
#  filter(fielder_challenge == 1) %>%
#  group_by(count) %>%
#  summarise(n = n()) %>%
#  mutate(challenger = "Fielder")

# Combine
#challenge_counts <- bind_rows(batter_counts, fielder_counts)

# Plot
#ggplot(challenge_counts, aes(x = count, y = n, fill = challenger)) +
#  geom_bar(stat = "identity", position = "dodge") +
#  labs(title = "Challenges by Count and Challenger Side",
#       x = "Count",
#       y = "Number of Challenges",
#       fill = "Challenger") +
#  theme_minimal() +
#  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#pitch_data_l_field %>%
#  filter(fielder_challenge == 1, strikes == 3, strikeout == TRUE) %>%
#  select(date, batting_team, pitching_team, balls, strikes, description, isOverturned) %>%
#  head(20)

pitch_data_challenges <- filter(pitch_data_raw, batting_team == "Minnesota Twins")
pitch_data_challenges <- filter(pitch_data_challenges, date == "2026-05-07")
pitch_data_challenges <- filter(pitch_data_challenges, hasReview == TRUE)

ump_review <- pitch_data_raw[, c("umpire", "umpire_call_strike", "isStrike", "hasReview", "isOverturned", "pX", "pZ")] %>% filter(hasReview == TRUE)
umpreviewstrike <- ump_review %>% filter(umpire_call_strike == "True") %>% filter(isOverturned == "False")
umpreviewSoverturned <- ump_review %>% filter(umpire_call_strike == "True") %>% filter(isOverturned == "True")


average_strikeTop <- (sum(pitch_data_raw$strikeZoneTop)/length(pitch_data_raw$strikeZoneTop))
average_strikeBottom <- (sum(pitch_data_raw$strikeZoneBottom)/length(pitch_data_raw$strikeZoneBottom))
strikeWidth = (17/12) #always 17in
#batter info 
batters <- pitch_data_raw[, c("batting_team", "batter_name", "strikeZoneTop", "strikeZoneBottom")]
battersdist <- batters %>% distinct(batter_name, .keep_all = TRUE)

#plotting box on scatter plot
batterbox_corners <-data.frame(
  y = c(average_strikeBottom, average_strikeTop, average_strikeTop, average_strikeBottom),
  x = c(-strikeWidth/2, -strikeWidth/2, strikeWidth/2, strikeWidth/2))

umpreviewS <- data.frame(
  URS_X = c(umpreviewstrike$pX),
  URS_Y = c(umpreviewstrike$pZ)
)

#KNN
train_dataURS <- bind_rows(
  umpreviewstrike %>% mutate(Call = 1),
  umpreviewSoverturned %>% mutate(Call = 0)
) %>% 
  select(pX, pZ, Call) %>% 
  na.omit()

knn_model <- knn3(Call ~ pX + pZ, data = train_dataURS, k = 5)

grid <- expand.grid(
  pX = seq(-2, 2, length.out = 100),
  pZ = seq(0, 5, length.out = 100)
)

preds <- as.data.frame(predict(knn_model, newdata = grid))
grid$ProbCall <- preds[, 2]

ggplot() +
  # KNN Probability Contour Layer
  geom_contour_filled(data = grid, aes(x = pX, y = pZ, z = ProbCall), 
                      alpha = 0.5, bins = 5) +
  # Original Strike Scatter Points
  geom_point(data = train_dataURS, aes(x = pX, y = pZ, color = as.factor(Call)), 
             size = 0.5, alpha = 0.8) +
  # Batter box / Strike Zone Bounding Box
  geom_polygon(data = batterbox_corners, aes(x = x, y = y), 
               fill = NA, color = "black", linewidth = 1, alpha = 0.8) +
  # Scales and Labels
  scale_color_manual(values = c("0" = "darkorange", "1" = "red"), 
                     labels = c("Strike Overturned", "Strike"), name = "Pitch Call") +
  scale_fill_brewer(palette = "Blues", name = "Strike Overturned") +
  coord_fixed() +
  theme_minimal() +
  labs(
    title = "KNN Probability of Strike not Overturned",
    subtitle = "Decision Boundary and Call Probability",
    x = "Horizontal Position (pX)",
    y = "Vertical Position (pZ)"
  )