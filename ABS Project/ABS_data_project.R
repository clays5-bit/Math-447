rm(list=ls())

library(dplyr)
library(boot)

setwd("C:/Users/clays/Documents/GitHub/Math-447/ABS Project")

abs_data <- read.csv("abs-challenges-2026-team-summary.csv", header=TRUE)
standings_raw <- read.csv("mlb_current_standings.csv", header=TRUE)

standings <- standings_raw[1:30,]

standings <- rename(standings, team = Tm)
abs_data <- rename(abs_data, team = entity_name)

league_mash <- standings %>% inner_join(abs_data, by = "team")

team_data <- league_mash[, c("team", "parent_org", "W.L.")]
team_data["tot_challenges"] <- (league_mash$n_challenges_off + league_mash$n_challenges_def)
team_data["tot_successes"] <- (league_mash$n_overturns_off + league_mash$n_overturns_def)
team_data["success_rate"] <- team_data$tot_successes/team_data$tot_challenges

dev.new(width = 5,
        height = 4,
        unit = "in")

plot(team_data$success_rate, 
     team_data$W.L.,
     xlab = "Challenge Success Rate",
     ylab = "Win%",
     xlim = c(.0,1),
     ylim = c(.0,1))

text(team_data$success_rate, 
     team_data$W.L., 
     labels = team_data$parent_org, 
     pos = 4, 
     cex= .75, 
     offset = .25)

fit.challenge <- lm(W.L. ~ success_rate, team_data)
summary(fit.challenge)
