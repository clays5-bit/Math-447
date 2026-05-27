rm(list=ls())
#The code below is based on 10.4 Lab 1: Principal Component Analysis
#Taken from https://www.kaggle.com/datasets/vivovinco/2023-2024-nba-player-stats?select=2023-2024+NBA+Player+Stats+-+Playoffs.csv
#Thanks to Mason Hammons for suggesting the dataset.

#Problem 1. Use the code below. Also, note the following:

#X3P.: 3-point field goal percentage
#X2P.: 2-point field goal percentage
#DRB: Defensive rebounds per game
#AST: Assists per game
#STL: Steals per game
#BLK: Blocks per game

setwd("C:/Users/Sean's Desktop/Documents/GitHub/Math-447/S26HW6")
NBA_data <- read.csv("NBA.csv", header=TRUE)
NBA <- NBA_data[,c("X3P.", "X2P.", "DRB", "AST", "STL", "BLK")]

NBA_pos <- NBA_data$Pos #positions
#Notes on the positions
#PG: Point Guard
#SG: Shooting Guard
#SF: Small Forward
#PF: Point Forward
#C: Center 
#To understand the roles of these positions, see 
#https://www.underarmour.com/en-us/t/playbooks/basketball/basketball-positions/

#Principal components
pr.out <- prcomp(NBA, scale=TRUE)
names(pr.out)
dim(pr.out$x)

pr.out$rotation
biplot(pr.out, scale=0, xlim=c(-5,5), ylim=c(-5,5))
pr.out$x

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", 
     ylab="Proportion of Variance Explained", 
     ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", 
     ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1),type='b')

#Problem 2. Obtaining the principal component scores
NBA_scores <- pr.out$x
NBA_scores12 <- NBA_scores[,1:2]

hc.NBAcomplete = hclust(dist(NBA_scores12), method = "complete")
hc.NBAaverage = hclust(dist(NBA_scores12), method = "average")
hc.NBAsingle = hclust(dist(NBA_scores12), method = "single")

NBAcomplete_label <- cutree(hc.NBAcomplete, 5)
cutree(hc.NBAaverage, 2)
cutree(hc.NBAsingle, 2)

par(mfrow=c(1,3))
plot(hc.NBAcomplete, main = "Complete", cex = .9)
plot(hc.NBAaverage, main = "Average", cex=.9)
plot(hc.NBAsingle, main = "Single", cex=.9)

par(mfrow=c(1,1))
plot(NBA_scores12[,1], NBA_scores12[,2], xlab = 'PC1', ylab = 'PC2', col = NBAcomplete_label)
#Black - 1, Red - 2, Green - 3, Blue - 4, Teal - 5

table(cutree(hc.NBAcomplete, 5), NBA_pos)
