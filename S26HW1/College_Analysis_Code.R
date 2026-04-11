rm(list=ls())
setwd('S26HW1')
college <- read.csv("college.csv",header=TRUE)

#preliminary problems a & b
#addition of row names and removal of college names as a data column
#fix(college)
rownames(college)=college[,1]
#fix(college)
college=college[-1]
fix(college)

#turning Private college into yes/no category
college$Private <- as.factor(college$Private)

#ci)
summary(college)

#cii) LOOK AT MORE CLOSELY
pairs(college[,1:10])

#ciii) box plots of private vs. public students from out of state
plot(Outstate ~ Private, data = college)

#civ) creation of elite college category
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50]="Yes"
Elite = as.factor(Elite)
college = data.frame(college ,Elite)

summary(college)
plot(Outstate ~ Elite, data = college)

#cv) Varied Histograms
#The second and third histograms are both broken into 20 breaks
#because they are percentage based. Easier for readability.
par(mfrow = c(2,2))
hist(college$Grad.Rate, prob=TRUE, breaks = 25)
hist(college$Top10perc, prob=TRUE, breaks = 20)
hist(college$Terminal, prob=TRUE, breaks = 20)

#cvi)Proportion of accepted to applied.
#Could turn into proportion of rejected?
accept_app_rat <- (college$Accept)/(college$Apps)
hist(accept_app_rat, prob=TRUE, breaks = 20)

wtf_college <- which.max(college$Grad.Rate)
rownames(college)[wtf_college]
#do colleges  that have a higher Top10% have a higher grad rate? Top25%?
#Do higher expense colleges predict more students graduating?
#Do "elite" schools have higher expense?
