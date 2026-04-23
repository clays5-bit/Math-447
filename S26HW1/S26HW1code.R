#---------------------------------------------------------------R PROBLEM--------------------------------------------------------------------------
#Remove previous data
rm(list=ls())
par(mfrow = c(1,1))

#Problem 2
#https://www.kaggle.com/datasets/farazrahman/earthquake
#Thanks to Lauryn Chapman for suggesting the dataset.

#NOTE: DO NOT present all your answers in the R script file only!
#Be sure to present everything in the PDF file as well.

#To know more about the variables, see:
#https://earthquake.usgs.gov/data/comcat/

#Loading data. "header=TRUE" means that the dataset has a header.
#Make sure to set the correct working directory before you load the dataset.
#The easiest way to do so is to Session -> Set Working Directory -> To Source File Location
#Alternatively, you may use setwd().
#setwd('S26HW1')
eq <- read.csv("all_month.csv", header=TRUE)

class(eq) #This should say "data.frame".

#(a) Keep only the following four columns, and sort them in the alphabetical order:
#mag, depth, dmin, rms.

#If you need to download, install, and use dplyr, uncomment the following lines:
#install.packages("dplyr")
library(dplyr)

#Variables (see https://earthquake.usgs.gov/data/comcat/)
#mag: The magnitude for the event
#depth: Depth of the event in kilometers
#dmin: Horizontal distance from the epicenter to the nearest station (in degrees). 
#1 degree is approximately 111.2 kilometers. 
#In general, the smaller this number, the more reliable is the calculated depth of the earthquake. 
#rms:The root-mean-square (RMS) travel time residual, in sec, using all weights. 
#This parameter provides a measure of the fit of the observed arrival times 
#to the predicted arrival times for this location. 
#Smaller numbers reflect a better fit of the data.

eq2 <- eq %>% select(mag, depth, dmin, rms)

#Some useful R functions

#dim() gives the dimension of the matrix/data.frame
dim(eq2)

#class() gives the object class of the variable
class(eq2)

#head() gives the first six rows of the data
head(eq2)

#(a) Which variable(s) seem to be associated with each other in eq2? 
#Use pairs() to create a matrix plot and cor() to compute the correlation matrix.
#Because there are missing observations in the data, 
#for cor(), set use="pairwise.complete.obs". 
#Otherwise, it will return NA whenever observation(s) are missing.
#Then, identify a pair of variables which seem to be (somewhat) 
#linearly correlated with each other, and justify whether 
#that linear correlation makes sense.
#You do not need to present the plot and correlation matrix in your answer.
pairs(eq2)
cor(eq2, use="pairwise.complete.obs")

#(b)
#If you want to predict the magnitude (mag), which predictor(s) would you use?
#Identify the top two predictors by examining cor(eq2) and pairs(eq2).
#You do not need to construct any multiple linear regression model.

#We are looking for predictors of mag with high correlation, so from best to  worst:
#rms, dmin, depth
#This is backed by the matrix plot, where the mag_rms relation looks the most linear.
#There is some fan out, which may raise constant variance concerns, but the
#mag-depth and mag-dmin plots certainly do not satisfy the constant variance assumption

#(c)
#Is it a good idea to include all the variables you identified in (b) 
#as predictors in the multiple linear regression to magnitude?
#Justify your answer by referring to cor(eq2) and pairs(eq2).

#For the lack of constant variance and the low correlation in part b, we may not
#want to include depth and dmin as predictors. It may be that rms is sufficient
#for our model in predicting earthquake magnitude.

#(d)
#Write R code and state the locations of the 12 earthquakes with the highest magnitudes.
#Then, compute the average depth for these 12 earthquakes.
twelvehigh = eq %>% slice_max(mag, n = 12) #from dplyr
print(twelvehigh[1:12, "place"])
twelvehigh_mean = mean(twelvehigh$depth)

#(e)
#Determine the approximate distribution of the square-root of the depth.
#Use hist() and identify the name of the distribution by recalling 
#the distributions you learned in MATH 342.
#In hist(), set prob = TRUE. By doing so, you will have density on the y-axis.
#Also, fit the probability density function of the distribution you chose. 
#To understand how it works, see below.
#https://r-charts.com/distribution/histogram-curves/
#Hint: The normal distribution does not fit well.

sqrt_depth <- sqrt(eq2$depth)
sqrt_depth <- sqrt_depth[!is.nan(sqrt_depth)]

x2 <- seq(min(sqrt_depth), max(sqrt_depth), length=40)
fun <- dchisq(x2, df=mean(sqrt_depth))
hist(sqrt_depth, prob = TRUE, breaks = 25)
lines(x2, fun, col=2, lwd=2)

#(f)
#Create a vector of 0's and 1's, where 1's are assigned to the 
#earthquakes whose magnitudes are at least 2. Otherwise, 0's are assigned. 
#Consider using as.integer() after obtaining the output with TRUE's and FALSE's.
#That way, you can covert them into 1's and 0's.
#In your answer, present the R code. 
#Do not use any loop statement such as a for- or a while-loop.

binary_mags <- as.integer(eq2$mag >= 2)
mag_2 <- sum(binary_mags, na.rm=TRUE)

#(g)
#Apply the table() function to the vector you created in (f) to obtain the 
#number of earthquakes whose magnitudes are at least 2.
#In your answer, present the R code and show the output.

table(binary_mags)

#(h)
#Which row has the highest rms value? Use which.max() to answer this question.
#Then, using which.min(), identify the row with the lowest rms value.
#In your answer, present the R code and show the output.

highest_rms <- which.max(eq2$rms)
lowest_rms <- which.min(eq2$rms)
print(highest_rms)
print(lowest_rms)

#(i)
#How many earthquakes had the dmin value of at most 0.05? 
#Use sum(eq2$dmin <= 0.05, na.rm=TRUE). 
#Then, explain why this code can be used to answer the question asked. 
#Note that na.rm=TRUE removes all the NAs. In other words, if you just do
#sum(eq2$dmin <= 0.05), it will return an NA.

num_of_dmin <- sum(eq2$dmin <= 0.05, na.rm=TRUE)
bool <- eq2$dmin <= .05

#eq$dmin <= .05 returns a boolean vector with a chain of True-Falses. na.rm removes all of the Nan
#entries of the vector. The sum function then considers all of the "True" entries as 1's and adds
#them all together.

#---------------------------------------------------------------PROBLEM 2.4.8--------------------------------------------------------------------------
par(mfrow = c(1,1))

#part a)
college <- read.csv("college.csv",header=TRUE)

#part b)
#fix(college)
rownames(college)=college[,1]
#fix(college)
college=college[-1]
fix(college)

college$Private <- as.factor(college$Private)

#ci)
summary(college)

#cii)
pairs(college[,1:10])

#ciii)
plot(Outstate ~ Private, data = college, main = "Private vs. Not Private Students From Out of State")

#civ)
Elite = rep("No", nrow(college))
Elite[college$Top10perc > 50]="Yes"
Elite = as.factor(Elite)
college = data.frame(college ,Elite)

summary(college)
plot(Outstate ~ Elite, data = college, main = "Elite vs Non-Elite Students From Out of State")

#cv)
par(mfrow = c(2,2))
hist(college$Grad.Rate, prob=TRUE, breaks = 25, main = "Graduation Rate")
hist(college$Top10perc, prob=FALSE, breaks = 20, main = "Proportion of top 10%")
hist(college$Terminal, prob=FALSE, breaks = 20, main = "Proportion of top 25%")

accept_app_rat <- (college$Accept)/(college$Apps)
hist(accept_app_rat, prob=FALSE, breaks = 20, main = "Acceptance/Applications")

#cvi)
#Which college had a graduation rate of over 100%?
wtf_college <- which.max(college$Grad.Rate)
rownames(college)[wtf_college]
#do colleges  that have a higher Top10% have a higher grad rate? Top25%?
par(mfrow = c(1,1))
top10_grad <- college[,c("Top10perc", "Grad.Rate")]
plot(top10_grad, main  = "Top10Proportion to Graduation Rate")
top25_grad <- college[,c("Top25perc", "Grad.Rate")]
plot(top25_grad, main = "Top25Proportion to Graduation Rate ")

predict_model_25 <- lm(college$Grad.Rate ~ college$Top25perc, data = college)
predict_model_10 <- lm(college$Grad.Rate ~ college$Top10perc, data = college)

print(predict_model_25)
print(predict_model_10)

summary(predict_model_25)
summary(predict_model_10)

