#Remove previous data
rm(list=ls())
par(mfrow = c(1,1))

#3. We now review k-fold cross-validation.
#(a) Explain how k-fold cross-validation is implemented.
#(b) What are the advantages and disadvantages of k-fold cross-validation
#relative to:
#  i. The validation set approach?
#  ii. LOOCV?
#  4. Suppose that we use some statistical learning method to make a prediction
#for the response Y for a particular value of the predictor X.
#Carefully describe how we might estimate the standard deviation of
#our prediction.

#3.
#a) For k-fold cross-validation, we divide the dataset into k amount of samples, where each
#sample size is n/k many data points.  We then use the same process as LOOCV, except we leave
#out and compare the subset instead of just one data point.
#b) i) The validation set approach will overestimate the test error rate, given that it only
#      uses half the data set to create it's model. In terms of bias, the k-fold will be less biased
#      with LOOCV being the least biased.
#   ii) The most clear advantage here is the number of computations it  takes to complete this
#      process. For LOOCV, the process is repeated n number of times, where with k-fold, it is
#      done k number of times. The book/notes(?) say that LOOCV is a version of k-fold where
#      k = n.
#4.
#a)k-fold:

#Problem 2

#Impact of social media on health data
#https://www.kaggle.com/datasets/sumeakash/impact-of-social-media-on-health
#Thanks to Sarah Loader for suggesting the data.

#Be sure to set the correct working directory.
#You may do so by doing Session -> Set Working Directory -> To Source File Location.
SocialMediaData <- read.csv("Social_media_impact_on_life.csv", header=TRUE)
head(SocialMediaData)
SocialMedia <- SocialMediaData[,c("Avg_Daily_Usage_Hours",
                                  "Sleep_Hours_Per_Night",
                                  "Mental_Health_Score")]
pairs(SocialMedia)

usage <- SocialMedia$Avg_Daily_Usage_Hours
sleep <- SocialMedia$Sleep_Hours_Per_Night
mental <- SocialMedia$Mental_Health_Score

plot(usage, sleep)
plot(sleep, sleep)
plot(usage, mental)

numobs <- nrow(SocialMedia)

#For Part (b), refer to the code below.
library(boot) #make sure to do install.packages("boot") beforehand.

#data_usage is for the models with usage as an explanatory variable.
data_usage = data.frame(sleep, usage)
for(i in 1:10)
{
  #The i-th order polynomial model
  fit.glm.usage <- glm(sleep~poly(usage, degree = i, raw = TRUE))
  #Computing the LOOCV error for the i-th order polynomial model
  print(cv.glm(data_usage, fit.glm.usage)$delta[1])
}

#c) 
#create data_mental in a similar way and repeat the analysis.
data_mental = data.frame(sleep, mental)
for(i in 1:10)
{
  #The i-th order polynomial model
  fit.glm.mental <- glm(sleep~poly(mental, degree = i, raw = TRUE))
  #Computing the LOOCV error for the i-th order polynomial model
  print(cv.glm(data_mental, fit.glm.mental)$delta[1])
}
#d)
predict(best_usage_fit, newdata=data.frame(usage = 0), data = data_usage)
predict(best_mental_fit, newdata=data.frame(mental = 0), data = data_mental)

#e)
data_usage_resample <- sample(data_usage, length(data_usage), replace = TRUE, prob = NULL)

#f)


#Problem 3

#2 (5.4.8) hints

#If the boot package is not installed yet, do install.packages("boot") first.
#The boot package contains special functions such as cv.glm.
library(boot)
set.seed(50, sample.kind="Rejection")
x <- rnorm(100, sd=1)
y <- 2*x-5*x^2+0.01*x^3+rnorm(100, sd=10)
#I think set.seed just creates some random preset for number generation? Not sure.
#In this case n=100, p=1?
#ask about this

#For (b). It would be a very good idea to specify "main", "xlab", and "ylab".
plot(x,y)
#There is not quite a linear trend, and it doesn't necessarily violate the constant
#variance assumption either. There is a certainly a fanning out of data points
#as the plot gets wider.

#For (c)
set.seed(50, sample.kind="Rejection") #For (d), use set.seed(4391, sample.kind="Rejection").
Data <- data.frame(x, y)

#For (c) and (d). poly(x, 2) is for the quadratic fit.
#Modify appropriately for the higher-order polynomials.
fit.glm.c.1 <- glm(y~x)
cv.glm(Data, fit.glm.1)$delta[1]
fit.glm.c.2 <- glm(y ~ poly(x, 2, raw = TRUE))
cv.glm(Data, fit.glm.2)$delta[1]
fit.glm.c.3 <- glm(y ~ poly(x, 3, raw = TRUE))
cv.glm(Data, fit.glm.3)$delta[1]
fit.glm.c.4 <- glm(y ~ poly(x, 4, raw = TRUE))
cv.glm(Data, fit.glm.4)$delta[1]

#part d)
set.seed(4391, sample.kind="Rejection")
fit.glm.d.1 <- glm(y~x)
cv.glm(Data, fit.glm.1)$delta[1]
fit.glm.d.2 <- glm(y ~ poly(x, 2, raw = TRUE))
cv.glm(Data, fit.glm.2)$delta[1]
fit.glm.d.3 <- glm(y ~ poly(x, 3, raw = TRUE))
cv.glm(Data, fit.glm.3)$delta[1]
fit.glm.d.4 <- glm(y ~ poly(x, 4, raw = TRUE))
cv.glm(Data, fit.glm.4)$delta[1]
#part e)
#The smallest error I had was in the 2nd degree polynomial.

#For (f), use summary(model) where model is the variable for your linear regression, 
#e.g., fit.glm.1.
