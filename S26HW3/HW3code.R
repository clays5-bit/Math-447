#The R command below clears the environment.
rm(list=ls())

#Before you run R, be sure to do the following: 
#Session -> Set Working Directory -> To Source File Location

#Thanks to Dominic Lisiecki for suggesting the dataset.
#https://www.kaggle.com/datasets/nikolasgegenava/cat-breeds
#Check this website for the response and explanatory variables.

#laptop working directory
#setwd("C:/Users/clays/Documents/GitHub/Math-447/S26HW3")
#desktop working directory

cat_dataset <- read.csv("cats.csv", header=TRUE)
catdata <- cat_dataset[-c(1,2,7:10)]

#Standardize each variable.
#Standardization is performed by using scale().
catdata2 <- data.frame(scale(as.matrix(catdata)))

library(glmnet)

# R code for Problems 3 and 4 using CATS data. Modify appropriately for your homework data.

#Problem 3
set.seed(5, sample.kind="Rejection") #a random seed. Use this for your Problem 3.

#a
#Random indices (row numbers) for the training data.
#This will create the test and training data by dividing the original data into two parts.
train = sample(1:dim(catdata2)[1], dim(catdata2)[1] / 2)
test <- -train
catdata2.train <- catdata2[train, ]
catdata2.test <- catdata2[test, ]

#b
#Fitting a multiple linear regression model
#avg_width is the response variable. 
#The dot (.) after tilde (~) means all the remaining variables.
fit.lm <- lm(avg_width ~ ., data = catdata2.train) 
pred.lm <- predict(fit.lm, catdata2.test)

#MSE calculation
mean((pred.lm - catdata2.test$avg_width)^2)

#c
train.mat <- model.matrix(avg_width ~ ., data = catdata2.train)[,-1]
test.mat <- model.matrix(avg_width ~ ., data = catdata2.test)[,-1]
#alpha = 0 corresponds to the ridge regression
fit.ridge <- glmnet(train.mat, catdata2.train$avg_width, alpha = 0)
cv.ridge <- cv.glmnet(train.mat, catdata2.train$avg_width, alpha = 0)
bestlam.ridge <- cv.ridge$lambda.1se #lambda.1se uses the "one standard error" criterion.
#Note: lambda.min uses the lambda which gives the smallest MSE for the test data.
bestlam.ridge

pred.ridge <- predict(fit.ridge, s = bestlam.ridge, newx = test.mat)
mean((pred.ridge - catdata2.test$avg_width)^2)

#d
#alpha = 1 corresponds to the lasso regression
fit.lasso <- glmnet(train.mat, catdata2.train$avg_width, alpha = 1)
cv.lasso <- cv.glmnet(train.mat, catdata2.train$avg_width, alpha = 1)
bestlam.lasso <- cv.lasso$lambda.1se #lambda.1se uses the "one standard error" criterion.
#Note: lambda.min uses the lambda which gives the smallest MSE for the test data.
bestlam.lasso

pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
mean((pred.lasso - catdata2.test$avg_width)^2)

#List of selected/dropped variables with their respective beta estimates
predict(fit.lasso, s = bestlam.lasso, type = "coefficients")

#Problem 4

#Number of times training sets are generated
B <- 1000

#Matrices and vector storing results
lasso01mat<-c()
lassobetamat<-c()
mse.lasso<-c()

for(b in 1:B)
{
  set.seed(b, sample.kind="Rejection")
  print(paste("Iteration #", b, sep=""))
  train = sample(1:dim(catdata2)[1], dim(catdata2)[1]/2)
  test <- -train
  catdata2.train <- catdata2[train, ]
  catdata2.test <- catdata2[test, ]
  
  train.mat <- model.matrix(avg_width ~ ., data = catdata2.train)[,-1]
  test.mat <- model.matrix(avg_width ~ ., data = catdata2.test)[,-1]
  
  #alpha = 1 corresponds to the lasso regression
  fit.lasso <- glmnet(train.mat, catdata2.train$avg_width, alpha = 1)
  cv.lasso <- cv.glmnet(train.mat, catdata2.train$avg_width, alpha = 1)
  bestlam.lasso <- cv.lasso$lambda.1se
  bestlam.lasso
  
  pred.lasso <- predict(fit.lasso, s = bestlam.lasso, newx = test.mat)
  mse.lasso[b] <- mean((pred.lasso - catdata2.test$avg_width)^2)
  
  #List of selected/dropped variables with their respective beta estimates
  beta.lasso <- predict(fit.lasso, s = bestlam.lasso, type = "coefficients")
  lassobetamat <- rbind(lassobetamat, beta.lasso[-1,1])
  
  #Convert the variables to 0 (not selected) or 1 (selected)
  lasso01 <- as.integer(abs(beta.lasso[-1,1]) > 0)
  lasso01mat <- rbind(lasso01mat, lasso01)
}
colnames(lasso01mat) <- colnames(catdata2)[-1]
colnames(lassobetamat) <- colnames(catdata2)[-1]

#Showing the probability of selecting each predictor for the Lasso
colMeans(lasso01mat)

#Showing the mean of the beta coefficients for each predictor for the Lasso
colMeans(lassobetamat)

#Mean of the MSE values
mean(mse.lasso)