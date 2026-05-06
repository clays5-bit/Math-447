#Remove previous data
rm(list=ls())

#Thanks to Nolan Mickenham for suggesting the Dinosaur Genera Dataset.
#https://www.kaggle.com/datasets/canozensoy/dinosaur-genera-dataset

#Be sure to do Session -> Set Working Directory -> To Source File Location
dino_data <- read.csv("dinoDatasetCSV.csv", header=TRUE)
dino_data2 <- dino_data[which(dino_data$diet %in% c("Carnivore","Herbivore")),c(4,5,6,7)]

dino_data2$log_len_wt <- log(dino_data2$length_m/dino_data2$weight_kg)
dino_data2$log_len_ht <- log(dino_data2$length_m/dino_data2$height_m)

dino_data2$carnivore <- as.integer(dino_data2$diet == "Carnivore")

rowNAs <- which(is.na(rowSums(dino_data2[,-1])))
dino_data2 <- dino_data2[-rowNAs,-1] #removing carnivore vs. herbivore
columns <- colnames(dino_data2)

#Glossary
#Predictors
#length_m: Length in meters
#weight_kg: Weight in kilograms
#height_m: Height in meters
#log_len_wt: Log(Length/Weight)
#log_len_ht: Log(Length/Height)
#Response
#carnivore: 1 if carnivore, 0 if herbivore

#standardizing each predictor variable
dino_data2_x <- data.frame(scale(as.matrix(dino_data2[,-6])))
dino_data_sd <- cbind(dino_data2[,6], dino_data2_x)
colnames(dino_data_sd) <- columns[c(6,1:5)]

attach(dino_data_sd)

#For the homework, run the following. 
#If you do not have MASS or class, install them first using install.packages().
library(MASS)
library(class)

#For the example dataset Auto, the following is necessary.
library(ISLR)

#Note: The following line is necessary only for the Auto data.
attach(Auto)

#R tip: For large data, use head(Auto). 
#This displays the header and the first six rows of the data.

#a
#Creating a binary variable for the example data.
mpg01 <- as.numeric(mpg > median(mpg))
Auto <- data.frame(Auto, mpg01) #This adds mpg01 as a new column.

#b
#Create a boxplot for each predictor against the response. 
#In the example below, cylinders is a predictor and mpg01 is the response.
#note that outline=FALSE is used to remove any outliers from the display.
boxplot(cylinders ~ mpg01, data = Auto, main = "Cylinders vs mpg01", outline=FALSE)

#c
set.seed(11, sample.kind="Rejection") #Use this seed for your homework
train <- sample(1:dim(Auto)[1], dim(Auto)[1]/2)
test <- -train
Auto.train <- Auto[train, ]
Auto.test <- Auto[test, ]
mpg01.test <- mpg01[test]

#d
fit.lda <- lda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.lda

pred.lda <- predict(fit.lda, Auto.test)
#Confusion matrix
table(pred.lda$class, mpg01.test)
#Test error rates
mean(pred.lda$class != mpg01.test)

#e
#Simply use all the predictor variables for your homework question.
fit.qda <- qda(mpg01 ~ cylinders + weight + displacement + horsepower, data = Auto, subset = train)
fit.qda

pred.qda <- predict(fit.qda, Auto.test)
table(pred.qda$class, mpg01.test)
mean(pred.qda$class != mpg01.test)

#f
#Simply use all the predictor variables for your homework question.
fit.glm <- glm(mpg01 ~ cylinders + weight + displacement + horsepower, 
               data = Auto, family = binomial, subset = train)
summary(fit.glm)

probs <- predict(fit.glm, Auto.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, mpg01.test)
mean(pred.glm != mpg01.test)

#g
#For this homework, create a plot of the misclassification error rate vs. K 
#for K = 1, 2, 3, ..., 10.
#Based on the plot, choose the 'best' K.
#Optionally, compute sensitivity and specificity for different values of K (not coded).
train.X <- cbind(cylinders, weight, displacement, horsepower)[train, ]
test.X <- cbind(cylinders, weight, displacement, horsepower)[test, ]
train.mpg01 <- mpg01[train]
pred.knn <- knn(train.X, test.X, train.mpg01, k = 1) #K = 1
table(pred.knn, mpg01.test) #Confusion matrix
mean(pred.knn != mpg01.test) #Test error (misclassification rate)

