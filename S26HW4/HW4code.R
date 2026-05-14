#Remove previous data
rm(list=ls())
#setwd("C:/Users/Sean's Desktop/Documents/GitHub/Math-447/S26HW4")

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
#weight_kg: weight_kgin kilograms
#height_m: Height in meters
#log_len_wt: Log(Length/Weight)
#log_len_ht: Log(Length/Height)
#Response
#carnivore: 1 if carnivore, 0 if herbivore

#standardizing each predictor variable
dino_data2_x <- data.frame(scale(as.matrix(dino_data2[,-6])))
dino_data_sd <- cbind(dino_data2[,6], dino_data2_x)
colnames(dino_data_sd) <- columns[c(6,1:5)]

#For the homework, run the following. 
#If you do not have MASS or class, install them first using install.packages().
library(MASS)
library(class)

#For the example dataset dino_data_sd, the following is necessary.
library(ISLR)

#Note: The following line is necessary only for the dino_data_sd data.
attach(dino_data_sd)

#R tip: For large data, use head(dino_data_sd). 
#This displays the header and the first six rows of the data.

#b
#Create a boxplot for each predictor against the response. 
#In the example below, length_m is a predictor and carnivore is the response.
#note that outline=FALSE is used to remove any outliers from the display.
boxplot(length_m ~ carnivore, data = dino_data_sd, main = "length_m vs carnivore", outline=FALSE)
boxplot(weight_kg ~ carnivore, data = dino_data_sd, main = "weight_kg vs carnivore", outline=FALSE)
boxplot(height_m ~ carnivore, data = dino_data_sd, main = "height_m vs carnivore", outline=FALSE)
boxplot(log_len_wt ~ carnivore, data = dino_data_sd, main = "log_len_wt vs carnivore", outline=FALSE)
boxplot(log_len_ht ~ carnivore, data = dino_data_sd, main = "log_len_ht vs carnivore", outline=FALSE)


#c
set.seed(11, sample.kind="Rejection") #Use this seed for your homework
train <- sample(1:dim(dino_data_sd)[1], dim(dino_data_sd)[1]/2)
test <- -train
dino_data_sd.train <- dino_data_sd[train, ]
dino_data_sd.test <- dino_data_sd[test, ]
carnivore.test <- carnivore[test]

#d
fit.lda <- lda(carnivore ~ length_m + weight_kg + height_m + log_len_wt + log_len_ht, data = dino_data_sd, subset = train)
fit.lda

pred.lda <- predict(fit.lda, dino_data_sd.test)
#Confusion matrix
table(pred.lda$class, carnivore.test)
#Test error rates
mean(pred.lda$class != carnivore.test)
#what is test error of model?
#Discuss if predicted predictors were accurate

#e
#Simply use all the predictor variables for your homework question.
fit.qda <- qda(carnivore ~ length_m + weight_kg+ height_m + log_len_wt + log_len_ht, data = dino_data_sd, subset = train)
fit.qda

pred.qda <- predict(fit.qda, dino_data_sd.test)
table(pred.qda$class, carnivore.test)
mean(pred.qda$class != carnivore.test)

#f
#Simply use all the predictor variables for your homework question.
fit.glm <- glm(carnivore ~ length_m + weight_kg+ height_m + log_len_wt + log_len_ht, 
               data = dino_data_sd, family = binomial, subset = train)
fit.glm
summary(fit.glm)

probs <- predict(fit.glm, dino_data_sd.test, type = "response")
pred.glm <- rep(0, length(probs))
pred.glm[probs > 0.5] <- 1
table(pred.glm, carnivore.test)
mean(pred.glm != carnivore.test)

#g
#For this homework, create a plot of the misclassification error rate vs. K 
#for K = 1, 2, 3, ..., 10.
#Based on the plot, choose the 'best' K.
#Optionally, compute sensitivity and specificity for different values of K (not coded).
train.X <- cbind(length_m, weight_kg, height_m, log_len_wt, log_len_ht)[train, ]
test.X <- cbind(length_m, weight_kg, height_m, log_len_wt, log_len_ht)[test, ]
train.carnivore <- carnivore[train]


error <- 1:10

for (i in 1:10){
  pred.knn <- knn(train.X, test.X, train.carnivore, k = i)
  error[i] <- mean(pred.knn != carnivore.test) #Test error (misclassification rate)
}

table(pred.knn, carnivore.test) #Confusion matrix
kvals <- 1:10
plot(1:10, error, xlab = "K", ylab="Error Rate")