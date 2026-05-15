rm(list=ls())
#The code below is based on 10.4 Lab 1: Principal Component Analysis
#The data are available at https://www.kaggle.com/datasets/rifatalam3/gold-price-trends/data
#Thanks to Divdish Kaur for suggesting the dataset.

#Problem 1. Use the code below. Note that the last 1000 observations are used.
gold_data_original <-read.csv("finalgolddata.csv", header=TRUE)
gold_data <- gold_data_original[c((nrow(gold_data_original)-999):
                                    nrow(gold_data_original)),]

gold <- gold_data[,c("Open", "High", "Low", "Close", "Volume")]

pr.out=prcomp(gold, scale=TRUE)
names(pr.out)
dim(pr.out$x)

apply(gold, 2, mean)
apply(gold, 2, var)

#Loading vector
pr.out$rotation

#Scores
pr.out$x

#The numbers in the biplot below represent the observation number.
#For more details, see "observations" above.
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", ylim=c(0,1),type='b')

#Problem 2. PCA and K-means clustering. Modify the code below appropriately.

#The code below is based on 10.4 Lab 1: Principal Component Analysis and 10.5 Lab 2: Clustering.
#The data are available at https://www.kaggle.com/datasets/fedesoriano/the-boston-houseprice-data
#Thanks to Lacie Pauw for suggesting the dataset.

#PCA: Use the code below.
housing_data <- read.csv("boston.csv", header=TRUE)
housing <- housing_data[,c("MEDV","RM","NOX","AGE")]
#MEDV: Median value of owner-occupied homes in $1000's
#RM: Average number of rooms per dwelling
#NOX: Nitric oxides concentration (parts per 10 million)
#AGE: Proportion of owner-occupied units built prior to 1940

pr.out=prcomp(housing, scale=TRUE)
names(pr.out)
dim(pr.out$x)

apply(housing, 2, mean)
apply(housing, 2, var)
pr.out$rotation
pr.out$x
biplot(pr.out, scale=0)

pr.out$sdev
pr.var=pr.out$sdev^2
pr.var
pve=pr.var/sum(pr.var)
pve
plot(pve, xlab="Principal Component", ylab="Proportion of Variance Explained", ylim=c(0,1),type='b')
plot(cumsum(pve), xlab="Principal Component", ylab="Cumulative Proportion of Variance Explained", 
     ylim=c(0,1),type='b')

#K-means clustering.
#prx is the matrix containing the first three principal component score vectors.
trio = c(1,2,3)
prx = pr.out$x[,trio]
Knum = 3 #this is for K=3. Change this value as necessary.
km.out = kmeans(prx, Knum, nstart=20) 

#Visualizing clusters. Repeat this for 1 vs 3 and 2 vs 3.
plot(prx[,c(1,2)], col=(km.out$cluster), 
     main="K-Means Results with K=3", 
     xlab="Z1", ylab="Z2", pch=20, cex=2) #modify xlab and ylab appropriately.

#3D scatterplot using plotly.
#See https://plotly.com/r/3d-scatter-plots/
library(plotly)

prx2 <- data.frame(prx)
prx2$cols <- as.factor(km.out$cluster)

#Assuming Knum = 3.
fig <- plot_ly(prx2, x = ~PC1, y = ~PC2, z = ~PC3, color = ~cols, 
               colors = c("black", "red","green"))
fig <- fig %>% add_markers()
fig <- fig %>% layout(scene = list(xaxis = list(title = 'PC1'),
                                   yaxis = list(title = 'PC2'),
                                   zaxis = list(title = 'PC3')))
fig


