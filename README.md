# Task_2
Task 2(The Sparks Foundation Internship Programme):Prediction using UnSupervised Machine Learning

#Install Required Packages
#install.packages("stats")
(require(stats))
#As it is TRUE the required package is already installed 
install.packages("dplyr")
install.packages("ggplot2")
install.packages("ggfortify")
install.packages("datasets")
(require(datasets))
#Load repuired libraries

library(stats)
library(dplyr)

library(ggplot2)
library(ggfortify)
library(datasets)

#Unsupervised Machine learning 
head(iris)
View(iris)
?iris
#Unlabel the data by dropping the species column from the dataset
mydata = select(iris,c(1,2,3,4))


#WSS plot to choose maximum number of cluster
wssplot<- function(data,nc=15,seed=1234)
{
  wss<- (nrow(data)-1)*sum(apply(data,2,var))
  for(i in 2:nc){
    set.seed(seed)
    wss[i]<- sum(kmeans(data,center=i)$withinss)}
  plot(1:nc,wss,type="b",xlab="Number of Cluster",
       ylab="within groups sum of squares")
}

wssplot(mydata)
#Here the elbow point is at K=3
#K-Means Cluster #
KM =kmeans(mydata,3)
 
#cluster plot#
autoplot(KM,mydata,frame=TRUE)

#cluster centres #
KM$centers
