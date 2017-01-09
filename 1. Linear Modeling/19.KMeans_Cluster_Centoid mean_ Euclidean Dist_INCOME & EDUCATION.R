
##### K Means Approch ####

getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 7")
Income.and.Education <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 7/Income and Education.csv")
par(mfrow=c(1,1))

library(psych)
attach(Income.and.Education)
ZDATA <- scale(Income.and.Education)
describe(ZDATA)


CLUSTERS <- kmeans(ZDATA,2)
CLUSTERS


EUCLIDEAN_DISTS <- dist(ZDATA, method = "euclidean")
EUCLIDEAN_DISTS

