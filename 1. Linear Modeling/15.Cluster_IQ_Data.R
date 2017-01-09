getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 6")
iq_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 6/iq_data.csv")
par(mfrow=c(1,1))

library(psych)
library(stats)
options(digits = 4)
################################################

attach(iq_data)
dim(iq_data)
names(iq_data)
cor(gdp, iq)
plot(gdp, iq)

##############################################

IQ_DATA <- scale(iq_data)
IQ_DATA
describe(IQ_DATA)

############################################

EUCLIDEAN_DISTS <- dist(IQ_DATA, method = "euclidean")
EUCLIDEAN_DISTS

################################################

CLUSTERS <- kmeans(IQ_DATA,2)
CLUSTERS
  
#############################################

EUCLIDEAN_DISTS <- dist(IQ_DATA, method = "euclidean")
EUCLIDEAN_DISTS

#############################################

DENDROGRAM <- hclust(EUCLIDEAN_DISTS, method = "ward.D")
DENDROGRAM
plot(DENDROGRAM)

GROUPS <- cutree(DENDROGRAM, k=2)
GROUPS
rect.hclust(DENDROGRAM, k=2, border = "red")
  