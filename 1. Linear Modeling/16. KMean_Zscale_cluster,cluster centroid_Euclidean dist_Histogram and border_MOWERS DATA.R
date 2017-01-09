getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 7/Material_for_Final_Exam_Assignment 6")
mowers_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 7/Material_for_Final_Exam_Assignment 6/1. mowers_data.csv")
par(mfrow=c(1,1))


library(psych)
library(class)
attach(mowers_data)
scale(mowers_data)
NUMERIC_MOWERS_DATA <- mowers_data[-1]
NUMERIC_MOWERS_DATA
describe(NUMERIC_MOWERS_DATA)

ZDATA <- scale(NUMERIC_MOWERS_DATA)
describe(ZDATA)

###############K- Mean Cluster###########################

CLUSTERS <- kmeans(ZDATA,2)
CLUSTERS

EUCIDEAN_DISTS <- dist(ZDATA, method = "euclidean")
EUCIDEAN_DISTS

#############################################

DENDROGRAM <- hclust(EUCIDEAN_DISTS, method = "ward.D")
DENDROGRAM
plot(DENDROGRAM, labels = mowers_data$ownership)

GROUPS <- cutree(DENDROGRAM, k=3)
GROUPS
rect.hclust(DENDROGRAM, k=3, border = "red")

####### hclst --- Hierarchial clustring #####

