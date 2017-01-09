library(psych)
library(class)
getwd()
mowers_data <- read.csv("C:/Users/Avinash Kustagi/Desktop/Desktop/MS BIA/1M. Linear Modelling - SPRING A 2016/0. Material_for_Final_Exam_Assignment 6/mowers_data.csv")
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

CLUSTERS$cluster
COMPARE_METHODS<-data.frame(mowers_data$ownership, 
                            CLUSTERS$cluster)
COMPARE_METHODS

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

