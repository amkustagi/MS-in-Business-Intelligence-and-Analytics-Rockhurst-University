library(psych)
library(stats)
options(digits = 4)
################################################
protein <- read.csv("C:/Users/Avinash Kustagi/Desktop/Desktop/MS BIA/1M. Linear Modelling - SPRING A 2016/0. Material_for_Final_Exam_Assignment 6/protein.csv")

attach(protein)
dim(protein)
names(protein)
str(protein)
protein$Country<-as.character(protein$Country)
summary(protein)
cor(RedMeat, WhiteMeat)
plot(RedMeat, WhiteMeat)

##############################################
PRO_DATA <- scale(protein[,2:3])
PRO_DATA
describe(PRO_DATA)

########################   KMean  ########################
set.seed(1)
CLUSTERS <- kmeans(PRO_DATA,3)
CLUSTERS

CLUSTERS1 <- data.frame(Country, CLUSTERS$cluster)
CLUSTERS1
#############################################

EUCLIDEAN_DISTS <- dist(PRO_DATA, method = "euclidean")
EUCLIDEAN_DISTS

#############################################

DENDROGRAM <- hclust(EUCLIDEAN_DISTS, method = "ward.D")
DENDROGRAM
plot(DENDROGRAM, labels = protein$Country)

GROUPS <- cutree(DENDROGRAM, k=3)
GROUPS
rect.hclust(DENDROGRAM, k=3, border = "red")

####### hclst --- Hierarchial clustring #####




#%%%%%%%%%%%%%% CLUSTER 7###############################################
set.seed(1)
CLUSTERS <- kmeans(PRO_DATA,7)
CLUSTERS

CLUSTERS2 <- data.frame(Country, CLUSTERS$cluster)
CLUSTERS2
#############################################

EUCLIDEAN_DISTS <- dist(PRO_DATA, method = "euclidean")
EUCLIDEAN_DISTS

#############################################

DENDROGRAM <- hclust(EUCLIDEAN_DISTS, method = "ward.D")
DENDROGRAM
plot(DENDROGRAM, labels = protein$Country)

GROUPS <- cutree(DENDROGRAM, k=7)
GROUPS
rect.hclust(DENDROGRAM, k=7, border = "red")

####### hclst --- Hierarchial clustring #####

