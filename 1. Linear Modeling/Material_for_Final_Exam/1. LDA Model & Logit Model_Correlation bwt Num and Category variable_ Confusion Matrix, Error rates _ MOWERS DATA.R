library(psych)
library(DiscriMiner)
mowers_data <- read.csv("C:/Users/Avinash Kustagi/Desktop/Desktop/MS BIA/1M. Linear Modelling - SPRING A 2016/0. Material_for_Final_Exam_Assignment 6/mowers_data.csv")

attach(mowers_data)
str(mowers_data)
contrasts(as.factor(ownership))
describe(mowers_data)
options(scipen = 999)
#####################Approach 2 -- LDA #######################

corRatio(income, ownership)   # Correlation between numerica and categorical variable
corRatio(lotsize, ownership)

##########Linera Discreminent Analysis################################
View(mowers_data)
LDA_MODEL <- linDA(mowers_data[,2:3], ownership)
LDA_MODEL
LDA_MODEL$scores
LDA_MODEL$classification
COMPARISON <- data.frame(mowers_data$ownership, LDA_MODEL$classification)
COMPARISON


########APPROACH NO 3 LOGIT MODELS########################################################


contrasts(as.factor(ownership))
LOGIT_MODEL <- glm(ownership~income+lotsize, data = mowers_data, family = binomial())
summary(LOGIT_MODEL)

predict(LOGIT_MODEL)
ODDS <- exp(predict(LOGIT_MODEL))
ODDS
Probability <- (ODDS/(1+ODDS))
Probability
PROBABILITY<-data.frame(Probability*100)
PROBABILITY
describe(PROBABILITY)


describe(mowers_data)



##################### Approach NO 1 K Means ########################

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

CLUSTERS$cluster
COMPARE_METHODS<-data.frame(mowers_data$ownership, 
                            LDA_MODEL$classification,
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

