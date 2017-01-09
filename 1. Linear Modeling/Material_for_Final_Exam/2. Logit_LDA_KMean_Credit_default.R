library(psych)
library(DiscriMiner)
library(plyr)
library(outliers)
options(scipen=999)
######################################################
dim(credit_default_data)
names(credit_default_data)
str(credit_default_data)
attach(credit_default_data)
describe(credit_default_data)

table(default)
contrasts (as.factor(default))  
outlier(balance)
outlier(income)
####################LOGIT MODEL############################
LOGIT_MODEL<-glm(default~student+balance+income, data=credit_default_data, 
                 family=binomial())

summary(LOGIT_MODEL)
predict(LOGIT_MODEL)
ODDS<-exp(predict(LOGIT_MODEL))
ODDS

PROB<-(ODDS)/(1+ODDS)
PROBABILITY<-data.frame(PROB*100)
describe(PROBABILITY)

#####################LDA MODEL########################
library(DiscriMiner)
contrasts(as.factor(default))

LDA_MODEL<-linDA(credit_default_data[,3:5], credit_default_data[,2])
LDA_MODEL
LDA_MODEL$scores
LDA_MODEL$classification

COMPARISON<-data.frame(credit_default_data$default, 
                       LDA_MODEL$classification)
COMPARISON
table(COMPARISON)

#########################K-MEANS ############################

NUMERIC_DATA<-credit_default_data[-1:-2]
NUMERIC_DATA

SCALED_DATA<-scale(NUMERIC_DATA)
SCALED_DATA

CLUSTERS<-kmeans(SCALED_DATA, 2)
CLUSTERS
CLUSTERS$cluster
COMPARE_METHODS<-data.frame(credit_default_data$default, 
                            LDA_MODEL$classification,
                            CLUSTERS$cluster)
COMPARE_METHODS

