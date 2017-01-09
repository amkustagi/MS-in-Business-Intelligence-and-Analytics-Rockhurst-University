getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 7/Material_for_Final_Exam_Assignment 6")
mowers_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 7/Material_for_Final_Exam_Assignment 6/1. mowers_data.csv")
par(mfrow=c(1,1))

library(psych)
library(DiscriMiner)
attach(mowers_data)
str(mowers_data)
describe(mowers_data)
options(scipen = 999)
#####################Approach 2 -- LDA #######################

corRatio(income, ownership)   # Correlation between numerica and categorical variable
corRatio(lotsize, ownership)

##########Linera Discreminent Analysis################################

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


describe(mowers_data)



##################### Approach NO 1 K Means ########################

