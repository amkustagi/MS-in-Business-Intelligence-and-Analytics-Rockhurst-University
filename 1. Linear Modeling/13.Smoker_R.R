getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 5/Liner Modling MID TERM EXAM")
smoker_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 5/Liner Modling MID TERM EXAM/smoker_data.csv")



attach(smoker_data)

library(psych)
names(smoker_data)
options(scipen = 999)
########################### General Linear Model - Logitmodel#####
describe(smoker_data)
FREQUENCYSMOKE_TABLE<-table(smoker)
FREQUENCYSMOKE_TABLE
PROBABILITY_TABLE<-prop.table(FREQUENCYSMOKE_TABLE)
PROBABILITY_TABLE
FULL_MODEL<-glm(smoker~age+education+income+price_cigs, data=smoker_data, family=binomial())
summary(FULL_MODEL)


##############REGRESSIO SUBSETS#########3
library(leaps)
REGSUBSETS<-regsubsets(smoker~age+education+income+price_cigs, data=smoker_data, nvmax = 4)

summary(REGSUBSETS)
SUMMARY_REGSUBSETS<-summary(REGSUBSETS)
names(SUMMARY_REGSUBSETS)

SUMMARY_REGSUBSETS$adjr2

FULL_MODEL1<-glm(smoker~age+education+price_cigs, data=smoker_data, family=binomial())
summary(FULL_MODEL1)
