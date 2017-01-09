getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 5/Liner Modling MID TERM EXAM")
mutual_fund_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 5/Liner Modling MID TERM EXAM/mutual_fund_data.csv")

DATA <- mutual_fund_data
dim(DATA)
names(DATA)
attach(DATA)
summary(DATA)

library(psych)
describe(DATA)

contrasts(as.factor(fund_type))

REG1 <- lm(return~nav+expense_ratio+fund_type)
summary(REG1)
anova(REG1)
predict(REG1)

##############REGRESSIO SUBSETS#########3
library(leaps)
REGSUBSETS<-regsubsets(return~nav+expense_ratio+fund_type, data = DATA, nvmax = 3)

summary(REGSUBSETS)
SUMMARY_REGSUBSETS<-summary(REGSUBSETS)
names(SUMMARY_REGSUBSETS)

SUMMARY_REGSUBSETS$adjr2

REG2 <- lm(return~nav+expense_ratio+fund_type)
summary(REG2)
anova(REG2)
predict(REG2)

