getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Raw data")
gold_stockprices_inflation_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Raw data/gold_stockprices_inflation_data.csv")

DATA <- gold_stockprices_inflation_data
dim(DATA)
names(DATA)
attach(DATA)

# ***********************************

  summary(DATA)
library(psych)
describe(DATA)
hist(goldprices)
plot(cpi, goldprices)
plot(cpi, stockindex)

#*************************************
  REG1 <- lm(goldprices~cpi)
summary(REG1)
anova(REG1)
predict(REG1)

#*************************************
  options(scipen = 999)
REG2 <- lm(stockindex~cpi)
summary(REG2)
anova(REG2)
predict(REG2)

#**************************************
  library(ISLR)
data()
fix(cars)
 
library(MASS)
data()
fix(Boston)

library(outliers)
