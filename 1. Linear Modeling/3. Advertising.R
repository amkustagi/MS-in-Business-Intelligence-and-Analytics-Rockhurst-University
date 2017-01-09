getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Assignment")
Advertising <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Assignment/Advertising.csv")
DATA <- Advertising
dim(DATA)
names(DATA)
attach(DATA)
summary(DATA)

library(psych)
describe(DATA)
hist(Sales)
hist(TV)
plot(TV, Sales)

REG1 <- lm(Sales~TV)
summary(REG1)
anova(REG1)
predict(REG1)

REG2 <- lm(Sales~Radio)
summary(REG2)
anova(REG2)
predict(REG2)

REG3 <- lm(Sales~Newspaper)
summary(REG3)
anova(REG3)
predict(REG3)

REG4 <- lm(Sales~TV + Radio + Newspaper)
summary(REG4)
anova(REG4)
predict(REG4)

View(DATA)
View(DATA[1:3])
View(DATA[1:3,])
View(DATA[1:3,2:4])


pairs(DATA)
pairs(DATA[,2:5])
cor(DATA[,2:5])

cor(DATA[,2:5], method = "spearman")

cov(DATA[,2:5])
cov(DATA[,2:5], method = "spearman")

