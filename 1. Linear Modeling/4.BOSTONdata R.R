getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Assignment")
BOSTONDATA <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Assignment/BOSTONDATA.csv")

library(MASS)
fix(Boston)
write.csv(Boston, file="D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 2/Assignment/BOSTONDATA.csv")
View(BOSTONDATA)
DATA <- BOSTONDATA
dim(DATA)
names(DATA)
attach(DATA)

summary(DATA)
library(psych)
describe(DATA)
hist(crim)
hist(medv)
plot(crim, medv)
plot(medv, crim)

REG1 <- lm(crim~medv)
summary(REG1)
anova(REG1)
predict(REG1)
abline(REG1)
plot(REG1)

# all available variables
REG2 <- lm(crim~medv + indus + zn + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat)
summary(REG2)
anova(REG2)
predict(REG2)
resid(REG2)

predict(REG2, data.frame(BOSTONDATA=c(500)),type="response")
?predict
# Considering only insignificant variables
REG3 <- lm(crim~indus + chas + nox + rm + age + tax + ptratio)
summary(REG3)

# Considering only significant variables
REG4 <- lm(crim~medv + zn + dis + rad + black)
summary(REG4)



# to get the adjusted R2 value ---- SCALED (Z) REGRESSION ###########

standardized_bostondata<-scale(BOSTONDATA)
describe(standardized_bostondata)

STANDARDIZED_BOSTONDATA<-data.frame(standardized_bostondata)
STANDARDIZED_BOSTON_REGRESSION<-lm(crim~medv + indus + zn + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = STANDARDIZED_BOSTONDATA)
summary(STANDARDIZED_BOSTON_REGRESSION)


##############REGRESSIO SUBSETS#########3
library(leaps)
REGSUBSETS<-regsubsets(crim~medv + indus + zn + chas + nox + rm + age + dis + rad + tax + ptratio + black + lstat, data = STANDARDIZED_BOSTONDATA, nvmax = 13)

summary(REGSUBSETS)
SUMMARY_REGSUBSETS<-summary(REGSUBSETS)
names(SUMMARY_REGSUBSETS)

SUMMARY_REGSUBSETS$adjr2


###### Using R-Square, it says using 9 variable creates the best model, #########
############ Running the model using 9 variables###################


standardized_bostondata<-scale(BOSTONDATA)
describe(standardized_bostondata)

STANDARDIZED_BOSTONDATA<-data.frame(standardized_bostondata)
STANDARDIZED_BOSTON_REGRESSION_FOR9<-lm(crim~medv + indus + zn + nox + dis + rad + ptratio + black + lstat, data = STANDARDIZED_BOSTONDATA)
summary(STANDARDIZED_BOSTON_REGRESSION_FOR9)




