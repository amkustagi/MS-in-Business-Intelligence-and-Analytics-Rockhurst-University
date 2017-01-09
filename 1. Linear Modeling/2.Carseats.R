data()
library(ISLR)
fix(Carseats)
View(Carseats)
names(Carseats)
attach(Carseats)


#Converting as Factors
contrasts(as.factor(Urban))
contrasts(as.factor(US))

#Regression
REGMODEL1<-lm(Sales~Price+Urban+US)
summary(REGMODEL1)
anova(REGMODEL1)


REGMODEL2<-lm(Sales~Price+US)
summary(REGMODEL2)
anova(REGMODEL2)

#****************DETECTING OUTLIERS******************
  library (outliers)
outlier(Sales)
outlier(Price)

par(mfrow = c(2,2))
plot(REGMODEL2)

plot(hatvalues(REGMODEL2))
which.max (hatvalues(REGMODEL2))

