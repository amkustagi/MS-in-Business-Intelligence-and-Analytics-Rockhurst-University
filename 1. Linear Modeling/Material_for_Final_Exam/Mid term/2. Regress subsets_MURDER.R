library(psych)

# global validation of linear model assumption #

library(gvlma)  
library(leaps)  

###########################SUBSET DATA################
attach(murder_data)
colnames(murder_data)
MURDER_DATA <- as.data.frame(murder_data[,c("Murder","Population","Income","Illiteracy","Frost")])
View(MURDER_DATA)
attach(MURDER_DATA)
cor(MURDER_DATA)

REGpop<-lm(Murder~Population)
anova(REGpop)
summary(REGpop)
predict(REGpop)

REG1<-lm(Murder~Population+Illiteracy+Income+Frost)
anova(REG1)
summary(REG1)
predict(REG1)

gvlma(REG1)

##############REGRESSIO SUBSETS#########3
library(leaps)
REGSUBSETS<-regsubsets(Murder~Population+Illiteracy+Income+Frost, data=MURDER_DATA, nvmax = 4)

summary(REGSUBSETS)
SUMMARY_REGSUBSETS<-summary(REGSUBSETS)
names(SUMMARY_REGSUBSETS)

SUMMARY_REGSUBSETS$adjr2

