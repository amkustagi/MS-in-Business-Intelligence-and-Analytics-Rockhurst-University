getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 4/Assignment")
creditdefault_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 4/Assignment/creditdefault_data.csv")


library(psych)
attach(creditdefault_data)
names(creditdefault_data)
dim(creditdefault_data)
str(creditdefault_data)
options(scipen=999)

######################################
describe(creditdefault_data)

FREQ_TABLE<-table(default)
FREQ_TABLE

PROB_TABLE<-prop.table(FREQ_TABLE)
PROB_TABLE

#######################################

contrasts(as.factor(default))


MODEL1<-glm(default~ balance, data=creditdefault_data, family=binomial())
summary(MODEL1)

###########################################

contrasts(as.factor(student))


MODEL2<-glm(default~ student, data=creditdefault_data, family=binomial())
summary(MODEL2)


#########################################################

