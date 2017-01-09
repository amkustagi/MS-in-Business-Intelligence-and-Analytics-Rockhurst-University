attach(affairs_data)

library(psych)
names(affairs_data)
options(scipen = 999)
########################### General Linear Model - Logitmodel#####
describe(affairs_data)
FREQUENCY_TABLE<-table(number_affairs)
FREQUENCY_TABLE
PROBABILITY_TABLE<-prop.table(FREQUENCY_TABLE)
PROBABILITY_TABLE
FULL_MODEL<-glm(YNAFFAIRS~male+age+years_married+children+religious+education+occupation+marriage_rating, data=affairs_data, family=binomial())
summary(FULL_MODEL)

REDUCED_MODEL<-glm(YNAFFAIRS~age+years_married+religious+marriage_rating,data=affairs_data, family=binomial())
summary(REDUCED_MODEL)


coef(REDUCED_MODEL)
exp(coef(REDUCED_MODEL))

#########################################################

LOGITS<-data.frame(predict(REDUCED_MODEL))
LOGITS

ODDS<-data.frame(exp(LOGITS))
ODDS

PROBABILITY<-data.frame(ODDS/(1+ODDS))
PROBABILITY

min(PROBABILITY)
max(PROBABILITY)

describe(PROBABILITY)

