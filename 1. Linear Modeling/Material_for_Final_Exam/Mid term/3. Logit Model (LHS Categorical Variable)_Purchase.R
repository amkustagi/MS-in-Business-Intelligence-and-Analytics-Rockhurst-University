attach(purchase_data)

library(psych)
names(purchase_data)
options(scipen = 999)
########################### General Linear Model - Logitmodel#####
LOGITMODEL <- glm(purchase~income+age+zip, data=purchase_data, family = binomial())
summary(LOGITMODEL)

LOGITS <- data.frame(predict(LOGITMODEL))
LOGITS

ODDS <- data.frame(exp(LOGITS))
ODDS

PROBABILITY <- data.frame(ODDS/(1+ODDS))
PROBABILITY

describe(PROBABILITY)
