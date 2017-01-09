library(psych)
library(DiscriMiner)
attach(death_penalty_data)
str(death_penalty_data)
describe(death_penalty_data)
options(scipen = 999)
#####################Approach 2 -- LDA #######################

table(race_of_victim,death_penalty)

#contrasts(as.factor(ownership))
LOGIT_MODEL <- glm(death_penalty~severity_of_crime+race_of_victim, data = death_penalty_data, family = binomial())
summary(LOGIT_MODEL)

predict(LOGIT_MODEL)
ODDS <- exp(predict(LOGIT_MODEL))
ODDS
Probability <- (ODDS/(1+ODDS))
Probability
PROBABILITY<-data.frame(Probability*100)
PROBABILITYS
describe(PROBABILITY)

