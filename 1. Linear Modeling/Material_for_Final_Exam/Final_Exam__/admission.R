library(psych)
library(DiscriMiner)
attach(admission_data)
str(admission_data)
#contrasts(as.factor(ownership))
describe(admission_data)
options(scipen = 999)
#####################Approach 2 -- LDA #######################
##########Linera Discreminent Analysis################################

LDA_MODEL <- linDA(admission_data[,1:2], decision)
LDA_MODEL
LDA_MODEL$scores
LDA_MODEL$classification
COMPARISON <- data.frame(admission_data$decision, LDA_MODEL$classification)
COMPARISON

