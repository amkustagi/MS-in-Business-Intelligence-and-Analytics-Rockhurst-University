library(psych)
attach(medexpenses_data)
options(scipen = 999)

REG1<-lm(medical_expenses~age+bmi)
summary(REG1)
anova(REG1)

####33333##### SCALED (Z) REGRESSION ###########
standardized_data<-scale(medexpenses_data)
describe(standardized_data)

STANDARDIZED_DATA<-data.frame(standardized_data)
STANDARDIZED_REGRESSION<-lm(medical_expenses~age+bmi, data = STANDARDIZED_DATA)
summary(STANDARDIZED_REGRESSION)

write.csv(STANDARDIZED_DATA, file="C:EXPORTDATA.csv")
