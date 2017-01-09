DATA <- DEATH_PENALTY_COUNTS
dim(DATA)
names(DATA)
attach(DATA)
options(scipen = 999)
library(psych)

########## POISSION MODEL #######

POISSON_MODEL <- glm(COUNTS~SEVERITY_OF_CRIME+RACE_OF_VICTIM, 
                     data=DATA, family = poisson())
summary(POISSON_MODEL)
PREDICTED_VALUES <- predict(POISSON_MODEL)
PREDICTED_VALUES
PREDICTED_COUNTS <- exp(PREDICTED_VALUES)
PREDICTED_COUNTS


