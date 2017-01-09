DATA <- strokes_data
dim(DATA)
names(DATA)
attach(DATA)

##### HISTO GRAM############
summary(DATA)
library(psych)
describe(DATA)
hist(strokes_pre_therapy)
hist(strokes_pre_therapy, breaks=19)
summary(strokes_pre_therapy)

hist(strokes_post_therapy)
hist(strokes_post_therapy, breaks=19)
summary(strokes_post_therapy)

##### Linear Regression Model ####

REG <- lm(strokes_post_therapy~strokes_pre_therapy+treatment+ age)
summary(REG)
predict(REG)

########## POISSION MODEL #######

POISSON_MODEL <- glm(strokes_post_therapy~strokes_pre_therapy+treatment+ age, 
                     data=DATA, family = poisson())
summary(POISSON_MODEL)
PREDICTED_VALUES <- predict(POISSON_MODEL)
PREDICTED_VALUES
PREDICTED_COUNTS <- exp(PREDICTED_VALUES)
PREDICTED_COUNTS


