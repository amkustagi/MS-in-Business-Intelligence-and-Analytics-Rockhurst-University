#### Why poisson dist is not normally distributed######
X = c(0:15)
X
MU <- 5
POISSON_DIST <- (MU^X)*(exp(-MU))/(factorial(X))
POISSON_DIST
sum(POISSON_DIST)
plot(POISSON_DIST)
# tail is not symmetrical --> Not normally distributed#

################################################################
getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 6")
aids_data <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 6/aids_data.csv")

attach(aids_data)
names(aids_data)
summary(aids_data)
plot(quarter, aids_deaths)
# Clearly not a linear model #############

POISSON_MODEL <- glm(aids_deaths~quarter, data=aids_data, family = poisson())
summary(POISSON_MODEL)
PREDICTED_VALUES <- predict(POISSON_MODEL)
PREDICTED_VALUES
PREDICTED_COUNTS <- exp(PREDICTED_VALUES)
PREDICTED_COUNTS

#################################################################

LINEAR_MODEL <- lm(aids_deaths~quarter, data = aids_data)
LINEAR_MODEL
plot(LINEAR_MODEL)
abline(LINEAR_MODEL)

