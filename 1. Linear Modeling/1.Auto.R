# Attach the file
getwd()
setwd("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 1")
Auto <- read.csv("D:/MS BIA/1M. Linear Modelling - SPRING A 2016/Week 1/Auto.csv")
DATA <- Auto
dim(DATA)
names(DATA)
attach(DATA)

# Descriptive Statistics
summary(DATA)

# Interpretation of MPG Summary
# 1st Quartile - 25% of the cars will have MPG of 17.5 or lesser
# 2nd Quartile - Median 50% of the cars will have MPG OF 23Mile or lesser
# 3rd Quartile - 75 % of the cars will have MPG of 29 or lesser

# Descriptive Statistics
#install.packages("psych")
library(psych)
View(DATA)
describe(DATA)

# Interpretation
# name* -> Non numeric variable
# Vars -> Variable number

# n -> No of observation

# SD -> The Standard Deviation is a measure of how spread out numbers are. 
# Its symbol is σ (the greek letter sigma) it is the square root of the Variance.

# Variance -> Variance is a measurement of the spread between numbers in a data set. 
# The variance measures how far each number in the set is from the mean

# Median (2nd Quartile) -> Value or quantity lying at the midpoint of a frequency distribution of 
# observed values or quantities

# Trimmed -> A trimmed mean is a method of averaging that removes a small designated percentage 
#of the largest and smallest values before calculating the mean. After removing the specified 
#observations, the trimmed mean is found using a standard arithmetic averaging formula.


# MAD -> Mean Absolute Deviation : On an average how for the data point from the mean

# Skew -> Skew show the shape of the graph ( https://www.thinglink.com/scene/439989194434019330)
# .3 ( + Ve Skew) , .4 ( + Ve Skew) ( .5 Now Skew)
# .9 ( - Ve Skew) , -.7 ( - Ve skew)

# Normal Distributions, Standard Deviations, Modality, Skewness and Kurtosis: 
# Understanding concepts - https://www.youtube.com/watch?v=HnMGKsupF8Q

# SE - Standard Error : SD/Square root(N)


#Read more: Trimmed Mean Definition | Investopedia http://www.investopedia.com/terms/t/trimmed_mean.asp#ixzz4V9GvG9SQ 



#What is the difference between the three terms below?

#percentile
#quantile
#quartile

#Percentiles go from 0 to 100 Quartiles go from 1 to 4 (or 0 to 4, see comment) 
#Quantiles can go from anything to anything. Percentiles and quartiles are examples of quantiles.


#Some q-quantiles have special names:
  
#The only 2-quantile is called the median
#The 3-quantiles are called tertiles or terciles → T
#The 4-quantiles are called quartiles → Q
#The 5-quantiles are called quintiles → QU
#The 6-quantiles are called sextiles → S
#The 8-quantiles are called octiles
#The 10-quantiles are called deciles → D
#The 12-quantiles are called duodeciles → Dd
#The 20-quantiles are called vigintiles → V
#The 100-quantiles are called percentiles → P
#The 1000-quantiles are called permilles → Pr






hist(DATA$mpg)
hist(horsepower)
plot(mpg, horsepower)

# Regression *******************

REG1 <- lm(mpg~horsepower)
summary(REG1)

#What Is R-squared?

#R-squared is a statistical measure of how close the data are to the fitted regression line. 
#It is also known as the coefficient of determination, or the coefficient of multiple determination for multiple regression.

anova(REG1)
predict(REG1)
predict(REG1, data.frame(horsepower=c(500)), interval="prediction")
abline(REG1)
plot(REG1)


