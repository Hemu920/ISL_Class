# To begin, load the Boston data set. The Boston data set is part of the MASS library in R
library(MASS)
Boston
head(Boston)
?Boston

Boston_corrected = read.csv("/Users/chiru/Downloads/Boston_corrected.csv", header = TRUE)
head(Boston_corrected)

# Make some pairwise scatterplots of the predictors (columns) in the data set. Describe your findings.

dim(Boston_corrected)

par(mfrow = c(2, 2))
plot(Boston_corrected$age, Boston_corrected$medv, xlab = "Percent of units built prior to 1940", ylab = "Median home value in $1000s")
plot(Boston_corrected$lstat, Boston_corrected$medv, xlab = "Percent of lower status residents", ylab = "Median home value in $1000s")
plot(Boston_corrected$medv, Boston_corrected$ptratio, xlab = "Median home value in $1000s", ylab = "Pupil-teacher ratio")
plot(as.factor(Boston_corrected$chas), Boston_corrected$medv, xlab = "Borders Charles River", ylab = "Median home value in $1000s")


par(mfrow = c(2, 2))
plot(Boston_corrected$medv, Boston_corrected$nox, xlab = "Median home value in $1000s", ylab = "Nitric oxides concentration (parts per 10 million)")
plot(Boston_corrected$indus, Boston_corrected$nox, xlab = "Percent of non-retail business acres", ylab = "Nitric oxides concentration (parts per 10 million)")
plot(Boston_corrected$medv, Boston_corrected$b, xlab = "Median home value in $1000s", ylab = "1000(Proportion of black residents - 0.63)^2")
plot(Boston_corrected$dis, Boston_corrected$medv, xlab = "Weighted distance to Boston employment centers", ylab = "Median home value in $1000s")


# Are any of the predictors associated with per capita crime rate? If so, explain the relationship.
par(mfrow = c(2, 2))
plot(Boston_corrected$b, Boston_corrected$crim, xlab = "1000(Proportion of black residents - 0.63)^2", ylab = "Per capita crime rate")
plot(Boston_corrected$lstat, Boston_corrected$crim, xlab = "Percent of lower status residents", ylab = "Per capita crime rate")
plot(Boston_corrected$medv, Boston_corrected$crim, xlab = "Median home value in $1000s", ylab = "Per capita crime rate")
plot(Boston_corrected$dis, Boston_corrected$crim, xlab = "Weighted distance to Boston employment centers", ylab = "Per capita crime rate")

# Do any of the suburbs of Boston appear to have particularly high crime rates? Tax rates? Pupil-teacher ratios? Comment on the range of each predictor.

par(mfrow = c(2, 2))
hist(Boston_corrected$crim, xlab = "Per capita crime rate", main = "Histogram of Boston crime rates")
hist(Boston_corrected$tax, xlab = "Tax rate per 10000 USD", main = "Histogram of Boston tax rates")
hist(Boston_corrected$ptratio, xlab = "Pupil-teacher ratio", main = "Histogram of Boston pupil-teacher ratios")

summary(Boston_corrected[, c(8, 17, 18)])

# How many of the suburbs in this data set bound the Charles river?

sum(Boston_corrected$chas)

# What is the median pupil-teacher ratio among towns in this data set?
summary(Boston_corrected$ptratio)


# Which suburb of Boston has the lowest median value of owner-occupied homes? What are the values of the other predictors for that suburb, and how do those values compare to the overall ranges for those predictors? Comment on your findings.

min(Boston_corrected$medv)
Boston_corrected[Boston_corrected$medv == 5, ]
summary(Boston_corrected[, c(8:10, 12:20)])

# In this data set, how many of the suburbs average more than seven rooms per dwelling? More than eight rooms per dwelling? Comment on the suburbs that average more than eight rooms per dwelling.

sum(Boston_corrected$rm > 7)
sum(Boston_corrected$rm > 8)
Boston_corrected[Boston_corrected$rm > 8, ]
summary(Boston_corrected[Boston_corrected$rm > 8, c(7:10, 12:20)])