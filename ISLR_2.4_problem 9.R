# This exercise involves the Auto data set studied in the lab. Make sure that the missing values have been removed from the data.

Auto = read.csv("/Users/chiru/Downloads/Auto.csv", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)
dim(Auto)


# Which of the predictors are quantitative, and which are qualitative?

head(Auto)

# What is the range of each quantitative predictor? You can answer this using the range() function.

?range

range(Auto$mpg)
range(Auto$cylinders)
range(Auto$displacement)
range(Auto$horsepower)
range(Auto$weight)
range(Auto$acceleration)
range(Auto$year)

# What is the mean and standard deviation of each quantitative predictor?

colMeans(Auto[, 1:7])
apply(Auto[, 1:7], MARGIN = 2, FUN = "sd")

# Now remove the 10th through 85th observations. What is the range, mean, and standard deviation of each predictor in the subset of the data that remains?
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "range")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "mean")
apply(Auto[-(10:85), 1:7], MARGIN = 2, FUN = "sd")

# Using the full data set, investigate the predictors graphically, using scatterplots or other tools of your choice. Create some plots highlighting the relationships among the predictors. Comment on your findings.

par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg, xlab = "Engine displacement (cubic inches)", ylab = "Miles per gallon")
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
plot(Auto$weight, Auto$mpg, xlab = "Car weight (pounds)", ylab = "Miles per gallon")
plot(Auto$year, Auto$mpg, xlab = "Model Year", ylab = "Miles per gallon")


par(mfrow = c(2, 2))
plot(Auto$year, Auto$acceleration, xlab = "Model Year", ylab = "0 to 60mph time (seconds)")
plot(Auto$year, Auto$displacement, xlab = "Model Year", ylab = "Engine displacement (cubic inches)")
plot(Auto$year, Auto$weight, xlab = "Model Year", ylab = "Car weight (pounds)")
plot(Auto$year, Auto$horsepower, xlab = "Model Year", ylab = "Horsepower")


par(mfrow = c(2, 2))
plot(Auto$weight, Auto$acceleration, xlab = "Car weight (pounds)", ylab = "0 to 60mph time (seconds)")
plot(Auto$cylinders, Auto$acceleration, xlab = "Number of engine cylinders", ylab = "0 to 60mph time (seconds)")
plot(Auto$displacement, Auto$acceleration, xlab = "Engine displacement (cubic inches)", ylab = "0 to 60mph time (seconds)")
plot(Auto$horsepower, Auto$acceleration, xlab = "Horsepower", ylab = "0 to 60mph time (seconds)")

par(mfrow = c(2, 1))
plot(Auto$weight, Auto$horsepower, xlab = "Car weight (pounds)", ylab = "Horsepower")
plot(Auto$weight, Auto$displacement, xlab = "Car weight (pounds)", ylab = "Engine displacement (cubic inches)")

# Suppose we wish to predict gas mileage (mpg) on the basis of the other variables. Do your plots suggest that any of the other variables might be useful in predicting mpg? 

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)

plot(Auto$origin, Auto$mpg, xlab = "Country of origin", ylab = "Miles per gallon")


