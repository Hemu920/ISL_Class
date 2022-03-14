Auto = read.csv("/Users/chiru/Downloads/Auto.data", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)

# Use the lm() function to perform a simple linear regression with mpg as the response and horsepower as the predictor. Use the summary() function to print the results

auto.lin.fit = lm(mpg ~ horsepower, data = Auto)
summary(auto.lin.fit)
predict(auto.lin.fit, data.frame(horsepower = 98), interval = "confidence")

predict(auto.lin.fit, data.frame(horsepower = 98), interval = "prediction")


# Plot the response and the predictor. Use the abline() function to display the least squares regression line.
plot(Auto$horsepower, Auto$mpg, xlab = "Horsepower", ylab = "Miles per gallon")
abline(auto.lin.fit, lwd = 3, col = "red")

#Use the plot() function to produce diagnostic plots of the least squares regression fit. Comment on any problems you see with the fit.
par(mfrow = c(2, 2))
plot(auto.lin.fit)