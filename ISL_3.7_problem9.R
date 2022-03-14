Auto = read.csv("/Users/chiru/Downloads/Auto.data", header = TRUE, na.strings = "?")
Auto = na.omit(Auto)
head(Auto)

Auto$origin[Auto$origin == 1] = "American"
Auto$origin[Auto$origin == 2] = "European"
Auto$origin[Auto$origin == 3] = "Japanese"
Auto$origin = as.factor(Auto$origin)
head(Auto)

# Produce a scatterplot matrix which includes all of the variables in the data set.

pairs(~mpg + cylinders + displacement + horsepower + weight + acceleration + year, Auto)

# Compute the matrix of correlations between the variables using the function cor(). You will need to exclude the name variable, which is qualitative.
cor(Auto[,-c(8, 9)])


#Use the lm() function to perform a multiple linear regression with mpg as the response and all other variables except name as the predictors. Use the summary() function to print the results. Comment on the output. For instance:

mpg.fit = lm(mpg ~ . - name, data = Auto)
summary(mpg.fit)
contrasts(Auto$origin)
library(car)
vif(lm(mpg ~ . - origin - name, data = Auto))

# Use the plot() function to produce diagnostic plots of the linear regression fit. Comment on any problems you see with the fit. Do the residual plots suggest any unusually large outliers? Does the leverage plot identify any observations with unusually high leverage?

par(mfrow = c(2, 2))
plot(mpg.fit)

# Use the * and : symbols to fit linear regression models with interaction effects. Do any interactions appear to be statistically significant?

mpg.fit.all.interactions = lm(mpg ~ (. - name)^2, data = Auto)
summary(mpg.fit.all.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.all.interactions, ~ . - horsepower:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - displacement:horsepower)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:acceleration)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - weight:year)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:horsepower)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . + cylinders:origin - displacement:acceleration)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:origin)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:displacement)
summary(mpg.fit.reduced.interactions)

mpg.fit.reduced.interactions = update(mpg.fit.reduced.interactions, ~ . - cylinders:weight)
summary(mpg.fit.reduced.interactions)

summary(lm(mpg ~ . + cylinders:displacement - name, data = Auto))

# Try a few different transformations of the variables, such as  log(X),X−−√,X2

par(mfrow = c(2, 2))
plot(Auto$displacement, Auto$mpg)
plot(Auto$horsepower, Auto$mpg)
plot(Auto$weight, Auto$mpg)
plot(Auto$acceleration, Auto$mpg)

summary(lm(mpg ~ acceleration, data = Auto))

par(mfrow = c(2, 2))
plot(lm(mpg ~ acceleration, data = Auto))

summary(lm(mpg ~ log(acceleration), data = Auto))

par(mfrow = c(2, 2))
plot(lm(mpg ~ log(acceleration), data = Auto))

displacement.linear = lm(mpg ~ displacement, data = Auto)
summary(displacement.linear)

displacement.quadratic = lm(mpg ~ poly(displacement, 2), data = Auto)
summary(displacement.quadratic)

anova(displacement.linear, displacement.quadratic)

displacement.quintic = lm(mpg ~ poly(displacement, 5), data = Auto)
summary(displacement.quintic)