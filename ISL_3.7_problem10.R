library(ISLR)
head(Carseats)

# Fit a multiple regression model to predict Sales using Price, Urban, and US.
carseats.fit.1 = lm(Sales ~ Price + Urban + US, data = Carseats)
summary(carseats.fit.1)

contrasts(Carseats$Urban)

contrasts(Carseats$US)

# On the basis of your response to the previous question, fit a smaller model that only uses the predictors for which there is evidence of association with the outcome.

carseats.fit.2 = lm(Sales ~ Price + US, data = Carseats)
summary(carseats.fit.2)

# How well do the models in Part 1 and Part 5 fit the data?

par(mfrow = c(2, 2))
plot(carseats.fit.1)

par(mfrow = c(2, 2))
plot(carseats.fit.2)

# Using the model from Part 5, obtain 95% confidence intervals for the coefficient(s).

confint(carseats.fit.2)