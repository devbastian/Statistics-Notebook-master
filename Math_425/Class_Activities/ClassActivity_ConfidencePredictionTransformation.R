# Class Activity - Confidence, Prediction, and Transformations

plot(gasbill ~ temp, data = Utilities)
u.lm <- lm(gasbill ~ temp, data = Utilities)
abline(u.lm, col = "hotpink")
u.lm.t <- lm(sqrt(sqrt(gasbill)) ~ temp, data = Utilities)
b <- coef(u.lm.t)
curve((b[1] +  b[2]*x)^4, add = TRUE, col = "skyblue", lwd = 2)
abline(h=predict(u.lm, data.frame(temp = 30), interval = "prediction"), lty = 2, col = "hotpink")
abline(v=30, lty = 2, col = "skyblue")
abline(h= predict(u.lm.t, data.frame(temp = 30), interval = "prediction")^4, lty = 2, col = "skyblue")

boxCox(u.lm)
boxCox(u.lm, lambda = seq(0,1, 1))
library(car)
