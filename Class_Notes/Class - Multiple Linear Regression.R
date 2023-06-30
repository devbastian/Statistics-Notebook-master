library(mosaic)
library(pander)
library(tidyverse)
library(car)

view(mtcars)
palette(c("skyblue","firebrick"))
plot(mpg ~ qsec, 
     pch=16, 
     col = as.factor(am), 
     data = mtcars)
legend("topright", 
       legend = c("automatic", "manual"), 
       col = c("skyblue", "firebrick"), 
       title= "Transmission (am)", 
       bty = "n", 
       pch = 1)
mtlm <- lm(mpg ~qsec + am + am:qsec, 
           data = mtcars)
summary(mtlm)
abline(-9.0099, 1.4385, 
       col = "skyblue")
abline((-9.0099 + -14.5107), 
       (1.4385 + 1.3214),
       col = ("firebrick"))

-9.0099+-14.5107
1.4385 + 1.3214



#Recreating Monday's linear model (lm)

palette(c("skyblue", "firebrick"))
plot(mpg ~ qsec, 
     data = mtcars, 
     col = as.factor(am), 
     xlim = c(0,30), 
     ylim = c(0,40),
     main = "1974 Motor Trend Cars",
     pch = 16)
mylm <- lm(mpg ~ qsec + am + qsec:am,
           data = mtcars)
b <- coef(mylm)
abline(b[1], 
       b[2],
       cd = "skyblue")
abline(b[1] + b[3],
       b[2] + b[4],
       col = "firebrick")


#Make graphs where you force the y-intercepts to meet and where the slopes can't change.


palette(c("skyblue", "firebrick"))
plot(mpg ~ qsec, 
     data = mtcars, 
     col = as.factor(am), 
     xlim = c(0,30), 
     ylim = c(-30,40),
     main = "1974 Motor Trend Cars",
     pch = 16)
slope.mylm <- lm(mpg ~ qsec + am,
                 data = mtcars)
summary(slope.mylm)
b1 <- coef(slope.mylm)
abline(b1[1], 
       b1[2],
       cd = "skyblue")
abline(b1[1] + b1[3], b1[2],
       col = "firebrick")


#Create a graph that has the y-intercept as the same value.

palette(c("skyblue", "firebrick"))
plot(mpg ~ qsec, 
     data = mtcars, 
     col = as.factor(am), 
     xlim = c(0,30), 
     ylim = c(-30,40),
     main = "1974 Motor Trend Cars",
     pch = 16)
yintercept.mylm <- lm(mpg ~ qsec + qsec:am,
                      data = mtcars)
summary(yintercept.mylm)
b2 <- coef(yintercept.mylm)
abline(b2[1], 
       b2[2],
       cd = "skyblue")
abline(b2[1], b2[3] + b2[2],
       col = "firebrick")