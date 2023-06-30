library(mosaic)
view(KidsFeet)
KidsFeet.lm <- lm(length ~ width, data = KidsFeet)
summary(KidsFeet.lm)
par(mfrow=c(1,3))
plot(KidsFeet.lm,which=1:2)
plot(KidsFeet.lm$residuals)
library(ggplot2)
library(tidyverse)
library(DT)
library(car)
library(pander)
mPlot(KidsFeet)
gf_point(length ~ width, data = KidsFeet) %>% 
  gf_labs(title = "", caption = "") + 
  stat_smooth(method = lm)



# Multiple Linear Regression Assessment Quiz

Orange.lm <- lm(circumference ~ age, data = Orange)
summary(Orange.lm)
par(mfrow=c(1,3))
plot(Orange.lm,which=1:2)
plot(Orange.lm$residuals)
mPlot(Orange)
gf_point(circumference ~ age, data = Orange) %>% 
  gf_labs(title = "", caption = "") + 
  stat_smooth(method = lm)

kids.lm <- lm(length ~ width + as.factor(birthyear), data=KidsFeet)
summary(kids.lm)
