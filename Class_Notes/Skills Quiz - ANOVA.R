Skills Quiz - ANOVA


View(warpbreaks)

library(car) 
friendly.aov <- aov(correct ~ condition, data=Friendly)
summary(friendly.aov)
par(mfrow=c(1,2))
plot(friendly.aov, which=1:2)
View(Friendly)
xyplot(correct ~ condition, data=Friendly, jitter.x=TRUE, type=c("p","a"))




library(mosaic)
library(pander)
library(tidyverse)
?ToothGrowth
View(ToothGrowth)
xyplot(len ~ dose, groups=supp, data=ToothGrowth, type=c("p","a"), auto.key=TRUE)

Tooth.myaov <- aov(len ~ as.factor(dose)+supp+as.factor(dose):supp, data=ToothGrowth) #Perform the ANOVA
summary(Tooth.myaov) #View the ANOVA Results
par(mfrow = c(1,3))
plot(Tooth.myaov, which=1:2)

xyplot( len ~ supp, data=ToothGrowth, type=c("p","a")) 
xyplot( len ~ dose, data=ToothGrowth, type=c("p","a"))
xyplot( len ~ supp, data=ToothGrowth, groups=dose, type=c("p","a"), auto.key=TRUE) 







# Assessment Quiz
View(KidsFeet)
Feet.myaov <- aov(length ~ domhand+sex+domhand:sex, data=KidsFeet) #Perform the ANOVA
summary(Feet.myaov) #View the ANOVA Results
par(mfrow = c(1,3))
plot(Tooth.myaov, which=1:2)

xyplot( length ~ domhand, data=KidsFeet, type=c("p","a")) 
xyplot( length ~ sex, data=KidsFeet, type=c("p","a"))
xyplot( length ~ sex:domhand, data=KidsFeet, groups=domhand, type=c("p","a"), auto.key=TRUE) 
