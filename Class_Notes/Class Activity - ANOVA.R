Class Activity - ANOVA

library(car)
library(tidyverse)
library(mosaic)
view(Blackmore)
?Blackmore

?cut
Blackmore$ageGroup <- cut(Blackmore$age, c(7,10,12,14,18), labels=c("Youth","Pre-Teen","Early-Teen","Late-Teen"))

# This is Tidyverses version of adding a column
# cut() creates a new "group" by the way I declare. 7-10 is YOUTH, 10-12 is PRE-TEEN, 12-14 is EARLY-TEEN, 14-18 is LATE-TEEN

#Blackmore <- Blackmore %>%
#  mutate(ageGroup = cut(age, c(7,10,12,14,18), labels=c("Youth","Pre-Teen", "Early-Teen", #"Late-Teen"), ordered_result = TRUE))

Blackmore.myaov <- aov(exercise ~ group + ageGroup + group:ageGroup, data=Blackmore)
summary(Blackmore.myaov)

xyplot(exercise ~ group, data=Blackmore, type=c("p","a"), main="Significance of Age", col='gray')
xyplot(exercise ~ ageGroup, data=Blackmore, type=c("p","a"), main="Significance of Age Group", col='gray')
xyplot(exercise ~ ageGroup, data=Blackmore, groups=group, type=c("p","a"), main="Significance of Age's and Age Group's Interaction", auto.key=list(corner=c(1,1)))

#The conclusions are that age (p= 1.27e-11) and age group (p= 2e-16) have a significant factor on the amount of exercise an individual will do. The interaction of what age and age group an individual is a part of also holds significant value (p= 1.30e-09).

par(mfrow = c(1,3))
plot(Blackmore.myaov,
     which = 1)
qqPlot(Blackmore.myaov$residuals,
       main = "Q-Q Plot of Residuals",
       id = FALSE)
plot(Blackmore.myaov$residuals, 
     main = "Residuals vs Order")

# It looks like assumption #3 has an unconstant variance. In other words, the variance of the error terms are not constant. Assumption #2 looks like the error terms are not distributed normally and that data is heavy-tailed.








