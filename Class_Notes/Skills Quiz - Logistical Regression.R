Skills quiz - Logistical Regression

View(infert)
?infert
infert.glm <- glm( (spontaneous > 0) ~ age, data=infert, family=binomial)
summary(infert.glm)
plot( (spontaneous > 0) ~ age, data=infert)
curve( exp(1.48706 + -0.05616*x)/(1 + exp(1.48706 + -0.05616*x)), add=TRUE)
table(infert$age)
pchisq(334.01, 246, lower.tail=FALSE)


library(mosaic)
View(Galton)
?Galton
Galton.glm <- glm(sex == "M" ~ height, data = Galton, family=binomial)
summary(Galton.glm)
plot(sex == "M" ~ height, data= Galton)
curve(exp(-52.98422 + 0.79683*x)/(1 + exp(-52.98422 + 0.79683*x)), add = TRUE)
exp(-52.98422)
exp(0.79683)      
exp(-52.98422 + 0.79683*60)/(1 + exp(-52.98422 + 0.79683*60))
exp(-52.98422 + 0.79683*70)/(1 + exp(-52.98422 + 0.79683*70))
exp(-52.98422 + 0.79683*66.5)/(1 + exp(-52.98422 + 0.79683*66.5))
exp(-52.98422 + 0.79683*65)/(1 + exp(-52.98422 + 0.79683*65))
install.packages(ResourceSelection)
library(ResourceSelection)
hoslem.test(Galton.glm$y, Galton.glm$fitted, g=10)
