# Skills Quiz - Chi Squared

glasses <- cbind( Males = c(Glasses = 5, Contacts = 12, None = 18), Females = c(Glasses = 4, Contacts = 14, None = 22))
glasses
barplot(glasses, beside=TRUE, legend.text=TRUE, args.legend=list(x = "topleft", bty="n"))
chis.glasses <- chisq.test(glasses)
chis.glasses$expected 
chis.glasses
chis.glasses$residuals
mean(chis.glasses$expected)
chis.glasses$expected > 1

education <- cbind( `United States` = c(Engineering = 61941, `Natural Science` = 111158, `Social Science` = 182166), `Western Europe` = c(Engineering = 158931, `Natural Science` = 140126, `Social Science` = 116353), Asia = c(280772, 242879, 236018))
education
barplot(education, beside=TRUE, legend.text=TRUE, args.legend=list(x = "topleft", bty="n"))
chis.education <- chisq.test(education)
chis.education$expected 
chis.education
chis.education$residuals
mean(chis.education$expected)
chis.education$observed > 5

?InsectSprays
view(InsectSprays)
InsectSprays.aov <- aov(count ~ spray, data=InsectSprays) 
summary(InsectSprays.aov)
par(which=1:2)
plot(InsectSprays.aov, which=1)
qqPlot(InsectSprays.aov$residuals,
       main = "Q-Q Plot of Residuals",
       id = FALSE)
knitr::kable(favstats(count ~ spray, data = InsectSprays)[,c("spray","mean","sd","n")])











# Assessment Quiz

plot((Wind > 10) ~ Temp, data=airquality, xlab="Daily Average Temperature", ylab="Probability Daily Average Wind Speed (mph) > 10", main="La Guardia Airport Measurements in 1973")

air.glm <- glm(Wind > 10 ~ Temp + as.factor(Month), data=airquality, family=binomial)
summary(air.glm)
