Class Activity - $p$-value

Compute test-statistic, shake things up and see what random can do, what percentage goes beyond our observed test-statistic.

R-code requires four loop. 

Permutation.test <- 
N <- 2000

library(mosaic)
library(car)
boxplot(length ~ sex, data=KidsFeet)
boxplot(sample(length) ~ sex, data=KidsFeet)
boxplot(height ~ sex, data = Davis)
boxplot(sample(height) ~ sex, data = Davis)
boxplot(height ~ sample(sex), data = Davis) 

t.test(length ~ sex, data = KidsFeet)

# Test 1
myTest <- t.test(height ~ sex, data = Davis, mu=0)
observedTestStat <- myTest$statistic
observedTestStat

# Test 2
N <- 2000
permutedTestStats <- rep(NA, N)
for (i in 1:N){
  permutedData <- sample(Davis$height)
  permutedTest <- t.test(permutedData ~ sex, data = Davis, mu=0)
  permutedTestStats[i] <- permutedTest$statistic
}
permutedTestStats

# Test 3
hist(permutedTestStats, col="firebrick", xlim = c(-12,12))
abline(v=observedTestStat, col="black", lwd = 3)

sum(permutedTestStats > observedTestStat)/2000 * 2










# Class Activity 2
# Question 1 on mtcars
view(mtcars)
mtcars2 <- filter(mtcars, cyl %in% c(4,8))
view(mtcars2)
mtcars.t.test <- t.test(wt ~ cyl, data = mtcars2, mu=0)
mtcars.observedTestStat <- mtcars.t.test$statistic
mtcars.observedTestStat

# Test 2
N <- 2000
mtcars.permutedTestStats <- rep(NA, N)
for (i in 1:N){
  mtcars.permutedData <- sample(mtcars2$wt)
  mtcars.permutedTest <- t.test(mtcars.permutedData ~ cyl, data = mtcars2, mu=0)
  mtcars.permutedTestStats[i] <- mtcars.permutedTest$statistic
}
mtcars.permutedTestStats

# Test 3
hist(mtcars.permutedTestStats, col="firebrick", xlim = c(-12,12))
abline(v=mtcars.observedTestStat, col="black", lwd = 3)

sum(mtcars.permutedTestStats > mtcars.observedTestStat)/2000 * 2






# Question 2 on diamonds

table(diamonds$clarity)
view(diamonds)

#Step 1
diamonds.ANOVA <- aov(price ~ clarity, data = diamonds)
diamonds.observedTestStat <- summary(diamonds.ANOVA)[[1]]$`F value`[1]

#Step 2
N <- 100      
diamonds.permutedTestStats <- rep(NA, N)
for (i in  1:N){
  diamonds.permutedData <- sample(diamonds$price)
  diamonds.permutedTest <- aov(diamonds.permutedData ~ clarity, data = diamonds)
  diamonds.permutedTestStats[i] <- summary(diamonds.permutedTest)[[1]]$`F value`[1]
}
diamonds.observedTestStat
hist(diamonds.permutedTestStats, xlim=c(0,220))
abline(v=diamonds.observedTestStat)

#Step 3
sum(diamonds.permutedTestStats >= diamonds.observedTestStat)/N
sum(diamonds.permutedTestStats <= diamonds.observedTestStat)/N








# Question 3 on SAT

view(SAT)
Sat2 <- filter(SAT, sat > 1000)
view(SAT2)
?SAT

#Step 1
sat.glm <- glm(sat > 1000 ~ expend, data = SAT, family=binomial)
summary(sat.glm)
sat.observedTestStat <- summary(sat.glm)[[12]][2,3]
#what is the 12, 2, and 3 doing?
sat.observedTestStat

#Step 2
N <- 100      
sat.permutedTestStats <- rep(NA, N)
for (i in  1:N){
  sat.permutedData <- sample(SAT$sat)
  sat.permutedTest <- glm(sat.permutedData >1000 ~ expend, data = SAT, family = binomial)
  sat.permutedTestStats[i] <- summary(sat.permutedTest)[[12]][2,3]
}
hist(sat.permutedTestStats)
abline(v=sat.observedTestStat)
sat.observedTestStat

#Step 3
sum(sat.permutedTestStats >= sat.observedTestStat)/N
sum(sat.permutedTestStats <= sat.observedTestStat)/N