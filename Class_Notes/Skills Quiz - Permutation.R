set.seed(1140411)
sample1 <- rnorm(30, 69, 2.5)
sample2 <- rnorm(30, 69, 2.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30))
View(theData)
boxplot(values ~ group, data = theData)

library(mosiac)
theData.t.test <- t.test(values ~ group, data=theData, mu = 0, alternative="two.sided", conf.level=0.95)
theData.t.test
favstats(theData, mean=(), sd=())
mean(theData$values)
sd(theData$values)
n(theData$values)
table(theData)
t.test( ~ X, data = YourData, mu = YourNull, alternative = YourAlternative, conf.level = 0.95)








# Create the data:
set.seed(1140411)
sample1 <- rnorm(30, 69, 2.5)
sample2 <- rnorm(30, 69, 2.5)
theData <- data.frame(values = c(sample1,sample2), group = rep(c(1,2), each=30))
View(theData)
boxplot(values ~ group, data = theData)



# Run the permutation test:

myTest <-  t.test(values~group, data = theData, mu=0)
observedTestStat <- myTest$statistic
observedTestStat


N <- 2000      
permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  permutedData <- sample(x=theData$group)
  permutedTest <- t.test(values~permutedData, data=theData, mu=0)
  permutedTestStats[i]  <-  permutedTest$statistic
}
observedTestStat
hist(permutedTestStats)
abline(v=observedTestStat)
sum(permutedTestStats > observedTestStat)/N
sum(permutedTestStats < observedTestStat)/N
2*sum(permutedTestStats <= observedTestStat)/N










# Create the data:
set.seed(121)
sample3 <- rnorm(30, 185, 8)
sample4 <- sample3 - rnorm(30, 0, 3.5)
theData9 <- data.frame(values = c(sample3,sample4), group = rep(c(1,2), each=30), id = rep(c(1:30),times=2))
data3 <- filter(theData9, group==1)
data4 <- filter(theData9, group==2)
View(theData9)
with(theData9, hist(values[group==1] - values[group==2]))


# Perform the permutation test:

#data.t.tests <- t.test(data3$values, data4$values, paired = TRUE, mu = 0, alternative = "two.sided", conf.level = 0.95)
data.t.test <- t.test(values ~ group, data = theData9, paired = TRUE, mu=0)
data3 %>% 
  summarise(aveLength = mean(values), 
            sdLength = sd(values), 
            sampleSize = n( ))
data.observedTestStat <- data.t.tests$statistic
data.observedTestStat


N <- 2000      
data.permutedTestStats <-  rep(NA, N)
for  (i in 1:N ) {
  data.permutedData <- sample(x=c(1,-1), size=30, replace=TRUE)
  data.permutedTest <- with (theData9, t.test(data.permutedData*(values[group==1]- values[group==2]), mu=0))
  data.permutedTestStats[i]  <-  data.permutedTest$statistic
}
hist(data.permutedTestStats)
abline(v=data.observedTestStat)
sum(data.permutedTestStats >= data.observedTestStat)/N
sum(data.permutedTestStats <= data.observedTestStat)/N
2*sum(data.permutedTestStats <= data.observedTestStat)/N











# Question 5

library(mosaic)

?SaratogaHouses

View(SaratogaHouses)

table(SaratogaHouses$fuel)

kruskal.test(price ~ fuel, data = SaratogaHouses)

boxplot(price ~ fuel, data=SaratogaHouses)

SaratogaHouses %>% 
  group_by(fuel) %>% 
  summarise(medprice = median(price))








#Question 6
library(mosaic)
library(pander)
library(tidyverse)
?ToothGrowth
View(ToothGrowth)
xyplot(len ~ dose, groups=supp, data=ToothGrowth, type=c("p","a"), auto.key=TRUE)
as.factor()

Tooth.myaov <- aov(len ~ as.factor(dose)+supp+as.factor(dose):supp, data=ToothGrowth) #Perform the ANOVA
summary(Tooth.myaov) #View the ANOVA Results
par(mfrow = c(1,3))
plot(Tooth.myaov, which=1:2)

?RailTrail
View(RailTrail)
boxplot(cloudcover ~ weekday, data=RailTrail, names=c("Weekend/Holiday", "Weekday"), ylab="Cloud Cover Measurement (in oktas)")



t.test(cloudcover ~ weekday, data = RailTrail, mu = 0, alternative = "two.sided", conf.level = 0.95)
pander(summary(Rail.t.test))
car::qqPlot(cloudcover ~ weekday, data = RailTrail)
