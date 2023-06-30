Skills Quiz - Wilcoxon Test

library(ggplot2)
ggplot(cars) +
  geom_dotplot(aes(x=dist))


library(car)
qqPlot(cars$dist)


ggplot(cars) +
  geom_dotplot(aes(x=dist))


ggplot(cars) +
  geom_dotplot(aes(x=dist)) +
  geom_vline(xintercept=100)


wilcox.test(cars$dist, mu = 100, alternative = "two.sided")


ggplot(cars) +
  geom_dotplot(aes(x=dist)) +
  geom_vline(xintercept=100)
cars$dist - 100
wilcox.test(cars$dist, mu = 100, alternative = "two.sided", conf.level = 0.95)
sum(1:50)

library(mosaic)
library(car)
View(Duncan)
Ques.1 <- filter(Duncan, type %in% c("wc", "prof"))
View(Ques.1)
qqPlot(prestige ~ type, data = Duncan)

wilcox.test(prestige ~ type, data = Ques.1, mu = 0, alternative = "greater", conf.level = 0.95)


View(Salaries)
Salary2 <- filter(Salaries, rank == "Prof")
View(Salary2)
library(ggplot2)
#ggplot(Salary2, aes(x= factor(sex), y= salary) + 
#         geom_boxplot(fill = "skyblue", color = "black") +
#         labs(title = "Do Male Professors Earn Higher Median Wage?",
#              x = "Sex",
#              y = "Annual Salary (USD)"))

#ggplot(airquality, aes(x=factor(Month), y=Temp)) +
#  geom_boxplot(fill="skyblue", color="black") +
#  labs(title="La Guardia Airport Daily Mean Temperature", 
#       x="Month of the Year", 
#       y="Daily Mean Temperature")

boxplot(salary ~ sex, data = Salary2,
        ylab="Annual Salary (USD)",
        main ="Do Male Professors Earn Higher Median Wage?",
        col = "wheat")
library(tidyverse)
Salary2 %>%
  group_by(sex) %>%
  summarise(MidSalary = median(salary), 
            sampleSize = n( ))

wilcox.test(salary ~ sex, data = Salary2, mu = 0, alternative = "two.sided", conf.level = 0.95)



view(Davis)
?Davis
MenDavis <- Davis %>%
  filter(sex=="M") %>%
  mutate(Discrepancy = weight - repwt)
view(MenDavis)
ggplot(MenDavis, aes(x=Discrepancy)) +
  geom_histogram(binwidth=2.5, fill="skyblue",color="black")
hist(MenDavis$Discrepancy, col="skyblue", xlab="Positive values mean under-reported weight (Actual - Reported)", main="Differences in Individual's Actual and Reproted Weight (kg)")


wilcox.test(MenDavis$Discrepancy, 
            mu = 0, 
            alternative = "two.sided",
            conf.level = 0.95)



#Assessment Quiz

#Q1
Salary3 <- filter(Salaries, rank == "AsstProf")
Salary3 %>%
  group_by(sex) %>%
  summarise(MidSalary = median(salary), 
            sampleSize = n( ))

boxplot(salary ~ discipline, data = Salaries,
        ylab="Annual Salary (USD)",
        main ="Do Male Professors Earn Higher Median Wage?",
        col = "wheat")

wilcox.test(salary ~ discipline, data = Salaries, mu = 0, alternative = "two.sided", conf.level = 0.95)
