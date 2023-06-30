library(tidyverse)
library(pander)
library(DT)
library(car)
library(readr)

view(starwars)
?starwars

pander(table(starwars$species))
starwars.human2 <- filter(starwars, species %in% c("Human", "Wookiee"))
t.test(height ~ species, data = starwars.human, mu = 0, alternative = "two.sided", conf.level = 0.95)
qqPlot(starwars.human$species)

pander(table(starwars$homeworld))
starwars.human <- filter(starwars, species == "Human")
wilcox.test(height ~ gender, data = starwars.human, mu = 0, alternative = "two.sided", conf.level = 0.95)

starwars.aov2 <- aov(height ~ as.factor(gender), data = starwars)
summary(starwars.aov2)

starwars.aov <- aov(height ~ as.factor(gender) + as.factor(species) + as.factor(gender):as.factor(species), data = starwars)
summary(starwars.aov)

kruskal.test(height ~ species, data = starwars)
