---
Title:T-tests Make-up Work: 16 October 2019
  Output:
  html_document
theme: cerulean
code_folding: hide
toc: TRUE
toc_float: TRUE
---

library(tidyverse)
library(mosaic)
library(pander)

?CO2
view(CO2)
CO2.chilled.250 <- filter(CO2, Treatment == "chilled" & conc == 250)
mean(CO2.chilled.250$uptake)
CO2.chilled.250 %>%
  group_by(Type) %>%
  summarise(Avg.CO2.uptake = mean(uptake))


library(ggplot2)
stripchart(uptake ~ Type, data=CO2.chilled.250, method = "stack",
           xlab = "Uptake", main = "Quebec Shows Higher Uptake Values Than
           Mississippi when chilled at 250", col = c("blue","red"), pch=15)

t.test(uptake ~ Type, data = CO2.chilled.250, mu = 0, alternative = "two.sided",
       conf.level = 0.95)
library(car)
qqPlot(uptake ~ Type, data=CO2.chilled.250)



CO2 %>%
  group_by(Type) %>% 
  summarise(samplesize = n())
