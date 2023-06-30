---
  Title: 11 October 2019 - Make-up Work
  Output:
    html_document
    theme: cerulean
  code_folding: hide
  toc: TRUE
  toc_float: TRUE
---

<!-- This is the Skills Quiz - Data Wrangling and Visualization -->  

library(mosaic)
View(KidsFeet)  
?KidsFeet
table(KidsFeet$sex)
table(KidsFeet$biggerfoot)
table(KidsFeet$birthmonth)
barplot(KidsFeet$birthmonth)


<!-- I am not understanding how to write/use histograms and bar plots. -->
table(KidsFeet$sex, KidsFeet$domhand)
<!-- How do you make this a pretty table? -->
<!-- What is the probability of a Type I and Type II error? 
      Cause I have no idea. Never show a graph without having a p-value.-->

  
  
  library(tidyverse)
  KidsFeet %>%
  group_by(sex) %>%
  summarise(min = min(length), 
            Q1 = quantile(length, 0.25),
            med = median(length), 
            Q3 = quantile(length, 0.75), 
            max = max(length),
            sd_Length = sd(length),
            Sample_Size = n())

  
  
  ?airquality
View(airquality) 
library(mosaic)
table(airquality$Temp, airquality$Month)
airquality %>%
  group_by(Month) %>%
  summarise(Avg_Temp = mean(Temp))

83.9677-83.9032
65.5484-79.100
79.100-83.9032
83.9032-83.9677
83.9677-76.9
<!-- How do you automate this process?-->
  
  
  
  library(mosaicData)
View(Riders)
Riders %>%
  group_by(day) %>%
  summarise(Total = sum(riders))




?mtcars
View(mtcars)
mtcars %>%
  group_by(cyl) %>%
  summarise(Total = sum(cyl))

#This code didn't work properly. Why?
#mtcars %>%
#  group_by(cyl, c(4)) %>%
#  summarise(Transmission = sum(am))
Only_Cyl4 <- filter(mtcars, cyl == 4)
Only_Cyl4 %>%
  sum(am)
