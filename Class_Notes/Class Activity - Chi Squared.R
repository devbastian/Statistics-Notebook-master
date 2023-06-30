mytable <- rbind(A=c(R=14, S=20, T=54), B=c(R=10, S=40, T=37))
mytable
my.chisq <- chisq.test(mytable)
my.chisq
my.chisq$expected
barplot(mytable, beside=TRUE, legend.text=TRUE)
barplot(my.chisq$expected, beside=TRUE, legend.text=TRUE)
my.chisq$residuals
barplot(my.chisq$residuals, beside=TRUE, legend.text=TRUE)


mytab <- HairEyeColot[,,"Male"] + HairEyeColor[,,"Female"]
mytab
barplot(mytab, beside=TRUE)
barplot(mytab, beside=TRUE, legend.text=TRUE)
barplot(t(mytab), beside=TRUE, legend.text=TRUE)


Three interpretations of chi squared.
Most common, least common, and from residuals what was most different from expected.


#Class Activity we didn't get to
qs <- read.csv("http://byuimath.com/saunderspractice/ghsaund/qs.csv", header=TRUE)

survey <- read.csv("http://byuimath.com/saunderspractice/ghsaund/survey.csv", header=TRUE)
survey <- filter(survey, Group== "Fall 2016")
view(survey)
survey.chisq <- chisq.test(survey)
my.chisq
my.chisq$expected
barplot(mytable, beside=TRUE, legend.text=TRUE)
barplot(my.chisq$expected, beside=TRUE, legend.text=TRUE)
my.chisq$residuals
barplot(my.chisq$residuals, beside=TRUE, legend.text=TRUE)