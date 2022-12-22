rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()


#1
#Import spreadsheet
student = read.csv("student.csv", header = TRUE)
as.data.frame(student)
is.factor(student$group)
student$group = factor(student$group)
is.factor(student$group)
attach(student)

summary(as.factor(student$group))

summary(age)

library(ggplot2) 
library(dplyr)


grp_tbl <- student %>% group_by(group)
grp_tbl

age_tbl <- grp_tbl %>% summarise(mean(age))
age_tbl

iq_tbl <- grp_tbl %>% summarise(mean(iq))
iq_tbl
boxplot(iq ~ group, main="IQ", xlab="group", ylab="iq")
boxplot(age ~ group, main="Age", xlab="group", ylab="age")

ggplot(student, aes(age,iq, colour = group)) + 
  geom_point()



#2


#f critical value
qf(.95,2,42)


m<- aov(iq~group)
summary(m)
 

anova(m)

#Tukey 
TukeyHSD(m)




#3
Physics <- ifelse(student$group=='Physics', 1, 0)
Math <- ifelse(student$group=='Math', 1, 0)
Chemistry <- ifelse(student$group=='Chemistry', 1, 0)



m2 <- lm(iq ~ Physics+ Math, data=student)
summary(m2)
anova(m2)


#4

install.packages("car")
library(car)
Anova(lm(iq ~ group + age), type=3)

# Least square means

install.packages("emmeans")
library(emmeans)

my.model<-lm(iq~group+age,  data = student)
emm_options(contrasts=c("contr.treatment", "contr.poly"))
emmeans(my.model, specs = "group")
