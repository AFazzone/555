rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()


#1.)
#Import spreadsheet
heart = read.csv("heart.csv", header = TRUE)
as.data.frame(heart)

#Create new variable
heart$temp_level <- ifelse(heart$temp < 98.6,
                        c("0"), c("1"))
heart


#2.)
#Factor Sex

is.factor(heart$sex)
heart$sex = factor(heart$sex)
levels(heart$sex) <- c("Male", "Female")
is.factor(heart$sex)
attach(heart)


aggregate(sex, by=list(temp_level), summary)

#3
#critical value
qnorm(.975)

#sample proportions
pmale <- 14/65
pfemale <- 35/65
both <- 49/130

pmale
pfemale
both

#Risk difference
risk <- pfemale - pmale
risk

z <- (pfemale - pmale)/sqrt((both*(1-both))*(1/65 * 1/65))
z


#4
#make temp_level numeric
temp_level<- as.numeric(temp_level)

#logistic regression
m <- glm(temp_level ~ sex, family=binomial)
m
summary(m)

#odds ratio
odds1 <- (pmale/(1-pmale))/(pfemale/(1-pfemale))
odds1

odds2 <- (pfemale/(1-pfemale))/(pmale/(1-pmale))
odds2

#confidence interval

prop.test(c(14, 35), c(65, 65), conf.level=0.95, correct=FALSE)

#C statistic
library(pROC)

prob <-predict(m, type=c("response"))
g <- roc(temp_level ~ prob)
g



#5

#Multiple Logistic Regression
#logistic regression
m2 <- glm(temp_level ~ sex + Heart_rate, family=binomial)
m2
summary(m2)

#Odds Ratio per 10 unit increase

exp(10 * cbind(OR = coef(m2), confint.default(m2)))


#C statistic
prob <-predict(m2, type=c("response"))
g2 <- roc(temp_level ~ prob)
g2



#6
plot(g)
plot(g2)
roc(temp_level ~ prob, plot=TRUE, legacy.axes=TRUE, main = "Multiple  Logistic Regression",
    percent=TRUE, xlab="False Positive (%)", ylab="True Positive (%)", col="purple", lwd=4)
