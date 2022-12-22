rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()


#1
#Import spreadsheet
census = read.csv("Census.csv", header = TRUE)
as.data.frame(census)


plot(census$Education,  census$Prestige, col = "blue",
     xlab= "Education", ylab = "Prestige",
     main = "Prestige Score vs. Education")


cor(census$Education, census$Prestige)


# 2


# Perform Linear Regression.
m <- lm(census$Education ~ census$Prestige)
m

resid(m)


plot(census$Prestige, resid(m), 
        ylab="Residuals", xlab="Prestige", 
         main="Residual Plot", col = "purple") 
abline(0,0)


hist(resid(m))

fitted(m)

plot(fitted(m), resid(m), axes = TRUE , frame.plot = TRUE,
     xlab = "Fitted values", ylab = "Residual", col = "purple")


#3.)

m2 <- lm(census$Prestige ~ census$Education + census$Income + census$Women)
m2


#find N 
length(census$Education)
#f critical value
qf(.95,3,98)

summary(m2)

#4.)

confint(m2, level = .95)
anova(m2)
summary(m2)
#find t critical value
qt(.025,98)


#5.)

resid(m2)
fitted(m2)

plot(fitted(m2), resid(m2), axes = TRUE , frame.plot = TRUE,
     xlab = "Fitted values", ylab = "Residual", col = "green")
abline(0,0)

