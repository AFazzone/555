rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555/Module 3")
getwd()


#1
#Import spreadsheet
fish = read.csv("Hw_3.csv", header = TRUE)
fish


plot(fish$Fish_Meal ,  fish$Mercury, col = "blue", 
     xlab= "Fish Meals Eaten", ylab = "Mercury Levels",
     main = "Mercury in Fish Meals")

#2
# Calculate correlation
cor(fish$Fish_Meal, fish$Mercury)


#3

# Calculate standard deviations
sd.meals <- sd(fish$Fish_Meal)
sd.meals
sd.mercury <- sd(fish$Mercury)
sd.mercury

m.meals <- mean(fish$Fish_Meal)
m.meals

m.mercury <- mean(fish$Mercury)
m.mercury


# Perform Linear Regression.
m <- lm(fish$Mercury ~ fish$Fish_Meal)
m
# Get summary of linear regression.
summary(m)

# Plot scatter plot.
plot(fish$Fish_Meal ,  fish$Mercury, col = "purple", 
     xlab= "Fish Meals Eaten", ylab = "Mercury Levels",
     main = "Mercury in Fish Meals")
# Plot regression line.
abline(m)



#4.)
summary(m)


#5.)
#Calculate Anova Table
anova.table <- anova(m)
anova.table

#Calculate F statistic

qf(p=.05, df1=1, df2=98, lower.tail = TRUE)

pf(93.689, 1, 98)


#Calculate R squared

