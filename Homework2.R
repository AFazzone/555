rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()


#1
#Import spreadsheet
yes = read.csv("hw2_yes.csv", header = TRUE)
no = read.csv("hw2_no.csv", header = TRUE)

participants <- yes$ï..Yes
non_participants <-no$ï..No

participants
non_participants


hist(non_participants, col = "green")
hist(participants, col = "blue")

summary(participants)
summary(non_participants)

mean(participants)
sd(participants)
length(participants)


mean(non_participants)
sd(non_participants)
length(non_participants)

#2
#Critical Value
qt(1-(.05/2),24)

#Calculate  T Statistic
u = mean(participants)
s = sd(participants)
n = length(participants)
t = (425-u)/(s/sqrt(n))
t

#Calculate P Value
2*pt(-t,n-1)


#3  Calculate 90% confidence interval
# The sample size is less than 30, so we perform t-test instead z-test.
x.bar <- 410.1
sd <- 121.5138
n <- 25
# We need to use degree of freedom sample size - 1
df <- n - 1 
t <- qt(1-(0.1/2), df)
t

# Calculate lower confidence interval.
lower.interval <- x.bar - t * (sd / sqrt(n))
# Calculate higher confidence interval.
upper.interval <- x.bar + t * (sd / sqrt(n))
lower.interval
upper.interval


t.test(participants, alternative = "two.sided", conf.level = .9)

# 4.)

# Perform t test for two sample test.
x.bar1 <- 374.1
x.bar2 <- 410.1
n1 <- 22
n2 <- 25
sd1 <- 133.1393
sd2 <- 121.5138
# Calculate t statistic
t <- (x.bar1 - x.bar2) / sqrt((sd1**2/n1) + (sd2**2/n2))
t

t.test(non_participants, participants, alternative = "greater", conf.level = .95)
