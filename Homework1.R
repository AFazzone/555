rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()

#Import spreadsheet
hospital = read.csv("hw1.csv", header = TRUE)
hospital


Days <- hospital$Days
Days

# 2  Create Histogram

hist(Days,main = "Duration of days in Hospital", col = "blue",breaks = seq(min(Days),max(Days),by =1))

#3
minimum <- min(Days)
minimum
maximum <- max(Days)
maximum 
sd(Days)
summary(Days)
mean(Days)
median(Days)

#4  
#less than 10
pnorm(10,5,3)

#less than3
pnorm(3,5,3)

#between 10 and 3 (10 - 3)
bw_3_10 <- pnorm(10,5,3) - pnorm(3,5,3)
bw_3_10


1-pnorm(439.5,504,111)

pnorm(674,504,111)

qnorm(1-.05)
qni