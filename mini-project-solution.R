rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()

devtools::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(urbnmapr)


states_sf <- get_urbn_map("states", sf = TRUE)
states_sf


#Import spreadsheet
od_data = read.csv("OD_Dataset3.csv", header = TRUE)
od_data

#Create Totals column
od_data$Total <- (od_data$Cocaine + od_data$Heroin + od_data$Opioids)
od_data

attach(od_data)

df1 <- data.frame(od_data)
df1



#Yearly subset for map
d2016 <- subset(df1, df1$Year==2016)
d2017 <- subset(df1, df1$Year==2017)
d2018 <- subset(df1, df1$Year==2018)
d2019 <- subset(df1, df1$Year==2019)
d2020 <- subset(df1, df1$Year==2020)
d2021 <- subset(df1, df1$Year==2021)
d2022 <- subset(df1, df1$Year==2022)


#Plot Yearly on map
d2016 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total)) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2016 Deaths")

d2017 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total)) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2017 Deaths")

d2018 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total)) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2018 Deaths")


d2019 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total)) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2019 Deaths")

d2020 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total)) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2020 Deaths")

d2021 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total)) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2021 Deaths")

#2022 has is less because there is no November & December
d2022 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat,group = group, fill = Total, main = "Overdoeses 2022")) +
  geom_polygon(color = "blue", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)+
  labs(fill = "2022 Deaths (Thousands)")


library(rgl)

#Line graph of each drug variable
mycolors <- c('royalblue1', 'darkcyan', 'oldlace')
data$color <- mycolors[State]
data$color


grp_year <- od_data %>% group_by(Year)  %>% summarise(cocaine = sum(Cocaine), 
opioids = sum(Opioids), heroin = sum(Heroin))

op <- as.numeric(grp_year$opioids)
co <- as.numeric(grp_year$cocaine)
he <- as.numeric(grp_year$heroin)
he

#Histogram of each variable
hist(op, main = "Histogram of Opioids")
hist(co, main = "Histogram of Cocaine")
hist(he, main = "Histogram of Heroin")


#Line graph of each drug variable
library(reshape2)

df2 <- melt(grp_year ,  id.vars = 'Year', variable.name = 'deaths')
df2
ggplot(subset(df2, df2$Year<2022), aes(Year, value/1000)) +
  geom_line(aes(colour = deaths))

#examine correlation

plot(Opioids, Heroin, col = "blue",
     xlab= "Opioids", ylab = "Heroin",
     main = "Opioids vs. Heroin")


cor(Cocaine, Heroin)
cor(Cocaine, Opioids)
cor(Opioids, Heroin)


install.packages("ggplot2")            
install.packages("GGally")

library("ggplot2")                     
library("GGally")   
ggpairs(od_data, columns=3:5)  



#Boxplot of each variable  
boxplot(Opioids, Heroin, Cocaine, main="Deaths",xlab="Opioids             Herion            Cocaine", ylab="Deaths")


#Compare sample means
mean(Cocaine)
mean(Heroin)
mean(Opioids)

sd(Cocaine)
sd(Heroin)
sd(Opioids)

n <- length(Opioids)
n
t1 = (mean(Opioids)-mean(Heroin))/(sd(Heroin/sqrt(n)))
t1
qt(.90,313)
#One Sided
#Find T critical Value
#Find F critical Value
qt(.95,313)
qf(.95,1,313)
confint(m, level = .95)



# Perform Linear Regression
m <- lm(Opioids ~ Heroin)
summary(m)




#Multiple Linear Regression :Analyze different attributes against total
m2 <- lm(Total~ Heroin + Cocaine)
summary(m2)

m3 <- lm(Total~ Heroin + Opioids)
summary(m3)


m4 <- lm(Total~ Cocaine + Opioids)
summary(m4)


# Perform Anova
anova(m)

#Predict Totals for 2022
d2022$predict <- predict(m, d2022)
d2022


