devtools::install_github("UrbanInstitute/urbnmapr")

library(tidyverse)
library(urbnmapr)

states_sf <- get_urbn_map("states", sf = TRUE)

states_sf %>% 
  ggplot(aes()) +
  geom_sf(fill = "blue", color = "#ffffff")


rm(list=ls()); cat("\014")
#Set directory
setwd("C:/Users/HP/Documents/555")
getwd()


#1.)
#Import spreadsheet
test1 = read.csv("test.csv", header = TRUE)
test1
attach(test1)


test1 %>% 
  left_join(states, by = "state_name") %>% 
  ggplot(mapping = aes(long, lat, group = group, fill = OD)) +
  geom_polygon(color = "grey30", size = 0.25) +
  coord_map(projection = "albers", lat0 = 39, lat1 = 45)
  
