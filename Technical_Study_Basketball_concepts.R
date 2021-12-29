#call the necessary libraries
library(tidyverse)
library(dplyr)

## adjust working directory

#setwd("C:/Users/siyah/Desktop/Fall2021_GSU")

##load the source file and take a look at the general information

shots_data=read.csv(file='shots_data.csv',header = T)
view(shots_data)
str(shots_data)
summary(shots_data)

###calculate the distance of each shot's location to the center of the hoop based on the definition
###and determine the shot zones based on the created variable distance and y coordinates
shots_data_derived=shots_data %>%
  mutate(dist_to_hoop= if_else(y>7.8, sqrt(x^2+y^2),abs(x))) %>%
  mutate(shot_zone=if_else((y>7.8 & dist_to_hoop>23.75),"NC3",if_else((y<=7.8 & dist_to_hoop>22),"C3","2PT")))


###calculate shot distribution for each team

shot_distribution=shots_data_derived %>%
  group_by(team,shot_zone) %>%
  summarise(n=n()) %>%
  group_by(team) %>%
  mutate(sum= sum(n))%>%
  group_by(team,shot_zone) %>%
  mutate(perc=round(n/sum,3))
view(shot_distribution)
  

###calculate eFG% for each zone and for each team


team_metrics=shots_data_derived %>%
  group_by(team,shot_zone) %>%
  mutate(FGM=sum(fgmade))%>%
  group_by(team,shot_zone,FGM) %>%
  summarise(FGA=n())%>%
  group_by(team)%>%
  ###zone specific eFG%
  mutate(zone_eFG=round(if_else(shot_zone=="2PT",FGM/FGA,FGM*1.5/FGA),3))
view(team_metrics)

###---------- COMMENT---------####
###Eventhough team B has a less chance to try 2 pts, they win with their high percentage performance within 2PT shot zone.


####overall eFG% for each team
team_eFG= team_metrics %>%
  group_by(team)%>%
  ####calculate the numerator part for eFG as eFGM
  mutate(eFGM=sum(if_else(shot_zone=="2PT",FGM*1,FGM*1.5)))%>%
  ####calculate the all attempts for each team
  mutate(sum_FGA=sum(FGA)) %>%
  mutate(eFG=round(eFGM/sum_FGA,3))%>%
  select(team,eFGM,sum_FGA,eFG,) %>%
  distinct_all()
view(team_eFG) 

###---------- COMMENT---------####
#### eFG% is more reliable metric in evaluating team's performances.
  
