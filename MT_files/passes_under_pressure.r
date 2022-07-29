library(tidyverse) 
library(StatsBombR) 
library(SBpitch)
library(gganimate)
library(magick)
library(gridExtra)


#We will use the UEFA Euro which is competition_id == 55 and season_id == 43
Comp<-FreeCompetitions()%>%filter(competition_id==55, season_name=="2020")
#UEFA has 51 games
Matches <- FreeMatches(Comp)
#Free events
StatsBombData<-StatsBombFreeEvents(MatchesDF = Matches, Parallel = T)
StatsBombData = allclean(StatsBombData)
StatsBombData <- get.opposingteam(StatsBombData)

#data360

data360 <- StatsBombFree360Events(MatchesDF = Matches, Parallel = T)

lineups <- StatsBombFreeLineups(MatchesDF = Matches, Parallel = T)


#Then, we need to join the 360 frames to the respective events in the standard data, so we have both the standard event data and the 360 freeze-frames in one dataframe.

data360 = data360 %>% rename(id = event_uuid)



#Passes under pressure


###################### Italy ###################### 

##### Passes in each game
#3795506  Italy vs England - Finals
#3788741 Italy vs Turkey
#3788754 Italy vs Switzerland
#3788766 Italy vs Wales
#3794685 Italy vs Austria
#3795107  Italy vs Belgium
#3795220  Italy vs Spain - semi finals


Pup_italy<-StatsBombData%>%
  filter(match_id == 3795506, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns<-Pup_italy%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_1<-sum(Pup_italy_succ_uns$n)

#Only complete passes

Pup_italy_succ<-Pup_italy%>%
  distinct(Pup_italy_succ_uns$n[1])


#plot
p1 <- create_Pitch()+
  geom_segment(data = Pup_italy, aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs England (Final)",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ)))




p1a<-create_Pitch()+
  geom_segment(data = Pup_italy, aes(x = location.x, y = location.y,
                                     xend = pass.end_location.x, yend = pass.end_location.y,
                                     colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs England (Final)",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ)))+
  transition_time(minute)


animate(p2a,renderer = magick_renderer())







#Passes under pressure 

Pup_italy_turkey<-StatsBombData%>%
  filter(match_id == 3788741, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns_turkey<-Pup_italy_turkey%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_turkey_1<-sum(Pup_italy_succ_uns_turkey$n)

#Only complete passes

Pup_italy_succ_turkey<-Pup_italy_turkey%>%
  distinct(Pup_italy_succ_uns_turkey$n[1])


#plot
p2 <-create_Pitch()+
  geom_segment(data = Pup_italy_turkey, aes(x = location.x, y = location.y,
                                            xend = pass.end_location.x, yend = pass.end_location.y,
                                            colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs Turkey (Group Stage)",
       subtitle = "Quality Passes Italy under pressure ")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_turkey_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ_turkey)))



#Passes under pressure 

Pup_italy_switz<-StatsBombData%>%
  filter(match_id == 3788754, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns_switz<-Pup_italy_switz%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_switz_1<-sum(Pup_italy_succ_uns_switz$n)

#Only complete passes

Pup_italy_succ_switz<-Pup_italy_switz%>%
  distinct(Pup_italy_succ_uns_switz$n[1])


#plot
p3 <-create_Pitch()+
  geom_segment(data = Pup_italy_switz, aes(x = location.x, y = location.y,
                                           xend = pass.end_location.x, yend = pass.end_location.y,
                                           colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs Switzerland",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_switz_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ_switz)))



#Passes under pressure 

Pup_italy_Wales<-StatsBombData%>%
  filter(match_id == 3788766, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns_Wales<-Pup_italy_Wales%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_Wales_1<-sum(Pup_italy_succ_uns_Wales$n)

#Only complete passes

Pup_italy_succ_Wales<-Pup_italy_Wales%>%
  distinct(Pup_italy_succ_uns_Wales$n[1])


#plot
p4 <-create_Pitch()+
  geom_segment(data = Pup_italy_Wales, aes(x = location.x, y = location.y,
                                           xend = pass.end_location.x, yend = pass.end_location.y,
                                           colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs Wales (group Stage)",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_Wales_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ_Wales)))


#Passes under pressure 

Pup_italy_Austria<-StatsBombData%>%
  filter(match_id == 3794685, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns_Austria<-Pup_italy_Austria%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_Austria_1<-sum(Pup_italy_succ_uns_Austria$n)

#Only complete passes

Pup_italy_succ_Austria<-Pup_italy_Austria%>%
  distinct(Pup_italy_succ_uns_Austria$n[1])


#plot
p5 <-create_Pitch()+
  geom_segment(data = Pup_italy_Austria, aes(x = location.x, y = location.y,
                                             xend = pass.end_location.x, yend = pass.end_location.y,
                                             colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs Austria (Round of 16)",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_Austria_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ_Austria)))


#Passes under pressure 

Pup_italy_Belgium<-StatsBombData%>%
  filter(match_id == 3795107, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns_Belgium<-Pup_italy_Belgium%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_Belgium_1<-sum(Pup_italy_succ_uns_Belgium$n)

#Only complete passes

Pup_italy_succ_Belgium<-Pup_italy_Belgium%>%
  distinct(Pup_italy_succ_uns_Belgium$n[1])


#plot
p6 <-create_Pitch()+
  geom_segment(data = Pup_italy_Belgium, aes(x = location.x, y = location.y,
                                             xend = pass.end_location.x, yend = pass.end_location.y,
                                             colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs Belgium (Quarter-finals)",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_Belgium_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ_Belgium)))





#Passes under pressure 

Pup_italy_Spain<-StatsBombData%>%
  filter(match_id == 3795220, type.name == "Pass" &is.na(pass.type.name), team.name == "Italy")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_italy_succ_uns_Spain<-Pup_italy_Spain%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_italy_succ_uns_Spain_1<-sum(Pup_italy_succ_uns_Spain$n)

#Only complete passes

Pup_italy_succ_Spain<-Pup_italy_Spain%>%
  distinct(Pup_italy_succ_uns_Spain$n[1])


#plot
p7 <-create_Pitch()+
  geom_segment(data = Pup_italy_Spain, aes(x = location.x, y = location.y,
                                           xend = pass.end_location.x, yend = pass.end_location.y,
                                           colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "Italy vs Spain (Semi-finals)",
       subtitle = "Quality Passes Italy under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_italy_succ_uns_Spain_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_italy_succ_Spain)))




Combined<-grid.arrange(p1,p6,p7, ncol = 3, nrow = 1)

Combined<-grid.arrange(p2,p3,p4,p5, ncol = 2, nrow = 2)




ggsave(plot=Combined, filename="myPlot.png",width = 7,height = 14)

getwd()

class(p1)


###################### England ###################### 

England_sub= StatsBombData_Matches %>% 
  select('match_id', 'team.name','OpposingTeam','home_score','away_score','competition_stage.name','referee.country.name')%>% 
  filter(team.name=='England') %>%
  distinct()




#Passes under pressure 

Pup_England_Ukraine<-StatsBombData%>%
  filter(match_id == 3795187, type.name == "Pass" &is.na(pass.type.name), team.name == "England")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_England_succ_uns_Ukraine<-Pup_England_Ukraine%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_England_succ_uns_Ukraine_1<-sum(Pup_England_succ_uns_Ukraine$n)

#Only complete passes

Pup_England_succ_Ukraine<-Pup_England_Ukraine%>%
  distinct(Pup_England_succ_uns_Ukraine$n[1])


#plot
p8 <-create_Pitch()+
  geom_segment(data = Pup_England_Ukraine, aes(x = location.x, y = location.y,
                                               xend = pass.end_location.x, yend = pass.end_location.y,
                                               colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "England vs Ukraine (Quarter-finals)",
       subtitle = "Quality Passes England under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_England_succ_uns_Ukraine_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_England_succ_Ukraine)))






#Passes under pressure 

Pup_England_Denmark<-StatsBombData%>%
  filter(match_id == 3795221, type.name == "Pass" &is.na(pass.type.name), team.name == "England")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_England_succ_uns_Denmark<-Pup_England_Denmark%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_England_succ_uns_Denmark_1<-sum(Pup_England_succ_uns_Denmark$n)

#Only complete passes

Pup_England_succ_Denmark<-Pup_England_Denmark%>%
  distinct(Pup_England_succ_uns_Denmark$n[1])


#plot
p9 <-create_Pitch()+
  geom_segment(data = Pup_England_Denmark, aes(x = location.x, y = location.y,
                                               xend = pass.end_location.x, yend = pass.end_location.y,
                                               colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "England vs Denmark (Semi-finals)",
       subtitle = "Quality Passes England under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_England_succ_uns_Denmark_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_England_succ_Denmark)))



#Passes under pressure 

Pup_England_Italy<-StatsBombData%>%
  filter(match_id == 3795506, type.name == "Pass" &is.na(pass.type.name), team.name == "England")%>%
  filter(under_pressure == "TRUE")%>%
  mutate(pass.outcome = as.factor(if_else(is.na(pass.outcome.name), "Complete", "Incomplete")))


#The code to find successful and unsuccessful passes:

Pup_England_succ_uns_Italy<-Pup_England_Italy%>%
  filter(type.name == "Pass")%>%
  group_by(pass.outcome)%>%
  tally()

# creates  a new function, we then sum() the 'passes' data frame with $n specifying the n column.

Pup_England_succ_uns_Italy_1<-sum(Pup_England_succ_uns_Italy$n)

#Only complete passes

Pup_England_succ_Italy<-Pup_England_Italy%>%
  distinct(Pup_England_succ_uns_Italy$n[1])


#plot
p10 <-create_Pitch()+
  geom_segment(data = Pup_England_Italy, aes(x = location.x, y = location.y,
                                             xend = pass.end_location.x, yend = pass.end_location.y,
                                             colour = pass.outcome),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches")))+
  
  scale_y_reverse()+
  coord_fixed(ratio = 105/100)+
  labs(title = "England vs Italy (Finals)",
       subtitle = "Quality Passes England under pressure")+
  theme(legend.position = "bottom")+
  geom_text(aes(x = 3, y=-4, label = paste0("Passes: ", Pup_England_succ_uns_Italy_1)))+
  geom_text(aes(x = 3, y=-1,label = paste0("Complete: ",Pup_England_succ_Italy)))








library(gridExtra)
#combine(p1, p2, ncol = 1, nrow = 2)

Combined<-grid.arrange(p1,p6,p7, ncol = 3, nrow = 1)
Combined<-grid.arrange(p8,p9,p10, ncol = 3, nrow = 1)
Combined<-grid.arrange(p1,p10, ncol = 1, nrow = 2)

Combined<-grid.arrange(p2,p3,p4,p5, ncol = 2, nrow = 2)


p6

ggsave(plot=Combined, filename="myPlot.png",width = 7,height = 14)

getwd()










