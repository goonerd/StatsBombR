#install.packages('digest', repos='http://cran.us.r-project.org')
#install.packages("dplyr") 
library(SBpitch)
library(dplyr)
library(tidyverse)
library(StatsBombR)
library(ggplot2)


#Pull in Data for La Liga 2005-06 season 
Comp <- FreeCompetitions() %>%
  filter(competition_id==11 & season_name=="2005/2006") 
Matches <- FreeMatches(Comp) 
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T) 
StatsBombData = allclean(StatsBombData) 

#Create shots and goals dataframe

shots_goals = StatsBombData %>%
  group_by(team.name) %>% 
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE),
            goals = sum(shot.outcome.name=="Goal", na.rm = TRUE))


shots_goals = StatsBombData %>%
  group_by(team.name) %>%
  summarise(shots = sum(type.name=="Shot", na.rm =
                          TRUE)/n_distinct(match_id),
            goals = sum(shot.outcome.name=="Goal", na.rm =
                          TRUE)/n_distinct(match_id))

						  
#Plot shots and goals chart

ggplot(data = shots_goals, aes(x = reorder(team.name, shots), y = shots)) + geom_bar(stat = "identity", width = 0.5) + labs(y="Shots") + theme(axis.title.y = element_blank()) + scale_y_continuous( expand = c(0,0)) + coord_flip() + theme_SB() 


#Create table for player shots per 90

player_shots = StatsBombData %>%
  group_by(player.name, player.id) %>%
  summarise(shots = sum(type.name=="Shot", na.rm = TRUE)) 
player_minutes = get.minutesplayed(StatsBombData) 
player_minutes = player_minutes %>%
  group_by(player.id) %>%
  summarise(minutes = sum(MinutesPlayed)) 
player_shots = left_join(player_shots, player_minutes) 
player_shots = player_shots %>% mutate(nineties = minutes/90) 
player_shots = player_shots %>% mutate(shots_per90 = shots/nineties) 


#One time only: Install SBpitch from FCrSTATS

#devtools::install_github("FCrSTATS/SBpitch")

#load passes for Messi
passes = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.type.name) & player.id==5503)  %>%
  filter(pass.end_location.x>=102 & pass.end_location.y<=62 & pass.end_location.y>=18) 

#plot passes on pitch
create_Pitch() +  geom_segment(data = passes, aes(x = location.x, y = location.y,
                                  xend = pass.end_location.x, yend = pass.end_location.y),
               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +  labs(title = "Lionel Messi, Completed Box Passes", subtitle = "La Liga, 2005/2006") + coord_fixed(ratio = 105/100) 
