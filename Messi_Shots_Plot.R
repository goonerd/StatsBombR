library(SBpitch)
library(dplyr)
library(tidyverse)
library(StatsBombR)
library(ggplot2)

Comp <- FreeCompetitions() %>%
  filter(competition_id==11 & season_name=="2010/2011") 

Matches <- FreeMatches(Comp) 
StatsBombData <- StatsBombFreeEvents(MatchesDF = Matches, Parallel = T) 
StatsBombData = allclean(StatsBombData) 

passes_Xavi = StatsBombData %>%
  filter(type.name=="Pass" & is.na(pass.type.name) & player.id==20131)  

create_Pitch() +  geom_segment(data = passes_Xavi, aes(x = location.x, y = location.y,
                                                  xend = pass.end_location.x, yend = pass.end_location.y),
                               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +  labs(title = "Xavi, all passes", subtitle = "La Liga, 2010/2011") + coord_fixed(ratio = 105/100) 

shots_Messi = StatsBombData %>%
  filter(type.name=="Shot" & player.id==5503) 


create_Pitch() +  geom_segment(data = shots_Messi, aes(x = location.x, y = location.y,
                                                       xend = shot.end_location.x, yend = shot.end_location.y),
                               lineend = "round", size = 0.6, arrow = arrow(length = unit(0.08, "inches"))) +  labs(title = "Messi, all shots", subtitle = "La Liga, 2010/2011") + coord_fixed(ratio = 105/100) 
