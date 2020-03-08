library(StatsBombR)
require(dplyr); require(ggplot2)
## select just the shots from that match 
#shotData <- fifa_wc_data_claen %>% filter(match_id == 8658 & shot.statsbomb_xg > 0)

comps <- FreeCompetitions()

fifa_wc_matches <- comps %>% 
  filter(competition_id == 43) %>% 
  FreeMatches()

fifa_wc_data_raw <- StatsBombFreeEvents(MatchesDF = fifa_wc_matches)

#fifa_wc_data_clean <- fifa_wc_data_raw %>% 
  #allclean() %>%  
  #left_join(comps %>% select(season_id, season_name), by = "season_id")

fifa_wc_data_clean <- fifa_wc_data_raw %>% allclean()

## select just the shots from that match 
shotData <- fifa_wc_data_clean %>% filter(match_id == 8658 & shot.statsbomb_xg > 0)

## create a more precise metric for 'time' which combines minutes and seconds 
shotData$Timer <- shotData$minute + (shotData$second / 60)

## drop all unneeed data to make it easier to work with the data 
#shotData <- shotData[c(19,54,61,147)]


## Create a dataframe just for the Team 1 and Team 2
Croatia_shotData <- shotData %>% filter(team.name == unique(shotData$team.name)[1])
France_shotData <- shotData %>% filter(team.name == unique(shotData$team.name)[2])

Croatia_shotData$xg_total <- cumsum(Croatia_shotData$shot.statsbomb_xg)
France_shotData$xg_total <- cumsum(France_shotData$shot.statsbomb_xg)

# create the starting and ending rows of data to ensure that all teams start on 0 and end at 96 minutes 
Top2add <- data.frame(team.name = unique(shotData$team.name)[1], shot.statsbomb_xg = 0, Timer = 0, shot.outcome.name = "-", xg_total = 0, stringsAsFactors = F)
Bottom2add <- data.frame(team.name = unique(shotData$team.name)[1], shot.statsbomb_xg = 0, Timer = 96,  shot.outcome.name = "-", xg_total = max(Croatia_shotData$xg_total), stringsAsFactors = F)

## add these rows to the data with bind_rows()
Croatia_shotData <- bind_rows(Croatia_shotData, Top2add)
Croatia_shotData <- bind_rows(Croatia_shotData, Bottom2add)


# create the starting and ending rows of data to ensure that all teams start on 0 and end at 96 minutes 
Top2add <- data.frame(team.name = unique(shotData$team.name)[2], shot.statsbomb_xg = 0, Timer = 0, shot.outcome.name = "-", xg_total = 0, stringsAsFactors = F)
Bottom2add <- data.frame(team.name = unique(shotData$team.name)[2], shot.statsbomb_xg = 0, Timer = 96,  shot.outcome.name = "-", xg_total = max(France_shotData$xg_total), stringsAsFactors = F)

## add these rows to the data with bind_rows()
France_shotData <- bind_rows(France_shotData, Top2add)
France_shotData <- bind_rows(France_shotData, Bottom2add)

grep("Timer", colnames(Croatia_shotData))

##Collect: xg_total, shot.outcome.name, other 2 from tutorial only
Croatia_shotData[xg_total]

p <- ggplot()

p

#Plot Team1 Data
p <- p + geom_step(data=Croatia_shotData, mapping=aes(x=Timer, y=xg_total), colour = "#E24F55", size = 1.5, alpha = 0.8)
p

#Add Team2 Data
p <- p + geom_step(data=France_shotData, mapping=aes(x=Timer, y=xg_total), colour = "#2B6DD2", size = 1.5, alpha = 0.8)
p


### prepare the data 
Croatia_goals <- Croatia_shotData %>% filter(shot.outcome.name == "Goal")
France_goals <- France_shotData %>% filter(shot.outcome.name == "Goal")

## add the points on for the goals 
p <- p + 
  geom_point(data = Croatia_goals, aes(x=Timer, y=xg_total), colour = "black", size = 4, alpha = 1, fill = "#E24F55", shape = 21) +
  geom_point(data = France_goals, aes(x=Timer, y=xg_total), colour = "black", size = 4, alpha = 1, fill = "#2B6DD2", shape = 21)

p


## find the axis length for the plot
yMax <- ggplot_build(p)$layout$panel_ranges[[1]]$y.range

## create the annotation titles 
Croatia_label <- paste0(unique(shotData$team.name)[1], ": ", nrow(Croatia_goals), " Actual: ", round(max(Croatia_shotData$xg_total), 2)," xG ")

France_label <- paste0(unique(shotData$team.name)[2], ": ", "4", " Actual: ", round(max(France_shotData$xg_total), 2)," xG "," + 1 OG ")

## add the legend 
p + annotate("text", x = 0, y = 1.5, label = Croatia_label, hjust = 0, colour = "#E24F55") + annotate("text", x = 0, y = 1.7, label = France_label, hjust = 0, colour = "#2B6DD2") + labs(x = "Minutes", y = "Accumulative xG") + theme_minimal()
