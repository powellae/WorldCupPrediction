#World Cup 2018 Prediction
#Alexander Powell & Delaney Ambrosen
#April 18, 2018
#
#

#DATA PROCESSING ----
#Packages
library(dplyr)

#Data
soccer.db <- read.csv("Documents/Analytics/soccer_db.csv")

#Cleaning Data
soccer.db$Home <- gsub("\\ - .*", "", soccer.db$Match)
soccer.db$Away <- sub('.* - ', '', soccer.db$Match)
  
soccer.db$H.Score <- as.numeric(gsub("\\:.*", "", soccer.db$Score))
soccer.db$A.Score <- as.numeric(sub('.*:', '', soccer.db$Score))

soccer.db$Location <- sub('.*, ', '', soccer.db$Venue)
soccer.db$Loc.Home <- ifelse(soccer.db$Location == soccer.db$Home, 1, 0)


#Remove Amateur Games
soccer.db <- soccer.db %>%
  filter(Tournament != "Amateur")

#Analyzing One Team
Germany <- soccer.db %>%
  filter(Home == "Germany" | Away == "Germany")

#Looking at Home Team Effect
HomeTeams <- soccer.db %>%
  filter(Loc.Home == 1) %>%
  mutate(scoring = H.Score - A.Score) %>%
  group_by(Location) %>%
  dplyr::summarise(avg.score = mean(scoring), number = n()) %>%
  dplyr::select(Location, avg.score, number) %>%
  unique() %>%
  filter(number >= 50)
