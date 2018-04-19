#World Cup 2018 Prediction
#Alexander Powell & Delaney Ambrosen
#April 18, 2018
#
#

#DATA PROCESSING ----
#Packages
library(dplyr)

#Data
soccer.db <- read.csv("soccer_db.csv")

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

#Remove UNIFFAC Cup
soccer.db <-soccer.db %>%
  filter(Tournament != "UNIFFAC Cup")

#Analyzing One Team
Germany <- soccer.db %>%
  filter(Home == "Germany" | Away == "Germany")
60/210
b=50/210
#Adding in rankings for each cup
soccer.db_ext<-soccer.db %>% mutate(Rate=case_when (Tournament=="FIFA World Cup"~ (60/210), Tournament=="Confederations Cup"~ (50/210),Tournament=="UEFA Euro"|
                                                    Tournament=="AFC Asian Cup"|Tournament=="Copa América"|Tournament=="CONCACAF Championship"|Tournament=="Oceania Nations Cup"~(40/210),
                                                    Tournament=="African Cup of Nations"|Tournament=="Gold Cup"|Tournament=="UNCAF Cup"|Tournament=="UAFA Cup"|
                                                    Tournament=="CCCF Championship"|Tournament=="NAFU Championship"~(30/210),
                                                    Tournament=="CFU Caribbean Cup"|Tournament=="CECAFA Cup"|Tournament=="AFF Championships"|Tournament=="COSAFA Cup"|
                                                    Tournament=="South Pacific Games"|Tournament=="AFC Challenge Cup"|Tournament=="SAFF Cup"|Tournament=="EAFF Cup"|
                                                    Tournament=="WAFF Championship"~(20/210),Tournament=="Friendly"~(10/210)))




Tournament=="CFU Caribbean Cup"|Tournament=="CECAFA Cup"|
#Looking at Home Team Effect
HomeTeams <- soccer.db %>%
  filter(Loc.Home == 1) %>%
  mutate(scoring = H.Score - A.Score) %>%
  group_by(Location) %>%
  dplyr::summarise(avg.score = mean(scoring), number = n()) %>%
  dplyr::select(Location, avg.score, number) %>%
  unique() %>%
  filter(number >= 50)

