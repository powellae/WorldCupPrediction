#World Cup 2018 Prediction
#Alexander Powell & Delaney Ambrosen
#June 13, 2018
#
#

#Data Processing ----
#Packages
library(dplyr)
library(boot)

#Data
soccer.db <- read.csv("soccer_db2.csv", stringsAsFactors = FALSE)

#Cleaning Data
soccer.db$Home <- gsub("\\ - .*", "", soccer.db$Match)
soccer.db$Away <- sub('.* - ', '', soccer.db$Match)
  
soccer.db$H.Score <- as.numeric(gsub("\\:.*", "", soccer.db$Score))
soccer.db$A.Score <- as.numeric(sub('.*:', '', soccer.db$Score))

soccer.db$Location <- sub('.*, ', '', soccer.db$Venue)
soccer.db$Loc.Home <- ifelse(soccer.db$Location == soccer.db$Home, 1, 0)

soccer.db$Year <- as.numeric(gsub("\\-.*", "", soccer.db$Date))

#Remove Amateur Games
soccer.db <- soccer.db %>%
  filter(Tournament != "Amateur") %>%
  filter(Year >= (2018-50))

#Remove "XI" Teams
soccer.db$XI <- ifelse(grepl("XI", soccer.db$Home), 1, ifelse(grepl("XI", soccer.db$Away), 1, 0))
soccer.db <- soccer.db %>%
  filter(soccer.db$XI == 0) %>%
  dplyr::select(-c(XI))
#Looking at Home Team Effect
HomeTeams <- soccer.db %>%
  filter(Loc.Home == 1) %>%
  mutate(scoring = H.Score - A.Score) %>%
  group_by(Location) %>%
  dplyr::summarise(avg.score = mean(scoring), number = n()) %>%
  dplyr::select(Location, avg.score, number) %>%
  unique() %>%
  filter(number >= 50)

#Add World Cup Qualifying to Tournament
soccer.db$Extra <- soccer.db$Tournament
soccer.db <- within(soccer.db, Tournament[Tournament == 'FIFA World Cup' & nchar(as.character(soccer.db$Season)) > 4] <- "WC Qualifying")


#Functions----

#Provide weights and lambda values for a given team
Weighting <- function(team, data){
  df <- soccer.db %>%
    filter(Home == team | Away == team) %>%
    dplyr::mutate(Weight.Scale = ((Year-min(Year))/(max(Year)-min(Year)))*(200) - 100) %>%
    dplyr::mutate(Rate=case_when (Tournament=="FIFA World Cup"~ (.22), Tournament=="Confederations Cup" | Tournament=="WC Qualifying"~ (.2),Tournament=="UEFA Euro"|
                             Tournament=="AFC Asian Cup"|Tournament=="Copa América"|Tournament=="CONCACAF Championship"|Tournament=="Oceania Nations Cup"~(.18),
                           Tournament=="African Cup of Nations"| Tournament=="League" | Tournament=="Gold Cup"|Tournament=="UNCAF Cup"|Tournament=="UAFA Cup"|
                             Tournament=="CCCF Championship"|Tournament=="NAFU Championship"~(.16),
                           Tournament=="CFU Caribbean Cup"|Tournament=="CECAFA Cup"|Tournament=="AFF Championship"|Tournament=="COSAFA Cup"|
                             Tournament=="South Pacific Games"|Tournament=="AFC Challenge Cup"|Tournament=="SAFF Cup"|Tournament=="EAFF Championship"|
                             Tournament=="WAFF Championship" | Tournament=="UNIFFAC Cup"~(.1),Tournament=="Friendly"~(.14)))
  
  df$weights <- exp(.05*df$Weight.Scale)/(1 + exp(.05*df$Weight.Scale))
  df$weights <- df$weights/sum(df$weights)
  df$weights <- ifelse(is.nan(df$weights), 0, df$weights)
  df$Rate <- df$Rate/sum(df$Rate)
  
  df$Weights <- (.9*df$weights + .1*df$Rate)/(.9*sum(df$weights) + .1*sum(df$Rate))
  df$Weights <- ifelse(is.nan(df$Weights), 0, df$Weights)
  df$Offense.Score <- ifelse(df$Home == team, df$H.Score, df$A.Score)
  df$Defense.Score <- ifelse(df$Home == team, df$A.Score, df$H.Score)
  df$Offense.Lambda <- df$Offense.Score*df$Weights
  df$Defense.Lambda <- df$Defense.Score*df$Weights
  
  Offense <- sum(df$Offense.Lambda)
  Defense <- sum(df$Defense.Lambda)
  
  return(c(Offense, Defense))
}

Weighting2 <- function(team, data){
  df <- soccer.db %>%
    filter(Home == team | Away == team) %>%
    dplyr::mutate(Weight.Scale = ((Year-min(Year))/(max(Year)-min(Year)))*(200) - 100) %>%
    dplyr::mutate(Rate=case_when (Tournament=="FIFA World Cup"~ (.22), Tournament=="Confederations Cup" | Tournament=="WC Qualifying"~ (.2),Tournament=="UEFA Euro"|
                                    Tournament=="AFC Asian Cup"|Tournament=="Copa América"|Tournament=="CONCACAF Championship"|Tournament=="Oceania Nations Cup"~(.18),
                                  Tournament=="African Cup of Nations"| Tournament=="League" | Tournament=="Gold Cup"|Tournament=="UNCAF Cup"|Tournament=="UAFA Cup"|
                                    Tournament=="CCCF Championship"|Tournament=="NAFU Championship"~(.16),
                                  Tournament=="CFU Caribbean Cup"|Tournament=="CECAFA Cup"|Tournament=="AFF Championship"|Tournament=="COSAFA Cup"|
                                    Tournament=="South Pacific Games"|Tournament=="AFC Challenge Cup"|Tournament=="SAFF Cup"|Tournament=="EAFF Championship"|
                                    Tournament=="WAFF Championship" | Tournament=="UNIFFAC Cup"~(.1),Tournament=="Friendly"~(.14)))
  
  df$Opponent <- ifelse(df$Home == team, df$Away, df$Home)
  df <- merge(df, soccer.db2, by.x = "Opponent", by.y = "Team", all.x = TRUE)
  df$opp.wgt.off <- sum(df$Lambda.D)/nrow(df)
  df$opp.wgt.def <- sum(df$Lambda.O)/nrow(df)
  df$weights <- exp(.05*df$Weight.Scale)/(1 + exp(.05*df$Weight.Scale))
  df$weights <- df$weights/sum(df$weights)
  df$weights <- ifelse(is.nan(df$weights), 0, df$weights)
  df$Rate <- df$Rate/sum(df$Rate)
  
  df$Weights.O <- (.45*df$weights + .05*df$Rate + .5*df$opp.wgt.off)/(.45*sum(df$weights) + .05*sum(df$Rate) + .5*sum(df$opp.wgt.off))
  df$Weights.D <- (.45*df$weights + .05*df$Rate + .5*df$opp.wgt.def)/(.45*sum(df$weights) + .05*sum(df$Rate) + .5*sum(df$opp.wgt.def))
  df$Weights.O <- ifelse(is.nan(df$Weights.O), 0, df$Weights.O)
  df$Weights.D <- ifelse(is.nan(df$Weights.D), 0, df$Weights.D)
  df$Offense.Score <- ifelse(df$Home == team, df$H.Score, df$A.Score)
  df$Defense.Score <- ifelse(df$Home == team, df$A.Score, df$H.Score)
  df$Offense.Lambda <- df$Offense.Score*df$Weights.O
  df$Defense.Lambda <- df$Defense.Score*df$Weights.D
  
  Offense <- sum(df$Offense.Lambda)
  Defense <- sum(df$Defense.Lambda)
  
  avg.offense <- (sum(soccer.db$H.Score) + sum(soccer.db$A.Score))/(2*nrow(soccer.db))
  n <- nrow(df)
  
  x <- Weighting(soccer.db2$Team[i], soccer.db)
  Offense <- ifelse(n >= 500, Offense, Offense*(n/500) + avg.offense*(1-(n/500)))
  Defense <- ifelse(n >= 500, Defense, Defense*(n/500) + avg.offense*(1-(n/500)))
  
  return(c(Offense, Defense))
}

#Simulating a single game and add game to database
Simulate.Game <- function(Home.Team, Away.Team, temp){
  Home <- Weighting(Home.Team, temp)
  Away <- Weighting(Away.Team, temp)
  
  Home.Score <- rpois(1, (0.7*Home[1] + 0.3*Away[2]))
  Away.Score <- rpois(1, (0.7*Away[1] + 0.3*Home[2]))
  
  df.newgame <- data.frame(Home.Team, Away.Team, "FIFA World Cup", Home.Score, Away.Score, 2018)
  names(df.newgame) <- c("Home", "Away", "Tournament", "H.Score", "A.Score", "Year")
  temp <- rbind(temp, df.newgame)
  assign('temp',temp,envir=.GlobalEnv)

  return(c(Home.Score, Away.Score))
}
#Simulate.Game("German", "Peru", temp)

#Find head to head winners in group table (support function)
HeadToHead <- function(df, i, j, GroupStage){
  if(df$Diff[i] > df$Diff[j]){
    return(c(i,j))
  } else if(df$Diff[j] > df$Diff[i]) {
    return(c(j,i))
  } else if(df$PF[i] > df$PF[j]){
    return(c(i,j))
  } else if(df$PF[j] > df$PF[i]){
    return(c(j,i))
  } else {
    Team1 <- df$Team[i]
    Team2 <- df$Team[j]

    Group.H2H <- GroupStage %>%
      filter(Home == Team1 | Home == Team2) %>%
      filter(Away == Team1 | Away == Team2)
    
    i.score <- ifelse(Group.H2H$Home[1] == df$Team[i], Group.H2H$H.Score[1], Group.H2H$A.Score[1])
    j.score <- ifelse(Group.H2H$Home[1] == df$Team[j], Group.H2H$H.Score[1], Group.H2H$A.Score[1])
    
    if(i.score > j.score){
      return(c(i,j))
    } else if(j.score > i.score){
      return(c(j,i))
    } else {
      x = sample(1:2, 1)
      if(x == 1){
        return(c(i,j))
      } else {
        return(c(j,i))
      }
    }
  }
}

#Go through group table to find who advances (support function)
Advance <- function(df, GroupStage){
  for(i in 1:nrow(df)){
    var1 = i
    for(j in 1:nrow(df)){
      if(i != j){
        var2 = j
        if(df$Order[i] == df$Order[j]){
          reference <- HeadToHead(df, i, j, GroupStage)
          df$Order[reference[2]] <- df$Order[reference[2]] - .5
        }
      }
    }
  }
  
  return(df)
}

#Build group table and find those who advance
Group.Table <- function(Letter, GroupStage){
  df.Home <- GroupStage %>%
    filter(Group == Letter) %>%
    dplyr::select(Home, H.Score, A.Score, H.Points)
  
  df.Away <- GroupStage %>%
    filter(Group == Letter) %>%
    dplyr::select(Away, A.Score, H.Score, A.Points)
  
  colnames(df.Home) <- c("Team", "PF", "PA", "Pts")
  colnames(df.Away) <- c("Team", "PF", "PA", "Pts")
  
  df.Group <- rbind(df.Home, df.Away) %>%
    group_by(Team) %>%
    dplyr::mutate(PF = sum(PF), PA = sum(PA), Diff = PF-PA, Pts = sum(Pts)) %>%
    unique()
  
  for(i in 1:nrow(df.Group)){
    points <- df.Group$Pts[i]
    for(j in 1:nrow(df.Group)){
      
    }
  }
  df.Group$Order <- rank(df.Group$Pts, ties.method = "max")
  df.Group.Order <- Advance(df.Group, GroupStage)
  
  df.Group.Order <- df.Group.Order[with(df.Group.Order, order(-Order)),]
  
  return(c(df.Group.Order$Team[1], df.Group.Order$Team[2]))
}

#Simulation----
#Team Lambda 1
soccer.db_temp1 <- soccer.db %>%
  group_by(Home) %>%
  dplyr::select(Home) %>%
  unique()

soccer.db_temp2 <- soccer.db %>%
  group_by(Away) %>%
  dplyr::select(Away) %>%
  unique()

colnames(soccer.db_temp1) <- c("Team")
colnames(soccer.db_temp2) <- c("Team")

soccer.db2 <- merge(soccer.db_temp1, soccer.db_temp2, by = "Team", all = TRUE)

avg.offense <- (sum(soccer.db$H.Score) + sum(soccer.db$A.Score))/(2*nrow(soccer.db))
for(i in 1:nrow(soccer.db2)){
  y <- soccer.db %>%
    filter(Home == soccer.db2$Team[i] | Away == soccer.db2$Team[i])
  soccer.db2$n[i] <- nrow(y)
  
  x <- Weighting(soccer.db2$Team[i], soccer.db)
  soccer.db2$Lambda.O[i] <- ifelse(soccer.db2$n[i] >= 500, x[1], x[1]*(soccer.db2$n[i]/500) + avg.offense*(1-(soccer.db2$n[i]/500)))
  soccer.db2$Lambda.D[i] <- ifelse(soccer.db2$n[i] >= 500, x[2], x[2]*(soccer.db2$n[i]/500) + avg.offense*(1-(soccer.db2$n[i]/500)))
}




Simulate.WC <- function(sim, Game.Numb){
  GS <- read.csv("GroupStage18.csv")
  #Adding Results as Tournament Progresses
  GS$H.Score <- NA
  GS$A.Score <- NA
  GS$H.Points <- NA
  GS$A.Points <- NA
  #Game #1 Russia vs. Saudi Arabia
  GS$H.Score[1] <- 5
  GS$A.Score[1] <- 0
  for(k in 1:(Game.Numb-1)){
    GS$H.Points[k] <- ifelse(GS$H.Score[k] > GS$A.Score[k], 3, ifelse(GS$H.Score[k] < GS$A.Score[k], 0, 1))
    GS$A.Points[k] <- ifelse(GS$H.Points[k] == 3, 0, ifelse(GS$H.Points[k] == 0, 3, 1))
  }
  
  #Building results table
  Table1 <- GS %>%
    dplyr::select(Home, Group)
  
  Table2 <- GS %>%
    dplyr::select(Away, Group)
  
  colnames(Table1) <- c("Team", "Group")
  colnames(Table2) <- c("Team", "Group")
  
  Table <- rbind(Table1, Table2) %>%
    group_by(Team, Group) %>%  
    dplyr::mutate(RD16 = 0, QF = 0, SF = 0, Finals = 0, Winner = 0) %>% 
    unique()
  for(k in 1:sim){
    print(k)
    #Temporary version of soccer.db to run simulations hot
    temp <- soccer.db %>%
      dplyr::select(Home, Away, Tournament, H.Score, A.Score, Year)
    
    #Group Stage Data
    GroupStage <- GS
    
    #Simulate Group Stage
    for(i in Game.Numb:nrow(GroupStage)){
      Game <- Simulate.Game(GroupStage$Home[i], GroupStage$Away[i], temp)
      GroupStage$H.Score[i] <- Game[1]
      GroupStage$A.Score[i] <- Game[2]
      GroupStage$H.Points[i] <- ifelse(GroupStage$H.Score[i] > GroupStage$A.Score[i], 3, ifelse(GroupStage$H.Score[i] < GroupStage$A.Score[i], 0, 1))
      GroupStage$A.Points[i] <- ifelse(GroupStage$H.Points[i] == 3, 0, ifelse(GroupStage$H.Points[i] == 0, 3, 1))
    }
    GroupStage$Home <- as.character(GroupStage$Home)
    GroupStage$Away <- as.character(GroupStage$Away)
    
    #Finding those who advance from group stage
    A <- Group.Table("A", GroupStage)
    B <- Group.Table("B", GroupStage)
    C <- Group.Table("C", GroupStage)
    D <- Group.Table("D", GroupStage)
    E <- Group.Table("E", GroupStage)
    F <- Group.Table("F", GroupStage)
    G <- Group.Table("G", GroupStage)
    H <- Group.Table("H", GroupStage)
    
    #Round of 16
    RD16 <- data.frame(ID=1:8, Home=NA, Away=NA, H.Score=NA, A.Score=NA)
    RD16$Home[1] <- A[1]
    RD16$Away[1] <- B[2]
    
    RD16$Home[2] <- C[1]
    RD16$Away[2] <- D[2]
    
    RD16$Home[3] <- E[1]
    RD16$Away[3] <- F[2]
    
    RD16$Home[4] <- G[1]
    RD16$Away[4] <- H[2]
    
    RD16$Home[5] <- B[1]
    RD16$Away[5] <- A[2]
    
    RD16$Home[6] <- D[1]
    RD16$Away[6] <- C[2]
    
    RD16$Home[7] <- F[1]
    RD16$Away[7] <- E[2]
    
    RD16$Home[8] <- H[1]
    RD16$Away[8] <- G[2]
    
    #R16 Games
    for(i in 1:nrow(RD16)){
      repeat{
        Game <- Simulate.Game(RD16$Home[i], RD16$Away[i], temp)
        if(Game[1] != Game[2]){
          break
        }
      }
      RD16$H.Score[i] <- Game[1]
      RD16$A.Score[i] <- Game[2]
      for(j in 1:nrow(Table)){
        if(RD16$Home[i] == Table$Team[j]){
          Table$RD16[j] <- Table$RD16[j] + 1
        }
        if(RD16$Away[i] == Table$Team[j]){
          Table$RD16[j] <- Table$RD16[j] + 1
        }
      }
    }
    
    #Quarterfinals
    QF <- data.frame(ID=1:4, Home=NA, Away=NA, H.Score=NA, A.Score=NA)
    QF$Home[1] <- ifelse(RD16$H.Score[1] > RD16$A.Score[1], RD16$Home[1], RD16$Away[1])
    QF$Away[1] <- ifelse(RD16$H.Score[2] > RD16$A.Score[2], RD16$Home[2], RD16$Away[2])
    
    QF$Home[2] <- ifelse(RD16$H.Score[3] > RD16$A.Score[3], RD16$Home[3], RD16$Away[3])
    QF$Away[2] <- ifelse(RD16$H.Score[4] > RD16$A.Score[4], RD16$Home[4], RD16$Away[4])
    
    QF$Home[3] <- ifelse(RD16$H.Score[5] > RD16$A.Score[5], RD16$Home[5], RD16$Away[5])
    QF$Away[3] <- ifelse(RD16$H.Score[6] > RD16$A.Score[6], RD16$Home[6], RD16$Away[6])
    
    QF$Home[4] <- ifelse(RD16$H.Score[7] > RD16$A.Score[7], RD16$Home[7], RD16$Away[7])
    QF$Away[4] <- ifelse(RD16$H.Score[8] > RD16$A.Score[8], RD16$Home[8], RD16$Away[8])
    
    #QF Games
    for(i in 1:nrow(QF)){
      repeat{
        Game <- Simulate.Game(QF$Home[i], QF$Away[i], temp)
        if(Game[1] != Game[2]){
          break
        }
      }
      QF$H.Score[i] <- Game[1]
      QF$A.Score[i] <- Game[2]
      for(j in 1:nrow(Table)){
        if(QF$Home[i] == Table$Team[j]){
          Table$QF[j] <- Table$QF[j] + 1
        }
        if(QF$Away[i] == Table$Team[j]){
          Table$QF[j] <- Table$QF[j] + 1
        }
      }
    }
    
    #Semifinals
    SF <- data.frame(ID=1:2, Home=NA, Away=NA, H.Score=NA, A.Score=NA)
    SF$Home[1] <- ifelse(QF$H.Score[1] > QF$A.Score[1], QF$Home[1], QF$Away[1])
    SF$Away[1] <- ifelse(QF$H.Score[2] > QF$A.Score[2], QF$Home[2], QF$Away[2])
    
    SF$Home[2] <- ifelse(QF$H.Score[3] > QF$A.Score[3], QF$Home[3], QF$Away[3])
    SF$Away[2] <- ifelse(QF$H.Score[4] > QF$A.Score[4], QF$Home[4], QF$Away[4])
    
    #SF Games
    for(i in 1:nrow(SF)){
      repeat{
        Game <- Simulate.Game(SF$Home[i], SF$Away[i], temp)
        if(Game[1] != Game[2]){
          break
        }
      }
      SF$H.Score[i] <- Game[1]
      SF$A.Score[i] <- Game[2]
      for(j in 1:nrow(Table)){
        if(SF$Home[i] == Table$Team[j]){
          Table$SF[j] <- Table$SF[j] + 1
        }
        if(SF$Away[i] == Table$Team[j]){
          Table$SF[j] <- Table$SF[j] + 1
        }
      }
    }
    
    #Finals
    Finals <- data.frame(ID=1, Home=NA, Away=NA, H.Score=NA, A.Score=NA)
    Finals$Home[1] <- ifelse(SF$H.Score[1] > SF$A.Score[1], SF$Home[1], SF$Away[1])
    Finals$Away[1] <- ifelse(SF$H.Score[2] > SF$A.Score[2], SF$Home[2], SF$Away[2])
    
    #Finals Game
    for(i in 1:nrow(Finals)){
      repeat{
        Game <- Simulate.Game(Finals$Home[i], Finals$Away[i], temp)
        winner <- ifelse(Game[1] > Game[2], Finals$Home[i], Finals$Away[i])
        if(Game[1] != Game[2]){
          break
        }
      }
      Finals$H.Score[i] <- Game[1]
      Finals$A.Score[i] <- Game[2]
      for(j in 1:nrow(Table)){
        if(Finals$Home[i] == Table$Team[j]){
          Table$Finals[j] <- Table$Finals[j] + 1
        }
        if(Finals$Away[i] == Table$Team[j]){
          Table$Finals[j] <- Table$Finals[j] + 1
        }
        if(winner == Table$Team[j]){
          Table$Winner[j] <- Table$Winner[j] + 1
          print(paste0(winner, ": ", Table$Winner[j]))
        }
      }
    }
  }
  Table$RD16 <- 100*(Table$RD16 / sim)
  Table$QF <- 100*(Table$QF / sim)
  Table$SF <- 100*(Table$SF / sim)
  Table$Finals <- 100*(Table$Finals / sim)
  Table$Winner <- 100*(Table$Winner / sim)
  return(Table)
}

WC2018 <- Simulate.WC(1000, 2)

#Predicting one game
TeamOne <- "Russia"
TeamTwo <- "Saudi Arabia"
sim <- 10000

temp <- soccer.db %>%
  dplyr::select(Home, Away, Tournament, H.Score, A.Score, Year)

df <- data.frame(ID=1:sim)
for(i in 1:nrow(df)){
  x <- Simulate.Game(TeamOne, TeamTwo, temp)
  df$home[i] <- x[1]
  df$away[i] <- x[2]
}

df$home.win <- ifelse(df$home > df$away, 1, 0)
df$away.win <- ifelse(df$away > df$home, 1, 0)
Team1 <- sum(df$home.win)/sim
Team2 <- sum(df$away.win)/sim
Draw <- 1 - Team1 - Team2
Team1
Team2
Draw