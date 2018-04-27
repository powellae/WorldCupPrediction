# Load the ggplot2 package which provides
# the 'mpg' dataset.
#source("WorldCup18.R")
#source("Soccer.csv")
library(ggplot2)
library(DT)

shinyUI(
  fluidPage(
    
  tags$head(
    tags$style(HTML("
                    @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
                    
                    h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: Darkred;
                    }
                    
                    "))
    ),
  

    headerPanel(
      list(HTML('<img src="Worldcup.png" width="150" height="150"/>'), "2018 FIFA World Cup Predictions"),
      windowTitle="2018 FIFA World Cup Predictions"
    ),
    mainPanel(
      tabsetPanel(
        tabPanel("Table", DT::dataTableOutput('ex1')), 
        tabPanel("Methods", p(), p("Our 2018 World Cup Prediction is built from a database of all international competitions dating back to the 1870s
                 (soccer-db.info). We simulate the World Cup Group Stages and tournament play 1000 times, displaying the probabilites each nation advances to different levels
                 of the tournament and who achieves national glory in the summer of 2018."), p("The model is built on two iterations through each international
                                                                                               match. First, it weights each match by its level of competition (i.e. the World Cup matches are weighted
                                                                                               more heavily than Friendlies) and based on how recent the match was. The time-dependent weights were modeled using
                                                                                               an S-Curve for each nation, such that more recent games will be weighted most heavily. From this we build a mixture model
                                                                                               of each team's adjusted offensive and defensive value (goals per match). Then comes the second iteration in which we use each 
                                                                                               team's value to reweight their matches based on the level of competition. This enables known power-houses like Germany and Brazil to
                                                                                               rise to the top over teams that play in low-level confederations."), p("Using each team's newly calculated offensive and defensive value,
                                                                                                                                                                      we were then able to simulate each game via a Poisson distribution. We begin with simulating each group stage 
                                                                                                                                                                      match and then, after finding the winners and accessing tiebreakers in each group, simulate the 16-team bracket. Our methods are 
                                                                                                                                                                      incredibly robust for not using any statistics beyond the final score of matches, but this summer will be the true test for our 2018
                                                                                                                                                                      World Cup predictions. Check back here for updates to the model as more games are played, our github repository for more on the programming
                                                                                                                                                                      and statistical methods, and keep an eye on our medium.com page for a full explanation (links below)."), a("https://github.com/powellae/WorldCupPrediction", target="_blank"))
                 
                 )

)
))

