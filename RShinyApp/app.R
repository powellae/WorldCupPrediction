library(ggplot2)
library(DT)

WC2018<-read.csv("WC2018.csv")
#WC2018 <- read.csv("~/Documents/Sports Analytics/WorldCup/WorldCup2018/data/WC2018.csv")
WC2018 <- WC2018[,2:8]
j <- WC2018[with(WC2018, order(-Winner)),]
j[,7]
names(WC2018)[3]<-"RD16 (%)"
names(WC2018)[4]<-"Quarter-Finals (%)"
names(WC2018)[5]<-"Semifinals (%)"
names(WC2018)[6]<-"Finals (%)"
names(WC2018)[7]<-"Winner (%)"


#' ui <- fluidPage(
#'     
#'     tags$head(
#'       tags$style(HTML("
#'                       @import url('//fonts.googleapis.com/css?family=Lobster|Cabin:400,700');
#'                       
#'                       h1 {
#'                       font-family: 'Lobster', cursive;
#'                       font-weight: 500;
#'                       line-height: 1.1;
#'                       color: Darkred;
#'                       }
#'                       
#'                       "))
#'       ),
#'      
#'     headerPanel(
#'       #list(HTML('<img src="Worldcup.png" width="150" height="150"/>'), "2018 FIFA World Cup Predictions"),
#'       list(HTML( "2018 FIFA World Cup Predictions"),
#'       
#'       windowTitle="2018 FIFA World Cup Predictions",
#'       tags$p("Updated: Thursday June 14 at 3:41pm")
#'     ),
#'     mainPanel(
#'       tags$p("Updated: Thursday June 14 at 3:41pm"),
#'       tabsetPanel(
#'         tabPanel("Table", DT::dataTableOutput('ex1')), 
#'         tabPanel("Methods", p(), p("Our 2018 World Cup Prediction is built from a database of all international competitions dating back to the 1870s
#'                  (soccer-db.info). We simulate the World Cup Group Stages and tournament play 1000 times, displaying the probabilites each nation advances to different levels
#'                  of the tournament and who achieves national glory in the summer of 2018."), p("The model is built on two iterations through each international
#'                                                                                                match. First, it weights each match by its level of competition (i.e. the World Cup matches are weighted
#'                                                                                                more heavily than Friendlies) and based on how recent the match was. The time-dependent weights were modeled using
#'                                                                                                an S-Curve for each nation, such that more recent games will be weighted most heavily. From this we build a mixture model
#'                                                                                                of each team's adjusted offensive and defensive value (goals per match). Then comes the second iteration in which we use each 
#'                                                                                                team's value to reweight their matches based on the level of competition. This enables known power-houses like Germany and Brazil to
#'                                                                                                rise to the top over teams that play in low-level confederations."), p("Using each team's newly calculated offensive and defensive value,
#'                                                                                                                                                                       we were then able to simulate each game via a Poisson distribution. We begin with simulating each group stage 
#'                                                                                                                                                                       match and then, after finding the winners and accessing tiebreakers in each group, simulate the 16-team bracket. Our methods are 
#'                                                                                                                                                                       incredibly robust for not using any statistics beyond the final score of matches, but this summer will be the true test for our 2018
#'                                                                                                                                                                       World Cup predictions. Check back here for updates to the model as more games are played, our github repository for more on the programming
#'                                                                                                                                                                       and statistical methods, and keep an eye on our medium.com page for a full explanation (links below)."), a("https://github.com/powellae/WorldCupPrediction", target="_blank"))
#'         
#'       )
#'       
#'     )
#'       )


ui <- fluidPage(
  tag$h1("2018 Fifa World Cup Predictions"), tags$p("Last Updated Thursday June 14th at 3:41pm"),
  tabsetPanel(
    tabPanel("Table", DT::dataTableOutput('ex1'))
  )
  
  
)
server <- function(input, output) {
  
  # display 10 rows initially
  output$ex1 <- DT::renderDataTable(
    DT::datatable(head(WC2018,34), rownames = FALSE, options = list((pageLength = 32), list(visable=FALSE, targets=c(1)) ,initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#8B0000', 'color': '#fff'});",
      "}")
    ))
    %>% formatStyle(columns = "Winner (%)", 
                    background = styleEqual(j[,7], c('#CA0000','#CA0000','#FF0000','#FF0F0F','#FF1E1E','#FF5454',
                                                     '#FF5D5D','#FE6464','#FF6969','#FF6D6D','#FF7070','#FF7878',
                                                     '#FE7F7F','#FF8686','#FF9191','#FF9393','#FF9797','#FF9C9C',
                                                     '#FFA1A1','#FFAEAE','#FFB0B0','#FFB6B6','#FFC4C4','#FFCCCC',
                                                     '#FFD2D2','#FFD7D7','#FFD8D8','#FFDCDC','#FEE1E1','#FFE7E7',
                                                     '#FFF1F1','#FFFFFF'))) )}


shinyApp(ui = ui, server = server)