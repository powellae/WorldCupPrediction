
WC2018<-read.csv("./data/WC2018.csv")
#WC2018 <- read.csv("~/Documents/Sports Analytics/WorldCup/WorldCup2018/data/WC2018.csv")
j <- WC2018[with(WC2018, order(-Winner)),]
 j[,8]


function(input, output) {
  
  # display 10 rows initially
  output$ex1 <- DT::renderDataTable(
    DT::datatable(head(WC2018,34), options = list((pageLength = 20), list(visable=FALSE, targets=c(1)) ,initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#8B0000', 'color': '#fff'});",
      "}")
    ))
    %>% formatStyle(columns = "Winner", 
                    background = styleEqual(j[,8], c('#CA0000','#CA0000','#FF0000','#FF0F0F','#FF1E1E','#FF5454',
                                                     '#FF5D5D','#FE6464','#FF6969','#FF6D6D','#FF7070','#FF7878',
                                                     '#FE7F7F','#FF8686','#FF9191','#FF9393','#FF9797','#FF9C9C',
                                                     '#FFA1A1','#FFAEAE','#FFB0B0','#FFB6B6','#FFC4C4','#FFCCCC',
                                                     '#FFD2D2','#FFD7D7','#FFD8D8','#FFDCDC','#FEE1E1','#FFE7E7',
                                                     '#FFF1F1','#FFFFFF'))) )}

