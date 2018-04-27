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
    mainPanel(DT::dataTableOutput('ex1'))

)
)

