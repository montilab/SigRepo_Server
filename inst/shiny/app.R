
# R packages
library(htmltools)
library(shiny)


## Define ui logic ####
ui <- htmlTemplate("www/index.html")

## Define server logic ####
server <- function(input, output, session) {
}

## Start the app ####
shiny::shinyApp(ui=ui, server=server)




