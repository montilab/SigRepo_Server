
# R packages for building shiny dashboard
library(htmltools)
library(shiny)

# For DB connection
library(RMySQL)
library(DBI)

# For data cleaning, extraction and manipulation
library(tidyverse)

# For loading and installing packages
library(devtools)
load_all()

## Establish database connection
conn <- newConnHandler(
  driver = RMySQL::MySQL(),
  dbname = Sys.getenv("DBNAME"), 
  host = Sys.getenv("HOST"), 
  port = as.integer(Sys.getenv("PORT")), 
  user = Sys.getenv("USER"), 
  password = Sys.getenv("PASSWORD")
)

## Define ui logic ####
ui <- bootstrapPage(
  
  title = "SigRepo - Signature Repository",
  
  tagList(
    tags$head(
      tags$link(type="text/css", rel="stylesheet", href="assets/css/main.css"),
      tags$link(type="text/css", rel="stylesheet", href="assets/css/sign_in.css"),
      tags$link(type="text/css", rel="stylesheet", href="assets/css/search-signature.css"),
      tags$link(type="text/css", rel="stylesheet", href="assets/css/fontawesome-all.min.css")
    )
  ),
  
  div(
    id="page-wrapper",
    
    #### Header ####
    div(
      id="header-wrapper",
      includeHTML("www/nav.html")
    ),
    
    ### content ####
    div(
      id="content-wrapper",
      
      uiOutput("content_page"),
      
      ### Footer ####
      div(
        id="footer-wrapper",
        
        tags$footer(
          class="container",
          
          div(
            class="row footer",
            
            ### copyright ####
            div(
              class="col-12 col-12-medium box copyright",
              p(HTML('&copy; Montilab | Boston University | ', format(Sys.Date(), format = "%Y"), ' | All rights reserved | Design by <a href="http://html5up.net">HTML5 UP</a>'))
            )
          )
        )
      )
    )
  ),
  
  tags$script(src="assets/js/jquery.dropotron.min.js"),
  tags$script(src="assets/js/browser.min.js"),
  tags$script(src="assets/js/breakpoints.min.js"),
  tags$script(src="assets/js/util.js"),
  tags$script(src="assets/js/main.js")
  
)

## Define server logic ####
server <- function(input, output, session) {
  
  source("server/search_signature_server.R", local=TRUE)
  source("server/search_collection_server.R", local=TRUE)
  source("server/sign_in_server.R", local=TRUE)
  
  user_login <- reactiveVal(TRUE)
  page_selected <- reactiveVal("sign_in")
  
  observeEvent(input$selected_page, {
    print(input$selected_page)
    page_selected(input$selected_page)
  })
  
  output$content_page <- renderUI({
    
    req(user_login())
    
    if(page_selected() == "home"){
      includeHTML("www/home_content.html")
    }else if(page_selected() == "search_signature"){
      source("ui/search_signature_ui.R")$value
    }else if(page_selected() == "search_collection"){
      source("ui/search_collection_ui.R")$value
    }else if(page_selected() == "extract_signature"){
    }else if(page_selected() == "upload_signature"){
    }else if(page_selected() == "compare_signatures"){
    }else if(page_selected() == "hypeR_analysis"){
    }else if(page_selected() == "resources"){
    }else if(page_selected() == "sign_in"){
      includeHTML("www/sign_in.html")
    }
    
  })
  
  
}

## Start the app ####
shiny::shinyApp(ui=ui, server=server)




