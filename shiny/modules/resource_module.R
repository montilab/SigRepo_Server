# resource page module 

resource_module_ui <- function(id) {
  
  ns <- NS(id)
  
  tabPanel(
    title = "Resources",
    value = "resource",
    fluidPage(
      div(
        style = "padding-top: 70px;",  
        h2("resources"),
        br(),
        div(
          style = "color: #888; font-size: 20px;",
          icon("tools"),
          " This page is under construction. Please check back later."
        )
      )
    )
  )
  
}

resource_module_server <- function(id){
  moduleServer(id, function(input, output, session){
    # where the server logic goes, nothing yey
  })
}