# compare page modules

compare_module_ui <- function(id) {
  ns <- NS(id)
  
  tabPanel(
    title = "Compare",
    value = "compare",
    fluidPage(
      div(
        style = "padding-top: 50px;",  # Add top padding here
        h2("Compare"),
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


compare_module_server <- function(id) {
  moduleServer(id, function(input, output, session) {
   # where the server logic goes
  })
}
