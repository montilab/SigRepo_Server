shiny::div(
  class = "search-collection-container",
  
  shiny::fluidRow(
    class = "search-collection-wrapper",
    
    shiny::column(
      width = 12,
      class = "search-collection-title",
      shiny::p("Search Collection")
    ),
    
    shiny::column(
      width = 4,
      class = "search-collection-sidebar",
      
      shiny::selectizeInput(
        inputId = "collection_name",
        label = strong("Collection:"),
        choices = c("Please chooose from an option below"=""),
        multiple = TRUE,
        width = "100%"
      ),
      
      shiny::actionButton(
        inputId = "search_collection",
        label = strong("Search"),
        class = "submit-button"
      ),
      
      shiny::uiOutput(outputId = "sig_colletion_error_msg"),
    ),
    
    shiny::column(
      width = 8,
      class = "search-collection-content",
      
      DT::dataTableOutput("collection_tbl"),
      shiny::uiOutput(outputId = "search_sig_colletion_error_msg"),
      DT::dataTableOutput("collection_signature_tbl")
    )
  )
)
  
