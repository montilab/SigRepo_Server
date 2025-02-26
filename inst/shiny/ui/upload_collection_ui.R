
shiny::div(
  class = "upload-collection-container", 
  
  shiny::fluidRow(
    class = "upload-collection-wrapper",
    
    # UPLOAD collectionS #####
    shiny::column(
      width = 12,
      class = "upload-collection-title",
      shiny::p("Upload collection")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-collection-content",
      shiny::fileInput(
        inputId = "oms_file",
        label = strong(span(style = "color: red;", "*"), "Omiccollection File (.RDS):"),
        multiple = FALSE,
        width = "100%"
      ),
      
      shiny::uiOutput(outputId = "upload_sig_error_msg"),
      
      br(), br(),
      
      shiny::actionButton(
        inputId = "upload_collection",
        label = "Upload",
        class = "submit-button"
      )
    ),
    
    # UPDATE collectionS #####
    shiny::column(
      width = 12,
      class = "upload-collection-title",
      shiny::p("Update collection")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-collection-content"
    ),
    
    # DELETE collectionS #####
    shiny::column(
      width = 12,
      class = "upload-collection-title",
      shiny::p("Delete collection")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-collection-content"
    )
  )
)

