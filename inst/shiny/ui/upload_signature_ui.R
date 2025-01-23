
shiny::div(
  class = "upload-signature-container", 
  
  shiny::fluidRow(
    class = "upload-signature-wrapper",
    
    shiny::column(
      width = 12,
      class = "upload-signature-title",
      shiny::p("Upload Signature")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-signature-content",
      shiny::fileInput(
        inputId = "ES_file",
        label = strong(span(style = "color: red;", "*"), "OmicSignature File:"),
        width = "100%"
      ),
      
      br(), br(),
      
      shiny::uiOutput(outputId = "upload_sig_error_msg"),
      
      shiny::actionButton(
        inputId = "upload_signature",
        label = "Upload",
        class = "submit-button"
      )
    )
  )
)

