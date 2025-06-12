
shiny::div(
  class = "upload-signature-container", 
  
  shiny::fluidRow(
    class = "upload-signature-wrapper",
    
    # UPLOAD SIGNATURES #####
    shiny::column(
      width = 12,
      class = "upload-signature-title",
      shiny::p("Upload Signature")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-signature-content",
      shiny::fileInput(
        inputId = "oms_file",
        label = strong(span(style = "color: red;", "*"), "OmicSignature File (.RDS):"),
        multiple = FALSE,
        width = "100%"
      ),
      
      shiny::uiOutput(outputId = "upload_sig_error_msg"),
      
      br(), br(),
      
      shiny::actionButton(
        inputId = "upload_signature",
        label = "Upload",
        class = "submit-button"
      )
    ),
    
    # UPDATE SIGNATURES #####
    shiny::column(
      width = 12,
      class = "upload-signature-title",
      shiny::p("Update Signature")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-signature-content"
    ),
    
    # DELETE SIGNATURES #####
    shiny::column(
      width = 12,
      class = "upload-signature-title",
      shiny::p("Delete Signature")
    ),
    
    shiny::column(
      width = 12,
      class = "upload-signature-content"
    )
  )
)

