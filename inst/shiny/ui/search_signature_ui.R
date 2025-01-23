
shiny::div(
  class = "search-signature-container", 
  
  shiny::fluidRow(
    class = "search-signature-wrapper",
    
    shiny::column(
      width = 12,
      class = "search-signature-title",
      shiny::p("Search Signature")
    ),
    
    shiny::column(
      width = 4,
      class = "search-signature-content",

      shiny::selectizeInput(
        inputId = "search_options",
        label = "Search Options:",
        choices = c(
          "Choose from a list of options below" = "", 
          "signature_name", 
          "organism", 
          "phenotype",
          "sample_type", 
          "platform", 
          "assay_type"
        ),
        multiple = TRUE,
        width = "100%"
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "signature_name",
          label = "signature_name:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "organism",
          label = "organism:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "phenotype",
          label = "phenotype:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "sample_type",
          label = "sample_type:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "platform",
          label = "platform:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "assay_type",
          label = "assay_type:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%"
        )
      ),
      
      br(),
      
      shiny::actionButton(
        inputId = "search_signature",
        label = "Search",
        class = "submit-button"
      ), 
      
      br(), br(),
      
      shiny::uiOutput(outputId = "search_sig_error_msg")
      
    ),
    
    shiny::column(
      width = 8,
      style = "border: 1px solid gray; padding: 20px 20px 20px 20px; overflow: scroll;",
      
      DT::dataTableOutput("signature_table"),
      shiny::uiOutput(outputId = "sig_tbl_error_msg"),
      shiny::div(shiny::downloadButton(outputId = "download_oms_handler", label = "Download OMS"), style = "visibility: hidden;")
      
    )
  )
)

