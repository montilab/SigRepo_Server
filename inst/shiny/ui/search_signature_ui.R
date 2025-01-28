
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
      class = "search-signature-sidebar",

      shiny::selectizeInput(
        inputId = "search_options",
        label = "Search Options:",
        choices = c(
          "Choose from a list of options below" = "", 
          "signature_name", 
          "organism", 
          "phenotype",
          "sample_type", 
          "platform_id", 
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
          inputId = "platform_id",
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
      class = "search-signature-content",
      
      DT::dataTableOutput("signature_tbl"),
      shiny::uiOutput(outputId = "sig_tbl_error_msg"),
      br(),
      shinyjs::hidden(shiny::actionButton(inputId = "download_oms", label = "Download OmicSignature", class = "submit-button", onclick = "sig_tbl_select_rows();")), 
      shiny::div(shiny::downloadButton(outputId = "download_oms_handler", label = "Download"), style = "visibility: hidden;"),
      
      DT::dataTableOutput("difexp_tbl"),
      DT::dataTableOutput("sig_up_regulated_tbl"),
      DT::dataTableOutput("sig_down_regulated_tbl")
      
    )
  )
)

