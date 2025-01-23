
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
          "Signature Name" = "signature_name", 
          "Organism" = "organism", 
          "Phenotype" = "phenotype",
          "Sample Type" = "sample_type", 
          "Platform ID" = "platform", 
          "Assay Type" = "assay_type"
        ),
        multiple = TRUE,
        width = "100%"
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "signature_name",
          label = "Signature Name:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "organism",
          label = "Organism:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "phenotype",
          label = "Phenotype:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "sample_type",
          label = "Sample Type:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "platform",
          label = "Platform:",
          choices = c("Please choose from an option below" = ""),
          multiple = TRUE,
          width = "100%",
        )
      ),
      
      shinyjs::hidden(
        shiny::selectizeInput(
          inputId = "assay_type",
          label = "Assay Type:",
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
      style = "border: 1px solid gray; padding: 20px 20px 20px 20px;",
      
      DT::dataTableOutput("signature_table")
      
    )
  )
)

