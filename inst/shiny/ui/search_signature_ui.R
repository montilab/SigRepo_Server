
shiny::fluidRow(
  width = 12,
  id = "search-signature-wrapper",
  
  shiny::column(
    width = 12,
    shiny::h2("Search Signature")
  ),
  
  shiny::column(
    width = 4,
    style = "border: 1px solid gray; padding: 20px 20px 20px 20px;",
    
    shiny::selectizeInput(
      inputId = "search_options",
      label = "Search Options:",
      choices = c(
        "Choose from a list of options below" = "", 
        "Signature Name" = "signature_name", 
        "Organism" = "organism", 
        "Assay Type" = "assay_type", 
        "Sample Type" = "sample_type", 
        "Platform ID" = "platform", 
        "Phenotype" = "phenotype"
      ),
      multiple = TRUE,
      width = "100%"
    ),
    
    shiny::uiOutput(outputId = "search_inputs"),
    
    br(),
    
    shiny::actionButton(
      inputId = "search_signature",
      label = "Search",
      class = "mybutton"
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

