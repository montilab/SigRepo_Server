fluidRow(
  width = 12,
  id = "search-signature-wrapper",
  style = "border: 1px solid gray; padding: 20px 20px 20px 20px;",
  
  column(
    width = 4,
    style = "border: 1px solid gray; padding: 20px 20px 20px 20px;",
    
    selectizeInput(
      inputId = "search_options",
      label = "Search Options:",
      choices = c("Choose from a list of options below"="", "signature_name", "organism", "assay_type", "sample_type", "platform", "phenotype"),
      multiple = TRUE,
      width = "100%"
    ),
    
    uiOutput(outputId = "search_inputs"),
    br(),
    
    actionButton(
      inputId = "search_signature",
      label = "Search",
      class = "mybutton"
    ), 
    
    br(), br(),
    
    uiOutput(outputId = "search_sig_error_msg")
    
  ),
  
  column(
    width = 8,
    style = "border: 1px solid gray; padding: 20px 20px 20px 20px;",
    
    DT::dataTableOutput("signature_table")
    
  )
)

