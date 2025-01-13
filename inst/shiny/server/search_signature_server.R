

output$search_inputs <- renderUI({
  
  req(input$search_options)
  
  options <- isolate({ input$search_options })

  if(length(options) == 1){
    
    input_options <- textAreaInput(
      inputId = options,
      label = paste0(options[p], ":"),
      width = "100%",
      height = "30px"
    ) %>% as.character()
    
  }else{
    
    input_options <- seq_along(options) %>% 
      map_chr(
        function(p){
          #p=1;
          if(p < length(options)){
            
            text_area <- textInput(
              inputId = options[p],
              label = paste0(options[p], ":"),
              width = "100%"
            )
            
            filter <- radioButtons(
              inputId = paste0(options[p], "_filter"),
              label = NULL,
              choices = c("AND", "OR"),
              width = "100%",
              inline = TRUE
            )
            
            paste0(text_area, filter, sep="\n")
            
          }else{
            
            text_area <- textInput(
              inputId = options[p],
              label = paste0(options[p], ":"),
              width = "100%",
            ) %>% as.character()
            
          }
        }
      )
  }
  
  HTML(input_options)
  
})

# Create reactive values 
search_sig_error_msg <- reactiveVal()
search_signature_tbl <- reactiveVal()

observeEvent({
  input$search_signature
}, {
  
  req(user_conn_info())
  
  # Extract user connection
  conn_info <- isolate({ user_conn_info() })
  
  # Get selected input
  options <- isolate({ input$search_options })
  
  if(length(options) == 0){
    
    search_sig_error_msg("Please select one of the search options above.")
    return(NULL)
    
  }else{
    
    seq_along(options) %>% 
      purrr::walk(
        function(p){
          #p=1;
          input_value <- input[[options[p]]]
          if(input_value %in% c("", NA)){
            search_sig_error_msg(sprintf("%s cannot be empty.", options[p]))
            return(NULL)
          }
        }
      )  
    
  }
  
  # Look up organism id ####
  if("organism" %in% options){
    
    lookup_organism <- input[[options[p]]]
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn, 
      db_table_name = "organisms", 
      return_var = c("organism_id", "organism"), 
      filter_coln_var = "organism", 
      filter_coln_val = list("organism" = lookup_organism),
      check_db_table = TRUE
    ) 
  
  }
  
  # Look up phenotype id #####
  if("phenotype" %in% options){
    
    lookup_phenotype <- input[[options[p]]]
    
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "phenotypes", 
      return_var = c("phenotype", "phenotype_id"), 
      filter_coln_var = "phenotype", 
      filter_coln_val = list("phenotype" = lookup_phenotype),
      check_db_table = TRUE
    ) 

  }
  
  # Look up platform id ####
  if("platform" %in% options){
    
    lookup_platform <- input[[options[p]]]
    
    # SQL statement to look up platform in database
    platform_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "platforms", 
      return_var = "platform_id",
      filter_coln_var = "platform_id", 
      filter_coln_val = list("platform_id" = lookup_platform),
      check_db_table = TRUE
    ) 
    
  }
  
  # Look up sample_type id ####
  if("sample_type" %in% options){
    
    lookup_sample_type <- input[[options[p]]]
    
    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn_info$conn,
      db_table_name = "sample_types", 
      return_var = "sample_type_id", 
      filter_coln_var = "sample_type", 
      filter_coln_val = list("sample_type" = lookup_sample_type),
      check_db_table = TRUE
    ) 
    
  } 

  
})
  
output$search_sig_error_msg <- renderUI({
  
  req(search_sig_error_msg())
  
  p(class = "primary", search_sig_error_msg())
  
})


output$signature_table <- DT::renderDataTable({
  
  req(search_signature_tbl())
  
})








