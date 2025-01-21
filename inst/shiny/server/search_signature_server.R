
# Function to create search inputs for looking up a signature ####
create_search_input <- function(
    conn_handler,
    search_option,
    search_value
){
  
  if(search_option %in% "assay_type"){
    
    input_option <- shiny::selectizeInput(
      inputId = search_option,
      label = paste0(search_option, ":"),
      choices = c(
        "Transcriptomics" = "transcriptomics", 
        "Proteomics" = "proteomics", 
        "Metabolomics" = "metabolomics", 
        "methylomics" = "methylomics",
        "Genetic Variations" = "genetic_variations", 
        "DNA Binding Sites" = "dna_binding_sites"
      ),
      selected = search_value,
      width = "100%"
    )
    
  }else if(search_option %in% "signature_name"){
    
    signature_name_choices <- SigRepo::searchSignature(conn_handler = conn_handler)
    
    input_option <- shiny::selectizeInput(
      inputId = search_option,
      label = paste0(search_option, ":"),
      choices = unique(signature_name_choices$signature_name),
      selected = search_value,
      width = "100%",
    )
    
  }else if(search_option %in% "organism"){
    
    organism_choices <- SigRepo::searchOrganism(conn_handler = conn_handler)
    
    input_option <- shiny::selectizeInput(
      inputId = search_option,
      label = paste0(search_option, ":"),
      choices = unique(organism_choices$organism),
      selected = search_value,
      width = "100%",
    )
    
  }else if(search_option %in% "sample_type"){
    
    sample_type_choices <- SigRepo::searchSampleType(conn_handler = conn_handler)
    
    input_option <- shiny::selectizeInput(
      inputId = search_option,
      label = paste0(search_option, ":"),
      choices = unique(sample_type_choices$sample_type),
      selected = search_value,
      width = "100%",
    )
    
  }else if(search_option %in% "phenotype"){
    
    phenotype_choices <- SigRepo::searchPhenotype(conn_handler = conn_handler)
    
    input_option <- shiny::selectizeInput(
      inputId = search_option,
      label = paste0(search_option, ":"),
      choices = unique(phenotype_choices$phenotype),
      selected = search_value,
      width = "100%",
    ) 
    
  }else if(search_option %in% "platform"){
    
    platform_choices <- SigRepo::searchPlatform(conn_handler = conn_handler)
    
    input_option <- shiny::selectizeInput(
      inputId = search_option,
      label = paste0(search_option, ":"),
      choices = unique(platform_choices$platform_id),
      selected = search_value,
      width = "100%",
    )
    
  }
  
  return(input_option)
  
}

# Create search inputs ####
output$search_inputs <- renderUI({
  
  req(input$search_options, user_conn_handler())
  
  # Extract user connection
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Establish user connection
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Get search option inputs
  search_options <- shiny::isolate({ input$search_options })
  
  # Check if options are provided
  if(length(search_options) == 1){
    
    search_value <- shiny::isolate({ input[[search_options]] })
    
    input_options <- create_search_input(
      conn_handler = conn_handler,
      search_option = search_options,
      search_value = search_value
    ) %>% as.character()
    
  }else{
    
    input_options <- seq_along(search_options) %>% 
      purrr::map_chr(
        function(p){
          #p=1;
          search_value <- shiny::isolate({ input[[search_options[p]]] })
          
          if(p < length(search_options)){
            
            search_input <- create_search_input(
              conn_handler = conn_handler,
              search_option = search_options[p],
              search_value = search_value
            )
            
            filter_value <- shiny::isolate({ input[[paste0(search_options[p], "_filter")]] })
            
            filter_input <- shiny::radioButtons(
              inputId = paste0(search_options[p], "_filter"),
              label = NULL,
              choices = c("AND", "OR"),
              selected = filter_value,
              width = "100%",
              inline = TRUE
            )
            
            paste0(search_input, filter_input, sep="\n")
            
          }else{
            
            search_input <- create_search_input(
              conn_handler = conn_handler,
              search_option = search_options[p],
              search_value = search_value
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

shiny::observeEvent({
  input$search_signature
}, {
  
  req(user_conn_handler())
  
  # Get selected input
  search_options <- shiny::isolate({ input$search_options })
  
  if(length(search_options) == 0){
    search_sig_error_msg("Please select one of the search options above.")
    return(NULL)
  }
  
  # Get search option values
  search_option_values <- seq_along(search_options) %>% 
    purrr::map(
      function(p){
        #p=1;
        as.character(input[[search_options[p]]])
      }
    ) 
  
  # Get search option filter
  if(length(search_options) > 1){
    search_option_filter <- 1:(length(search_options)-1) %>% 
      purrr::map_chr(
        function(p){
          #p=1;
          as.character(input[[paste0(search_options[p], "_filter")]])
        }
      ) 
  }else{
    search_option_filter <- NULL
  }
  
  if(any(search_option_values %in% c("", NA))){
    search_sig_error_msg(sprintf("%s cannot be empty.", paste0(search_options[which(search_option_values %in% c("", NA))], collapse = ", ")))
    return(NULL)
  }
  
  # Reset message
  search_sig_error_msg(NULL)
  
  # Extract user connection
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Establish user connection
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Look up organism id ####
  if("organism" %in% search_options){
    
    lookup_organism <- input[["organism"]]
    
    organism_id <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "organisms",
      return_var = c("organism_id", "organism"),
      filter_coln_var = "organism",
      filter_coln_val = list("organism" = lookup_organism),
      check_db_table = TRUE
    )$organism_id
    
    print(organism_id)
    
    search_options[which(search_options == "organism")] <- "organism_id"
    search_option_values[which(search_options == "organism")] <- organism_id
    
  }
  
  # Look up phenotype id #####
  if("phenotype" %in% search_options){
    
    lookup_phenotype <- input[["phenotype"]]
    
    phenotype_id <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "phenotypes",
      return_var = c("phenotype", "phenotype_id"),
      filter_coln_var = "phenotype",
      filter_coln_val = list("phenotype" = lookup_phenotype),
      check_db_table = TRUE
    )$phenotype_id
    
    search_options[which(search_options == "phenotype")] <- "phenotype_id"
    search_option_values[which(search_options == "phenotype")] <- phenotype_id
    
  }
  
  # Look up platform id ####
  if("platform" %in% search_options){
    
    lookup_platform <- input[["platform"]]
    
    # SQL statement to look up platform in database
    platform_id <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "platforms",
      return_var = "platform_id",
      filter_coln_var = "platform_id",
      filter_coln_val = list("platform_id" = lookup_platform),
      check_db_table = TRUE
    )$platform_id
    
    search_options[which(search_options == "platform")] <- "platform_id"
    search_option_values[which(search_options == "platform")] <- platform_id
    
  }
  
  # Look up sample_type id ####
  if("sample_type" %in% search_options){
    
    lookup_sample_type <- input[["sample_type"]]
    
    sample_type_id <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "sample_types",
      return_var = c("sample_type", "sample_type_id"),
      filter_coln_var = "sample_type",
      filter_coln_val = list("sample_type" = lookup_sample_type),
      check_db_table = TRUE
    )$sample_type_id
    
    search_options[which(search_options == "sample_type")] <- "sample_type_id"
    search_option_values[which(search_options == "sample_type")] <- sample_type_id
    
  }
  
  names(search_option_values) <- search_options
  
  print(search_options)
  print(search_option_values)
  print(search_option_filter)
  
  # Look up signature table
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = search_options,
    filter_coln_val = search_option_values,
    filter_var_by = search_option_filter,
    check_db_table = TRUE
  )
  
  # Return table
  if(nrow(signature_tbl) == 0){
    search_signature_tbl(data.frame(WARNINGS = "THERE ARE NO DATA RETURNED FROM THE SEARCH PARAMETERS"))
  }else{
    search_signature_tbl(signature_tbl)
  }
  
  # Disconnect from database ####
  base::suppressMessages(DBI::dbDisconnect(conn))
  
})

output$search_sig_error_msg <- renderUI({
  
  req(search_sig_error_msg())
  
  p(class = "error-message", search_sig_error_msg())
  
})


output$signature_table <- DT::renderDataTable({
  
  req(search_signature_tbl())
  
  if("WARNINGS" %in% colnames(search_signature_tbl())){
    
    shiny::isolate({ search_signature_tbl() }) %>% 
      DT::datatable(
        rownames = FALSE,
        extensions = 'Buttons',
        selection = "single",
        options = list(
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          dom = 'T'
        )
      )
    
  }else{
    
    shiny::isolate({ search_signature_tbl() }) %>% 
      DT::datatable(
        filter = list(position = "top", clear = TRUE),
        rownames = FALSE,
        extensions = 'Buttons',
        selection = "none",
        options = list(
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          paging = TRUE,
          searching = TRUE,
          searchHighlight = TRUE,
          ordering = TRUE,
          pageLength = 20,
          scrollX = TRUE,
          scrollY = 400,
          dom = 'Bfrtip'
        )
      )
    
  }
  
})








