
# Create reactive values to store error messages #####
search_sig_error_msg <- shiny::reactiveVal()

# Create reactive values to store data tables ####
search_signature_tbl <- shiny::reactiveVal()

# Observe search_options ####
shiny::observeEvent({
  user_signature_tbl()
  input$search_options
}, {
  
  # Get user signature tbl
  user_signature_tbl <- shiny::isolate({ user_signature_tbl() })
  
  # Get search option inputs
  search_options <- shiny::isolate({ input$search_options })
  
  # Update assay_type ####
  if("assay_type" %in% search_options){
    
    assay_type_choices <- unique(user_signature_tbl$assay_type)
    
    selected_value <- shiny::isolate({ input$assay_type })
    
    if(length(selected_value) == 0 || all(selected_value %in% c(NA, ""))){
      selected_value <- assay_type_choices[1]
    }
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "assay_type",
      choices = assay_type_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "assay_type")
    
  }
  
  # Update signature_name ####
  if("signature_name" %in% search_options){
    
    signature_name_choices <- unique(user_signature_tbl$signature_name)
    
    selected_value <- shiny::isolate({ input$signature_name })
    
    if(length(selected_value) == 0 || all(selected_value %in% c(NA, ""))){
      selected_value <- signature_name_choices[1]
    }
    
    input_option <- shiny::updateSelectizeInput(
      session = session,
      inputId = "signature_name",
      choices = signature_name_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "signature_name")
    
  }
  
  # Update organism ####
  if("organism" %in% search_options){
    
    organism_choices <- unique(user_signature_tbl$organism)
    
    selected_value <- shiny::isolate({ input$organism })
    
    if(length(selected_value) == 0 || all(selected_value %in% c(NA, ""))){
      selected_value <- organism_choices[1]
    }
    
    input_option <- shiny::updateSelectizeInput(
      session = session,
      inputId = "organism",
      choices = organism_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "organism")
    
  }
  
  # Update sample_type ####
  if("sample_type" %in% search_options){
    
    sample_type_choices <- unique(user_signature_tbl$sample_type)
    
    selected_value <- shiny::isolate({ input$sample_type })
    
    if(length(selected_value) == 0 || all(selected_value %in% c(NA, ""))){
      selected_value <- sample_type_choices[1]
    }
    
    shiny::updateSelectizeInput(
      inputId = "sample_type",
      choices = sample_type_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "sample_type")
    
  }
  
  # Update phenotype ####
  if("phenotype" %in% search_options){
    
    phenotype_choices <- unique(user_signature_tbl$phenotype)
    
    selected_value <- shiny::isolate({ input$phenotype })
    
    if(length(selected_value) == 0 || all(selected_value %in% c(NA, ""))){
      selected_value <- phenotype_choices[1]
    }
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "phenotype",
      choices = phenotype_choices,
      selected = selected_value
    ) 
    
    shinyjs::show(id = "phenotype")
    
  }
  
  # Update platform ####
  if("platform" %in% search_options){
    
    platform_choices <- unique(user_signature_tbl$platform_id)
    
    selected_value <- shiny::isolate({ input$platform })
    
    if(length(selected_value) == 0 || all(selected_value %in% c(NA, ""))){
      selected_value <- platform_choices[1]
    }
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "platform" ,
      choices = platform_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "platform")
    
  }
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Observe search_signature ####
shiny::observeEvent({
  input$search_signature
}, {
  
  req(input$search_options,user_conn_handler())
  
  # Get selected input ####
  search_options <- shiny::isolate({ input$search_options })
  
  if(length(search_options) == 0){
    search_sig_error_msg("Please select one of the search options above.")
    return(NULL)
  }
  
  # Get search option values ####
  search_option_values <- base::seq_along(search_options) %>% 
    purrr::map(
      function(p){
        #p=1;
        as.character(input[[search_options[p]]])
      }
    ) 
  
  # Check if any search inputs are empty ####
  if(any(search_option_values %in% c("", NA))){
    search_sig_error_msg(sprintf("%s cannot be empty.", paste0(search_options[which(search_option_values %in% c("", NA))], collapse = ", ")))
    return(NULL)
  }
  
  # Reset message ####
  search_sig_error_msg(NULL)
  
  # Extract user connection ####
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Establish user connection ####
  conn <- SigRepo::conn_init(conn_handler = conn_handler)
  
  # Create a search list
  search_option_list <- list()
  
  # Look up organism id ####
  if("organism" %in% search_options){
    
    lookup_organism <- input[["organism"]]
    
    organism_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "organisms",
      return_var = "*",
      filter_coln_var = "organism",
      filter_coln_val = list("organism" = lookup_organism),
      check_db_table = TRUE
    )
    
    search_values <- list("organism_id" = organism_id_tbl$organism_id)
    search_option_list <- c(search_option_list, search_values)
    
  }
  
  # Look up phenotype id #####
  if("phenotype" %in% search_options){
    
    lookup_phenotype <- input[["phenotype"]]
    
    phenotype_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "phenotypes",
      return_var = "*",
      filter_coln_var = "phenotype",
      filter_coln_val = list("phenotype" = lookup_phenotype),
      check_db_table = TRUE
    )
    
    search_values <- list("phenotype_id" = phenotype_id_tbl$phenotype_id)
    search_option_list <- c(search_option_list, search_values)
    
  }
  
  # Look up platform id ####
  if("platform" %in% search_options){
    
    lookup_platform <- input[["platform"]]
    
    # SQL statement to look up platform in database
    platform_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "platforms",
      return_var = "*",
      filter_coln_var = "platform_id",
      filter_coln_val = list("platform_id" = lookup_platform),
      check_db_table = TRUE
    )
    
    search_values <- list("platform_id" = platform_id_tbl$platform_id)
    search_option_list <- c(search_option_list, search_values)
    
  }
  
  # Look up sample_type id ####
  if("sample_type" %in% search_options){
    
    lookup_sample_type <- input[["sample_type"]]
    
    sample_type_id_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "sample_types",
      return_var = "*",
      filter_coln_var = "sample_type",
      filter_coln_val = list("sample_type" = lookup_sample_type),
      check_db_table = TRUE
    )
    
    search_values <- list("sample_type_id" = sample_type_id_tbl$sample_type_id)
    search_option_list <- c(search_option_list, search_values)
    
  }
  
  # Look up signature table
  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "signatures",
    return_var = "*",
    filter_coln_var = names(search_option_list),
    filter_coln_val = search_option_list,
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
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)


# Search signature error message ####
output$search_sig_error_msg <- renderUI({
  
  req(search_sig_error_msg())
  
  p(class = "error-message", search_sig_error_msg())
  
})

# Output signature_table ####
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








