
# Create reactive values to store error messages #####
search_sig_error_msg <- shiny::reactiveVal()
sig_tbl_error_msg <- shiny::reactiveVal()

# Create reactive values to store data tables ####
search_signature_tbl <- shiny::reactiveVal()
sig_up_regulated_tbl <- shiny::reactiveVal()
sig_down_regulated_tbl <- shiny::reactiveVal()
download_omic_signature <- shiny::reactiveVal()

# Observe search_options ####
shiny::observeEvent({
  input$search_options
  user_signature_tbl()
}, {
  
  # Get user signature tbl
  user_signature_tbl <- shiny::isolate({ user_signature_tbl() })
  
  # Get search option inputs
  search_options <- shiny::isolate({ input$search_options })
  
  # Update assay_type ####
  if("assay_type" %in% search_options){
    
    assay_type_choices <- unique(user_signature_tbl$assay_type)
    
    selected_value <- shiny::isolate({ input$assay_type })
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "assay_type",
      choices = assay_type_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "assay_type")
    
  }else{
    
    shinyjs::hide(id = "assay_type")
    
  }
  
  # Update signature_name ####
  if("signature_name" %in% search_options){
    
    signature_name_choices <- unique(user_signature_tbl$signature_name)
    
    selected_value <- shiny::isolate({ input$signature_name })
    
    input_option <- shiny::updateSelectizeInput(
      session = session,
      inputId = "signature_name",
      choices = signature_name_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "signature_name")
    
  }else{
    
    shinyjs::hide(id = "signature_name")
    
  }
  
  # Update organism ####
  if("organism" %in% search_options){
    
    organism_choices <- unique(user_signature_tbl$organism)
    
    selected_value <- shiny::isolate({ input$organism })

    input_option <- shiny::updateSelectizeInput(
      session = session,
      inputId = "organism",
      choices = organism_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "organism")
    
  }else{
    
    shinyjs::hide(id = "organism")
    
  }
  
  # Update sample_type ####
  if("sample_type" %in% search_options){
    
    sample_type_choices <- unique(user_signature_tbl$sample_type)
    
    selected_value <- shiny::isolate({ input$sample_type })
    
    shiny::updateSelectizeInput(
      inputId = "sample_type",
      choices = sample_type_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "sample_type")
    
  }else{
    
    shinyjs::hide(id = "sample_type")
    
  }
  
  # Update phenotype ####
  if("phenotype" %in% search_options){
    
    phenotype_choices <- unique(user_signature_tbl$phenotype)
    
    selected_value <- shiny::isolate({ input$phenotype })
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "phenotype",
      choices = phenotype_choices,
      selected = selected_value
    ) 
    
    shinyjs::show(id = "phenotype")
    
  }else{
    
    shinyjs::hide(id = "phenotype")
    
  }
  
  # Update platform ####
  if("platform_id" %in% search_options){
    
    platform_choices <- unique(user_signature_tbl$platform_id)
    
    selected_value <- shiny::isolate({ input$platform_id })
    
    shiny::updateSelectizeInput(
      session = session,
      inputId = "platform_id" ,
      choices = platform_choices,
      selected = selected_value
    )
    
    shinyjs::show(id = "platform_id")
    
  }else{
    
    shinyjs::hide(id = "platform_id")
    
  }
  
}, ignoreNULL = FALSE, ignoreInit = FALSE)

# Observe search_signature ####
shiny::observeEvent({
  input$search_signature
}, {
  
  # Get user signature tbl
  signature_tbl <- shiny::isolate({ user_signature_tbl() })
  
  # Get selected input ####
  search_options <- shiny::isolate({ input$search_options })
  
  if(length(search_options) == 0 || all(search_options %in% c("", NA))){
    search_sig_error_msg("Please select one of the search options above.")
    return(NULL)
  }
  
  # Check if any search inputs are empty ####
  for(p in base::seq_along(search_options)){
    #p=1;
    val <- as.character(input[[search_options[p]]])
    if(length(val) == 0){
      search_sig_error_msg(sprintf("%s cannot be empty.", search_options[p]))
      return(NULL)
    }
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
  names(search_option_values) <- search_options
  
  # Reset message ####
  search_sig_error_msg(NULL)
  
  # Filter table with given search variables
  for(r in base::seq_along(search_option_values)){
    #r=1;
    filter_var <- names(search_option_values)[r]
    filter_val <- search_option_values[[r]]
    signature_tbl <- signature_tbl %>% dplyr::filter(trimws(tolower(!!!syms(filter_var))) %in% trimws(tolower(filter_val)))
  }
  
  # Return table
  if(nrow(signature_tbl) == 0){
    search_signature_tbl(base::data.frame(WARNINGS = "THERE ARE NO DATA RETURNED FROM THE SEARCH PARAMETERS"))
  }else{
    signature_tbl %>% 
      dplyr::mutate(
        entry = 1:nrow(.),
        select = base::sapply(base::seq_along(entry), function(r){ base::sprintf('<label class="checkbox-control"><input type="checkbox" name="sig_tbl_select_row" id="%s" value="%s"></label>', paste0("sig_tbl_select_row_", r), r) })
      ) %>% 
      dplyr::select(
        entry, select, everything()
      ) %>% 
      search_signature_tbl()
  }
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Search signature error message ####
output$search_sig_error_msg <- renderUI({
  
  req(search_sig_error_msg())
  
  shiny::p(class = "error-message", HTML(search_sig_error_msg()))
  
})

# Output signature_table ####
output$signature_tbl <- DT::renderDataTable({
  
  req(search_signature_tbl())
  
  if("WARNINGS" %in% colnames(search_signature_tbl())){
    
    shiny::isolate({ search_signature_tbl() }) %>% 
      DT::datatable(
        rownames = FALSE,
        extensions = 'Buttons',
        selection = "none",
        options = list(
          columnDefs = list(
            list(className = "dt-center", targets = "_all")
          ),
          dom = 'T'
        )
      )
    
  }else{
    
    # Show download_oms
    shinyjs::delay(1000, shinyjs::show(id = "download_oms"))
    
    # Get signature table
    signature_tbl <- shiny::isolate({ search_signature_tbl() }) %>% 
      dplyr::mutate(
        num_up_regulated = base::sapply(seq_along(signature_id), function(i){ 
          ifelse(has_difexp[i] == 1, base::sprintf("<a id='num_up_regulated' href='#' onclick=\"get_up_regulated_id(id='%s');\">%s</a>", signature_id[i], num_up_regulated[i]), num_up_regulated[i])
        }),
        num_down_regulated = base::sapply(seq_along(signature_id), function(i){ 
          ifelse(has_difexp[i] == 1, base::sprintf("<a id='num_down_regulated' href='#' onclick=\"get_down_regulated_id(id='%s');\">%s</a>", signature_id[i], num_down_regulated[i]), num_down_regulated[i])
        })
      )
    
    # Get no export column numbers
    no_export_columns <- which(colnames(signature_tbl) %in% c("entry", "select"))
    
    # Add overall checkbox to check all boxes
    colnames(signature_tbl)[which(colnames(signature_tbl) %in% "select")] <- sprintf('<label class="checkbox-control"><input type="checkbox" name="sig_tbl_select_all" id="sig_tbl_select_all" onclick="sig_tbl_select_all();"></label>')
    
    # Create data table
    signature_tbl %>% 
      DT::datatable(
        filter = list(position = "top", clear = TRUE),
        escape = FALSE,
        rownames = FALSE,
        extensions = 'Buttons',
        selection = "single",
        options = list(
          columnDefs = list(
            list(className = "dt-center", targets = "_all"),
            list(className = "no-export", targets = as.numeric(no_export_columns)),
            list(orderable = FALSE, targets = 1)
          ),
          searchHighlight = TRUE,
          searching = TRUE,
          ordering = TRUE,
          deferRender = FALSE,
          paging = TRUE,
          pageLength = 20,
          scroller = TRUE,
          scrollX = TRUE,
          scrollY = 400,
          dom = 'Bfrtip',
          buttons = list(
            list(
              extend = "csv",
              text = " CSV",
              className = "fas fa-download",
              filename = "signature_table",
              exportOptions = list(
                modifier = list(page = "all", columns = ":not(.no-export)")
              )
            ),
            list(
              extend = "excel",
              text = " EXCEL",
              className = "fas fa-download",
              filename = "signature_table",
              exportOptions = list(
                modifier = list(page = "all", columns = ":not(.no-export)")
              )
            )
          )
        )
      )
    
  }
  
})

# Observe download_oms ####
shiny::observeEvent({
  input$download_oms
}, {
  
  # Get the selected rows
  selected_rows <- shiny::isolate({ input$sig_tbl_selected_rows })
  
  # Check if any rows are selected
  if(length(selected_rows) == 0){
    sig_tbl_error_msg("Please check a list of signatures above to download.")
    return(NULL)
  }
  
  # Reset message
  sig_tbl_error_msg(NULL)
  
  # Get user handler
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Get selected signatures
  signature_tbl <- shiny::isolate({ search_signature_tbl() }) %>% dplyr::filter(entry %in% selected_rows)
  
  # Create a list of selected signature objects
  omic_signatures <- base::tryCatch({
    SigRepo::getSignature(conn_handler = conn_handler, signature_name = signature_tbl$signature_name)
  }, error = function(e){
    sig_tbl_error_msg(paste0(e, "\n"))
    print(e, "\n")
    return(NULL)
  }, warning = function(w){
    print(w, "\n")
  }, message = function(m){
    print(m, "\n")
  }) 
  
  # If omic_signatures is empty, escape the function
  if(is.null(omic_signatures)) return(NULL)
  
  # Update omic signature objects
  download_omic_signature(omic_signatures) 
    
  # Trigger the download button
  shinyjs::runjs("alert('here');document.getElementById('download_oms_handler').click();")
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)


# Signature tbl error message ####
output$sig_tbl_error_msg <- renderUI({
  
  req(sig_tbl_error_msg())
  
  shiny::HTML("<p class = 'error-message'>", sig_tbl_error_msg(), "</p>")
  
})

# Download handler ####
output$download_oms_handler <- shiny::downloadHandler(
  
  filename = function(){
    paste0("omic-signature-", base::Sys.Date(), ".RDS")
  },
  
  content = function(file){
    base::saveRDS(download_omic_signature(), file)
  }
  
)

# Observe sig_up_regulated_id #####
shiny::observeEvent({
  input$sig_up_regulated_id
}, {
  
  req(search_signature_tbl(), user_conn_handler())
  
  # Get user handler
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Establish connection to DB
  conn <- conn_init(conn_handler = conn_handler)
  
  # Get input
  signature_id <- shiny::isolate({ input$sig_up_regulated_id })
  
  # Get signature metadata tbl
  metadata_tbl <- shiny::isolate({ search_signature_tbl() }) %>% 
    dplyr::filter(signature_id %in% !!signature_id)
  
  # Get signature feature set table
  signature <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signature_feature_set", 
    return_var = "*", 
    filter_coln_var = "signature_id",
    filter_coln_val = list("signature_id" = signature_id),
    check_db_table = TRUE
  ) 
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Get number of up regulated features
  up_regulated_tbl <- signature %>% dplyr::filter(direction %in% "+")

  # Update table
  sig_up_regulated_tbl(up_regulated_tbl)
  
})


# Observe sig_down_regulated_id #####
shiny::observeEvent({
  input$sig_down_regulated_id
}, {
  
  req(search_signature_tbl(), user_conn_handler())
  
  # Get user handler
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Establish connection to DB
  conn <- conn_init(conn_handler = conn_handler)
  
  # Get input
  signature_id <- shiny::isolate({ input$sig_down_regulated_id })
  
  # Get signature metadata tbl
  metadata_tbl <- shiny::isolate({ search_signature_tbl() }) %>% 
    dplyr::filter(signature_id %in% !!signature_id)
  
  # Get signature feature set table
  signature <- SigRepo::lookup_table_sql(
    conn = conn, 
    db_table_name = "signature_feature_set", 
    return_var = "*", 
    filter_coln_var = "signature_id",
    filter_coln_val = list("signature_id" = signature_id),
    check_db_table = TRUE
  ) 
  
  # Disconnect from database ####
  base::suppressWarnings(DBI::dbDisconnect(conn))
  
  # Get number of down regulated features
  down_regulated_tbl <- signature %>% dplyr::filter(direction %in% "-")
  
  # downdate table
  sig_down_regulated_tbl(down_regulated_tbl)
  
})

# Output sig_up_regulated_tbl ####
output$sig_up_regulated_tbl <- DT::renderDataTable({
  
  req(sig_up_regulated_tbl()) %>% 
    DT::datatable(
      filter = list(position = "top", clear = TRUE),
      caption = HTML("<p style='font-size:20px; font-weight:bold;'>Up Regulated Features Table</p>"),
      escape = FALSE,
      rownames = FALSE,
      extensions = 'Buttons',
      selection = "single",
      options = list(
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        ),
        searchHighlight = TRUE,
        searching = TRUE,
        ordering = TRUE,
        deferRender = FALSE,
        paging = TRUE,
        pageLength = 20,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = 400,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = "csv",
            text = " CSV",
            className = "fas fa-download",
            filename = "up_regulated_table",
            exportOptions = list(
              modifier = list(page = "all", columns = ":not(.no-export)")
            )
          ),
          list(
            extend = "excel",
            text = " EXCEL",
            className = "fas fa-download",
            filename = "up_regulated_table",
            exportOptions = list(
              modifier = list(page = "all", columns = ":not(.no-export)")
            )
          )
        )
      )
    )
  
})

# Output sig_down_regulated_tbl ####
output$sig_down_regulated_tbl <- DT::renderDataTable({
  
  req(sig_down_regulated_tbl()) %>% 
    DT::datatable(
      filter = list(position = "top", clear = TRUE),
      caption = HTML("<p style='font-size:20px; font-weight:bold;'>Down Regulated Features Table</p>"),
      escape = FALSE,
      rownames = FALSE,
      extensions = 'Buttons',
      selection = "single",
      options = list(
        columnDefs = list(
          list(className = "dt-center", targets = "_all")
        ),
        searchHighlight = TRUE,
        searching = TRUE,
        ordering = TRUE,
        deferRender = FALSE,
        paging = TRUE,
        pageLength = 20,
        scroller = TRUE,
        scrollX = TRUE,
        scrollY = 400,
        dom = 'Bfrtip',
        buttons = list(
          list(
            extend = "csv",
            text = " CSV",
            className = "fas fa-download",
            filename = "down_regulated_table",
            exportOptions = list(
              modifier = list(page = "all", columns = ":not(.no-export)")
            )
          ),
          list(
            extend = "excel",
            text = " EXCEL",
            className = "fas fa-download",
            filename = "down_regulated_table",
            exportOptions = list(
              modifier = list(page = "all", columns = ":not(.no-export)")
            )
          )
        )
      )
    )
  
})








