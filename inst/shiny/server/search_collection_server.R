
# Create reactive values to store error messages #####
sig_colletion_error_msg <- shiny::reactiveVal()
search_collection_error_msg <- shiny::reactiveVal()

# Create reactive values to store data tables ####
collection_tbl <- shiny::reactiveVal()
download_omic_collection <- shiny::reactiveVal()

# Observe user_collection_tbl ####
shiny::observeEvent({
  user_collection_tbl()
}, {
  
  # Get user signature tbl
  user_collection_tbl <- shiny::isolate({ user_collection_tbl() })
  
  # Get a list of collection choices
  collection_name_choices <- unique(user_collection_tbl$collection_name)
  
  # Get the selected collection
  selected_collection_name <- shiny::isolate({ input$collection_name })
  
  if(length(selected_collection_name) == 0 || all(!selected_collection_name %in% collection_name_choices)){
    selected_collection_name <- collection_name_choices[1]
  }
  
  shiny::updateSelectizeInput(
    session = session,
    inputId = "collection_name",
    choices = collection_name_choices,
    selected = selected_collection_name
  )
  
}, ignoreNULL = FALSE, ignoreInit = FALSE)


# Observe search_collection
shiny::observeEvent({
  input$search_collection
}, {
  
  # Get selected input ####
  selected_collection_name <- shiny::isolate({ input$collection_name })
  
  if(length(selected_collection_name) == 0 || all(selected_collection_name %in% c("", NA))){
    search_collection_error_msg("Please select one of the search options above.")
    return(NULL)
  }
  
  # Get user collection table
  collection_tbl <- shiny::isolate({ user_collection_tbl() }) %>% 
    dplyr::filter(collection_name %in% selected_collection_name)
  
  if(nrow(collection_tbl) == 0){
    search_signature_tbl(base::data.frame(WARNINGS = "THERE ARE NO DATA RETURNED FROM THE SEARCH PARAMETERS"))
  }else{
    collection_tbl(collection_tbl)
  }
  
})


# Output collection_tbl ####
output$collection_tbl <- DT::renderDataTable({
  
  req(collection_tbl()) 
  
  if("WARNINGS" %in% colnames(collection_tbl())){
    
    shiny::isolate({ collection_tbl() }) %>% 
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
    
    # Get collection table
    collection_tbl <- shiny::isolate({ collection_tbl() }) %>% 
      dplyr::mutate(
        signature_name = base::sapply(seq_along(signature_name), function(i){ 
          base::sprintf("<a id='collection_signature_id' href='#' onclick=\"get_collection_signature_id(id='%s');\">%s</a>", signature_id[i], signature_name[i])
        })
      )
    
    # Create data table
    collection_tbl %>% 
      DT::datatable(
        filter = list(position = "top", clear = TRUE),
        caption = HTML("<p style='font-size:20px; font-weight:bold;'>Collection Table</p>"),
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
              filename = "collection_table",
              exportOptions = list(
                modifier = list(page = "all", columns = ":not(.no-export)")
              )
            ),
            list(
              extend = "excel",
              text = " EXCEL",
              className = "fas fa-download",
              filename = "collection_table",
              exportOptions = list(
                modifier = list(page = "all", columns = ":not(.no-export)")
              )
            )
          )
        )
      )
  }
  
})


# Search collection error message ####
output$sig_colletion_error_msg <- renderUI({
  
  req(sig_colletion_error_msg())
  
  shiny::p(class = "error-message", HTML(sig_colletion_error_msg()))
  
})








