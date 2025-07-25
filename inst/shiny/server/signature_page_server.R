# Signature Page Server Logic ####
signaturesServer <- function(id){
  moduleServer(id, function(input, output, session){
# Signature table refresh trigger
signature_update_trigger <- reactiveVal(0)

# Load all user-accessible/public signatures (reactive)
signature_db <- reactive({
  
  req(user_conn_handler())
 
  
  signature_update_trigger()  # Triggers re-evaluation
  tryCatch({
    df <- SigRepo::searchSignature(conn_handler = user_conn_handler)
    validate(need(nrow(df) > 0, "No signatures found."))
    df
  }, error = function(e) {
    showNotification(paste("Error fetching signatures:", e$message), type = "error")
    data.frame()
  })
})

# Reactive value to store filtered signatures
filtered_signatures <- reactiveVal()
search_sig_error_msg <- reactiveVal()

# Initialize filtered table with all records
observe({
  req(user_conn_handler())
  
  sigs <- signature_db()
  filtered_signatures(sigs)
})

# Upload Panel Server Logic ####

upload_sig_error_msg <- shiny::reactiveVal()

observeEvent(input$upload_btn_signature, {
  req(user_conn_handler())
  upload_sig_error_msg("")
  
  inputfile <- shiny::isolate({ input$upload_file_signature })
  
  if (is.null(inputfile)) {
    upload_sig_error_msg("Please choose an 'OmicSignature' file to import.")
    return(NULL)
  }
  
  file_extension <- tools::file_ext(inputfile$datapath)
  
  if (toupper(file_extension) != "RDS") {
    upload_sig_error_msg("Incorrect file format. Please check your 'OmicSignature' file again.")
    return(NULL)
  }
  
  omic_signature <- base::readRDS(inputfile$datapath)
  conn_handler <- shiny::isolate({ user_conn_handler() })
  # for minimal version this works, but I need to have the option for the user to make their signature public or not in the GUI 
  upload_message <- base::tryCatch({
    SigRepo::addSignature(conn_handler = conn_handler, omic_signature = omic_signature, return_signature_id = TRUE, visibility = TRUE, verbose = TRUE)
  }, error = function(e) {
    msg <- gsub("\\n|\\t", "<br>", conditionMessage(e), perl = TRUE)
    upload_sig_error_msg(msg)
    message("Upload error: ", conditionMessage(e))
    return(NULL)
  }, warning = function(w) {
    message("Upload warning: ", conditionMessage(w))
  }, message = function(m) {
    message("Upload message: ", conditionMessage(m))
    return(gsub("\\n|\\t", "<br>", conditionMessage(m), perl = TRUE))
  })
  
  upload_sig_error_msg(upload_message)
  
  if (!is.null(upload_message) && upload_message != "") {
    signature_update_trigger(signature_update_trigger() + 1)
  }
}, ignoreNULL = TRUE, ignoreInit = TRUE)

output$upload_sig_error_msg <- renderUI({
  req(upload_sig_error_msg())
  shiny::p(class = "error-message", HTML(upload_sig_error_msg()))
})

# Filter logic ####

observeEvent(input$search_options_signature, {
  req(user_conn_handler())
  
  all_fields <- c("signature_name", "organism", "phenotype", "sample_type", "platform_id", "assay_type")
  selected_fields <- input$search_options_signature
  
  for (f in all_fields) {
    if (f %in% selected_fields) {
      shinyjs::show(f)
    } else {
      shinyjs::hide(f)
      updateSelectizeInput(session, f, choices = "", selected = NULL)
    }
  }
  
  sigs <- signature_db()
  if (nrow(sigs) > 0) {
    if ("signature_name" %in% selected_fields)
      updateSelectizeInput(session, "signature_name", choices = unique(sigs$signature_name), server = TRUE)
    if ("organism" %in% selected_fields)
      updateSelectizeInput(session, "organism", choices = unique(sigs$organism), server = TRUE)
    if ("phenotype" %in% selected_fields)
      updateSelectizeInput(session, "phenotype", choices = unique(sigs$phenotype), server = TRUE)
    if ("sample_type" %in% selected_fields)
      updateSelectizeInput(session, "sample_type", choices = unique(sigs$sample_type), server = TRUE)
    if ("platform_id" %in% selected_fields)
      updateSelectizeInput(session, "platform_id", choices = unique(sigs$platform_id), server = TRUE)
    if ("assay_type" %in% selected_fields)
      updateSelectizeInput(session, "assay_type", choices = unique(sigs$assay_type), server = TRUE)
  }
}, ignoreNULL = FALSE)

observeEvent(input$search_signature, {
  tryCatch({
    
    conn_handler <- user_conn_handler()
    req(conn_handler)  # Ensure connection is ready

    args <- list(conn_handler = conn_handler)
    
    if (!is.null(input$signature_name) && input$signature_name != "")
      args$signature_name <- input$signature_name
    if (!is.null(input$organism) && input$organism != "")
      args$organism <- input$organism
    if (!is.null(input$phenotype) && input$phenotype != "")
      args$phenotype <- input$phenotype
    if (!is.null(input$sample_type) && input$sample_type != "")
      args$sample_type <- input$sample_type
    if (!is.null(input$platform_id) && input$platform_id != "")
      args$platform_id <- input$platform_id
    if (!is.null(input$assay_type) && input$assay_type != "")
      args$assay_type <- input$assay_type
    
    df <- do.call(SigRepo::searchSignature, args)
    
    if (nrow(df) == 0) {
      search_sig_error_msg("No matching signatures found.")
    } else {
      search_sig_error_msg(NULL)
    }
    
    filtered_signatures(df)
  }, error = function(e) {
    search_sig_error_msg(paste("Error searching signatures:", e$message))
    filtered_signatures(data.frame())
  })
})

output$search_sig_error_msg <- renderUI({
  req(search_sig_error_msg())
  p(class = "error-message", HTML(search_sig_error_msg()))
})

output$signature_tbl <- DT::renderDataTable({
  df <- filtered_signatures()
  req(!is.null(df), nrow(df) > 0)
  
  df$signature_name <- sprintf(
    '<a href="signature_info?sig_id=%s&sig_name=%s" target="_blank">%s</a>',
    df$signature_id,
    URLencode(df$signature_name, reserved = TRUE),  # encode to handle spaces/special chars
    df$signature_name
  )
  
  
  
  
  current_user <- user_conn_handler()$user
  owner_col_index <- which(colnames(df) == "user_name") - 1
  
  DT::datatable(
    df,
    escape = FALSE,
    colnames = c(
      "Signature Name" = "signature_name",
      "Organism" = "organism",
      "Phenotype" = "phenotype",
      "Sample Type" = "sample_type",
      "Platform" = "platform_id",
      "Assay Type" = "assay_type",
      "Visibility" = "visibility",
      "Keywords" = "keywords",
      "Cutoff Description" = "cutoff_description",
      "Others" = "others"
    ),
    options = list(
      pageLength = 10,
      scrollX = TRUE,
      columnDefs = list(
        list(
          targets = which(names(df) %in% c("signature_hashkey", "visibility", "others", "keywords", "cutoff_description")) - 1,
          visible = FALSE
        )
      ),
      rowCallback = DT::JS(sprintf("
        function(row, data, index) {
          if (data[%d] === '%s') {
            $('td', row).css('background-color', '#d9edf7');  // light blue
          }
        }", owner_col_index, current_user))
    ),
    rownames = FALSE,
    class = "nowrap"
  )
})


observeEvent(input$clear_filters, {
  updateSelectInput(session, "search_options_signature", selected = character(0))
  all_fields <- c("signature_name", "organism", "phenotype", "sample_type", "platform_id", "assay_type")
  for (f in all_fields) {
    shinyjs::hide(f)
    updateSelectizeInput(session, f, choices = "", selected = NULL)
  }
  filtered_signatures(signature_db())
  search_sig_error_msg(NULL)
})

# Delete Server Logic ####

# Update delete dropdown whenever filtered_signatures or signature_db changes
observe({
  sigs <- filtered_signatures()
  all_sigs <- signature_db()
  
  sigs_to_show <- if (!is.null(sigs) && nrow(sigs) > 0) sigs else all_sigs
  
  if (is.null(sigs_to_show) || nrow(sigs_to_show) == 0) {
    updateSelectInput(session, "delete_sig", choices = c("No signatures available" = ""), selected = NULL)
    return()
  }
  
  delete_choices <- setNames(
    as.character(sigs_to_show$signature_id),
    paste0(sigs_to_show$signature_name, " (ID: ", sigs_to_show$signature_id, ")")
  )
  
  updateSelectInput(session, "delete_sig", choices = delete_choices, selected = NULL)
})

# Server logic for deleting a signature from the database

observeEvent(input$delete_btn, {
  req(input$delete_sig)
  
  # get the connection handler reactively
  conn_handler <- user_conn_handler()
  req(conn_handler)  # make sure connection is ready
  
  tryCatch({
    SigRepo::deleteSignature(conn_handler = conn_handler,
                             signature_id = input$delete_sig)
    
    showNotification(paste("Signature", input$delete_sig, "deleted successfully."), type = "message")
    
    # refresh the database and the dropdowns
    signature_update_trigger(signature_update_trigger() + 1)
  }, error = function(e) {
    showNotification(paste("Error deleting signature:", e$message), type = "error")
  })
})


# update signature logic ####

update_sig_error_msg <- shiny::reactiveVal()

observe({
  sigs <- filtered_signatures()
  all_sigs <- signature_db()
  
  
  sigs_to_show <- if(!is.null(sigs) && nrow(sigs) > 0) sigs else all_sigs
  
  if (is.null(sigs_to_show) || nrow(sigs_to_show) == 0) {
    updateSelectInput(session, "update_sig", choices = c("No signatures available" = ""), selected = NULL)
    return()
  }
  # Update the update_sig selectInput choices
  update_choices <- setNames(
    as.character(sigs_to_show$signature_id),
    paste0(sigs_to_show$signature_name, " (ID: ", sigs_to_show$signature_id, ")")
  )
  # updatign the selectinput in the ui
  updateSelectInput(session, "update_sig", choices = update_choices, selected = NULL)
})

# update observe button
observeEvent(input$update_btn, {
  update_sig_error_msg("")# clearing previous errors
  req(input$update_sig)
  req(input$update_sig_file)
  
  conn_handler <- user_conn_handler()
  req(conn_handler)
  
  file <- input$update_sig_file
  file_ext <- tools::file_ext(file$datapath)
  
  if (toupper(file_ext) != "RDS"){
    update_sig_error_msg("Invalid file format, please uplaod an RDS file containg your updated signature object")
    return(NULL)
  }
  
  omic_signature <- tryCatch({
    readRDS(file$datapath)
  }, error = function(e) {
    update_sig_error_msg("Failed to read uploaded RDS file.")
    return(NULL)
  })
  
  # Exit early if file reading failed
  if (is.null(omic_signature)) return(NULL)
  
  tryCatch({
    SigRepo::updateSignature(
      conn_handler = conn_handler,
      signature_id = input$update_sig,
      omic_signature = omic_signature,
      visibility = TRUE,  # need to make this dynamic for the user GUI 
      verbose = TRUE
    )
    
    showNotification("Signature updated successfully.", type = "message")
    signature_update_trigger(signature_update_trigger() + 1)
    
  }, error = function(e) {
    update_sig_error_msg(paste("Error updating signature:", e$message))
  })
})

# Render error message
output$update_sig_error_msg <- renderUI({
  req(update_sig_error_msg())
  p(class = "error-message", HTML(update_sig_error_msg()))
})

# Donload Omic Signature Button Logic ####

# Store selected RDS file paths
selected_rds_files <- reactiveVal(NULL)

observeEvent(input$download_oms_handler, {
  selected_rows <- input$signature_tbl_rows_selected
  sig_list <- signature_store()
  all_ids <- names(sig_list)
  
  if (length(selected_rows) == 0) {
    showNotification("❌ Please select at least one signature to download.", type = "error")
    return()
  }
  
  # Prepare temp files for selected signatures
  temp_dir <- tempdir()
  rds_paths <- c()
  
  for (i in selected_rows) {
    sig_id <- all_ids[i]
    sig_obj <- sig_list[[sig_id]]
    sig_name <- sig_obj$metadata$signature_name
    # Make safe filename
    safe_name <- gsub("[^a-zA-Z0-9_\\-]", "_", sig_name)
    file_path <- file.path(temp_dir, paste0(safe_name, ".rds"))
    saveRDS(sig_obj, file_path)
    rds_paths <- c(rds_paths, file_path)
  }
  
  selected_rds_files(rds_paths)  # Save to be used in downloadHandler
  # Trigger download automatically? You could programmatically click a hidden button here if needed
})

output$download_oms_zip <- downloadHandler(
  filename = function() {
    paste0("OmicSignatures_", Sys.Date(), ".zip")
  },
  content = function(file) {
    files_to_zip <- selected_rds_files()
    if (is.null(files_to_zip) || length(files_to_zip) == 0) {
      stop("No signatures prepared for download.")
    }
    zip::zip(zipfile = file, files = files_to_zip, mode = "cherry-pick", flags = "-j")
  }
)})
}



