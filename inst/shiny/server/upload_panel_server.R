# Signature Page Server Logic #### 


# Reactive value to store messages (success or error)
upload_sig_error_msg <- reactiveVal()

# Upload Button Logic
observeEvent(input$upload_btn, {
  req(input$upload_file)
  req(user_conn_handler())
  
  # Reset previous message
  upload_sig_error_msg(NULL)
  
  inputfile <- isolate(input$upload_file)
  file_extension <- tools::file_ext(inputfile$datapath)
  
  if (toupper(file_extension) != "RDS") {
    msg <- "Invalid file format. Please upload a .RDS OmicSignature file."
    upload_sig_error_msg(msg)
    showNotification(msg, type = "error")
    return(NULL)
  }
  
  # Attempt to read and upload
  omic_signature <- tryCatch({
    readRDS(inputfile$datapath)
  }, error = function(e) {
    msg <- paste("Error reading file:", e$message)
    upload_sig_error_msg(msg)
    showNotification(msg, type = "error")
    return(NULL)
  })
  
  req(!is.null(omic_signature))  # Stop if reading failed
  
  conn_handler <- isolate(user_conn_handler())
  
  # Upload the signature
  upload_message <- tryCatch({
    result <- SigRepo::addSignature(
      conn_handler = conn_handler,
      omic_signature = omic_signature
    )
    as.character(result)  # Ensure return value is character
  }, error = function(e) {
    msg <- gsub("\\n|\\t", "<br>", e$message)
    upload_sig_error_msg(msg)
    showNotification("Upload failed: check below for details.", type = "error")
    return(NULL)
  }, message = function(m) {
    msg <- gsub("\\n|\\t", "<br>", m$message)
    return(as.character(msg))
  })
  
  # Show success message
  if (!is.null(upload_message)) {
    upload_sig_error_msg(upload_message)
    showNotification("Upload successful!", type = "message")
  }
  
  # Update the user's signature table in the background
  promises::future_promise({
    SigRepo::searchSignature(conn_handler = conn_handler, user_name = conn_handler$user)
  }) %...>% user_signature_tbl()
  
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# UI Output for Message 8
output$upload_sig_error_msg <- renderUI({
  msg <- upload_sig_error_msg()
  req(!is.null(msg), nzchar(msg))
  p(class = "error-message", HTML(as.character(msg)))  # HTML-safe
})
