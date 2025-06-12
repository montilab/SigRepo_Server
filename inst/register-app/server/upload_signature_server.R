
# Create reactive values to store error messages #####
upload_sig_error_msg <- shiny::reactiveVal()

# Observe upload_signature ####
shiny::observeEvent({
  input$upload_signature
}, {
  
  req(user_conn_handler())
  
  # Reset message
  upload_sig_error_msg(NULL)
  
  # Get file input
  inputfile <- shiny::isolate({ input$oms_file })

  # Check input file is valid
  if(is.null(inputfile)){
    upload_sig_error_msg("Please choose a 'OmicSignature' file to import.")
    return(NULL)
  }
  
  # Get file extension
  file_extension <-  tools::file_ext(inputfile$datapath)
  
  # Check file exension
  if(toupper(file_extension) == "RDS"){
    
    omic_signature <- base::readRDS(inputfile$datapath)
    
  }else{
    
    upload_sig_error_msg("Incorrect file format. Please check your 'OmicSignature' file again.")
    return(NULL)    
    
  }
  
  # Get user handler
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Upload signature
  upload_message <- base::tryCatch({
    SigRepo::addSignature(
      conn_handler = conn_handler,
      omic_signature = omic_signature
    )
  }, error = function(e){
    upload_sig_error_msg(gsub("\\n|\\t", "<br>", e, perl = TRUE))
    print(e, "\n")
    return(NULL)
  }, warning = function(w){
    print(w, "\n")
  }, message = function(m){
    print(m, "\n")
    return(base::gsub("\\n|\\t", "<br>", m, perl = TRUE))
  })
  
  # Update message
  upload_sig_error_msg(upload_message)

  # Update user signature tbl
  promises::future_promise({
    SigRepo::searchSignature(conn_handler = conn_handler, user_name = conn_handler$user)
  }, package = "tidyverse") %...>% user_signature_tbl()
 
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Upload signature error message ####
output$upload_sig_error_msg <- renderUI({
  
  req(upload_sig_error_msg())
  
  shiny::p(class = "error-message", HTML(upload_sig_error_msg()))
  
})



