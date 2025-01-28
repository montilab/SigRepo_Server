
# Create reactive values to store error messages #####
upload_sig_error_msg <- shiny::reactiveVal()

# Observe upload_signature ####
shiny::observeEvent({
  input$upload_signature
}, {
  
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
  base::tryCatch({
    SigRepo::addSignature(
      conn_handler = conn_handler,
      omic_signature = omic_signature
    )
  }, error = function(e){
    upload_sig_error_msg(paste0(e, "\n"))
    print(e, "\n")
    return(NULL)
  }, warning = function(w){
    print(w, "\n")
  }, finally = function(m){
    upload_sig_error_msg("Signature has been uploaded successfully.\n")
  })

  # Update user signature tbl
  promises::future_promise({
    SigRepo::searchSignature(conn_handler = conn_handler, user_name = conn_handler$user)
  }, package = "tidyverse") %...>% user_signature_tbl()
 
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Upload signature error message ####
output$upload_sig_error_msg <- renderUI({
  
  req(upload_sig_error_msg())
  
  shiny::p(class = "error-message", upload_sig_error_msg())
  
})



