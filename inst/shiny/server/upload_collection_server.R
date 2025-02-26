
# Create reactive values to store error messages #####
upload_collection_error_msg <- shiny::reactiveVal()

# Observe upload_collection ####
shiny::observeEvent({
  input$upload_collection
}, {
  
  req(user_conn_handler())
  
  # Reset message
  upload_collection_error_msg(NULL)
  
  # Get file input
  inputfile <- shiny::isolate({ input$oms_file })

  # Check input file is valid
  if(is.null(inputfile)){
    upload_collection_error_msg("Please choose a 'Omiccollection' file to import.")
    return(NULL)
  }
  
  # Get file extension
  file_extension <-  tools::file_ext(inputfile$datapath)
  
  # Check file exension
  if(toupper(file_extension) == "RDS"){
    
    omic_collection <- base::readRDS(inputfile$datapath)
    
  }else{
    
    upload_collection_error_msg("Incorrect file format. Please check your 'Omiccollection' file again.")
    return(NULL)    
    
  }
  
  # Get user handler
  conn_handler <- shiny::isolate({ user_conn_handler() })
  
  # Upload collection
  upload_message <- base::tryCatch({
    SigRepo::addcollection(
      conn_handler = conn_handler,
      omic_collection = omic_collection
    )
  }, error = function(e){
    upload_collection_error_msg(gsub("\\n|\\t", "<br>", e, perl = TRUE))
    print(e, "\n")
    return(NULL)
  }, warning = function(w){
    print(w, "\n")
  }, message = function(m){
    print(m, "\n")
    return(base::gsub("\\n|\\t", "<br>", m, perl = TRUE))
  })
  
  # Update message
  upload_collection_error_msg(upload_message)

  # Update user collection tbl
  promises::future_promise({
    SigRepo::searchcollection(conn_handler = conn_handler, user_name = conn_handler$user)
  }, package = "tidyverse") %...>% user_collection_tbl()
 
}, ignoreNULL = TRUE, ignoreInit = TRUE)

# Upload collection error message ####
output$upload_collection_error_msg <- renderUI({
  
  req(upload_collection_error_msg())
  
  shiny::p(class = "error-message", HTML(upload_collection_error_msg()))
  
})



