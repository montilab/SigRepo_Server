# server logic for delete signautre subpanel.


# delete signature logic


observeEvent(input$delete_btn, {
  
  # required inputs 
  req(input$delete_sig)
      
  tryCatch({
    SigRepo::deleteSignature(input$delete_sig, conn_handler = conn, verbose = TRUE)
  })
  
})