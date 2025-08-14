# Signature Page Server Logic ####
signaturesServer <- function(id){
   moduleServer(id, function(input, output, session){
     
    ns <- session$ns
# Signature table refresh trigger
signature_update_trigger <- reactiveVal(0)

# Load all user-accessible/public signatures (reactive)
signature_db <- reactive({
  

  
  signature_update_trigger()  # Triggers re-evaluation
  tryCatch({
    df <- SigRepo::searchSignature(conn_handler = conn_handler)
    validate(need(nrow(df) > 0, "No signatures found."))
    df
  }, error = function(e) {
    showNotification(paste("Error fetching signatures:", e$message), type = "error")
    data.frame()
  })
})

})
}
