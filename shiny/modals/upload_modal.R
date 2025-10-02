# script for upload modals




upload_modal_ui <- function(ns, type = c("Collection", "Signature")) {
  modalDialog(
    title = paste("Upload", type),
    
    fileInput(ns("upload_file"), paste("Choose", type, "RDS file"), accept = ".rds"),
    
    # Add visibility toggle
    radioButtons(
      inputId = ns("visibility"),
      label = "Set visibility:",
      choices = c("Private" = FALSE, "Public" = TRUE),
      selected = "private",
      inline = TRUE
    ),
    
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("upload_btn"), paste("Upload", type), class = "btn-primary")
    )
  )
}



upload_modal_server <- function(input,
                                output,
                                session,
                                type = c("Collection", "Signature"),
                                update_trigger,
                                conn_handler) {
  type <- match.arg(type)
  
  
  observeEvent(input$upload_btn, {
    req(input$upload_file)
    
    visibility <- as.logical(input$visibility)
    
    
    tryCatch({
      rds_object <- readRDS(input$upload_file$datapath)
      
      if (type == "Collection") {
        SigRepo::addCollection(conn_handler = conn_handler, omic_collection = rds_object, visibility = visibility)
        showNotification("Collection uploaded and added successfully!", type = "message")
      } else if (type == "Signature") {
        SigRepo::addSignature(conn_handler = conn_handler, signature = rds_object, visibility = visibility)
        showNotification("Signature uploaded and added successfully!", type = "message")
      }
      
      # Trigger reactive update after upload
      update_trigger(isolate(update_trigger()) + 1)
      
    }, error = function(e) {
      showNotification(paste(
        "Error reading or uploading",
        tolower(type),
        ":",
        e$message
      ),
      type = "error")
    })
    
    removeModal()
  })
}