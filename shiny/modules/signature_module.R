# Signature page module


# Signatures UI
signature_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-top: 70px;",
      actionButton(
        ns("open_upload_modal"),
        "Upload Signature",
        icon = icon("upload"),
        class = "btn-primary"
      ),
      br(),
      
      uiOutput(ns("action_buttons")),
      DTOutput(ns("signature_tbl")),
      
    
    )
  )
}

signature_module_server <- function(id, signature_db, user_conn_handler, signature_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Vals
    
    selected_sig <- reactiveVal(NULL)
    sig_object <- reactiveVal(NULL)
    signature_update_trigger <- reactiveVal(NULL)
   
    current_sig <- reactive({
      req(sig_object())
      sig_object()[[1]]  # extract the first and only OmicSignature object
    })
    
    
    

    
    # main signature_tbl
    output$signature_tbl <- renderDT({
      df <- signature_db()
      
      # util function for datatable
      
      DatatableFX(
        df = df,
        hidden_columns = c(0, 6, 7, 8, 9, 11,12, 14, 15, 16,18, 19,21,22, 24, 25, 26),
        scrollY = "500px",
        paging = TRUE,
        row_selection = "single"
      )
    })
    
  # signature_tbl
    
    output$signature_file_table <- DT::renderDataTable({
      
      DatatableFX(current_sig()$signature,
                  scrollY = "500px", paging = TRUE)
    })
  
  # difexp_tbl
    
    output$difexp_file_table <- DT::renderDataTable({
      
      DatatableFX(current_sig()$difexp,
                  scrollY = "500px",
                  paging = TRUE)
    })
    
    
    # Action Buttons UI ####
    output$action_buttons <- renderUI({
      req(input$signature_tbl_rows_selected)
      row <- input$signature_tbl_rows_selected
      df <- signature_db()
      signature_selected <- df[row, ]
      
      # updating the reactive val
      selected_sig(signature_selected)
      
      
      # action buttons
      tagList(
        h4(
          paste("Actions for Signature:", selected_sig()$signature_name)
        ),
        actionButton(ns("view_btn"), "View"),
        actionButton(ns("update_btn"), "Update"),
        actionButton(ns("delete_btn"), "Delete"),
        actionButton(ns("access_btn"), "Access"),
        downloadButton(ns("download_btn"), "Download")
      )
    })
    
   
    
    # === View Button Clicked ===
    observeEvent(input$view_btn, {
      req(selected_sig())
      
      sig_id <- selected_sig()$signature_id
      
      # Fetch the signature
      sig <- SigRepo::getSignature(
        conn_handler = user_conn_handler(),
        signature_id = sig_id
      )
      str(sig)
      
      # Store in reactiveVal
      sig_object(sig)
      
    
      
    })
      
    #### Show Modal when sig_object is ready ####
    observeEvent(sig_object(), {
      req(current_sig())
      
      showModal(
        modalDialog(
          title = paste("Signature:", selected_sig()$signature_name),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          tabsetPanel(
            tabPanel("Metadata",
                     uiOutput(ns("signature_metadata"))
            ),
            tabPanel("Signature",
                     DTOutput(ns("signature_file_table"))
            ),
            tabPanel("Differential Expression",
                     DTOutput(ns("difexp_file_table"))
            )
          )
        )
      )
    })
     
    
    # Metadata UI
    output$signature_metadata <- renderUI({
      req(selected_sig())
      
      tagList(
        p(strong("Description"), selected_sig()$description),
        p(strong("Organism:"), selected_sig()$organism),
        p(strong("Direction Type:"), selected_sig()$direction_type),
        p(strong("Assay Type:"), selected_sig()$assay_type),
        p(strong("Phenotype:"), selected_sig()$phenotype),
        p(strong("Platform:"), selected_sig()$platform_name),
        p(strong("Sample Type:"), selected_sig()$sample_type),
        p(strong("Covariates:"), selected_sig()$covariates),
        p(strong("Score Cutoff:"), selected_sig()$score_cutoff),
        p(strong("LogFC Cutoff:"), selected_sig()$logfc_cutoff),
        p(strong("Date Created:"), selected_sig()$date_created),
        p(strong("User:"), selected_sig()$user_name),
        p(strong("Number of diff. expressed"))
        
      )
    })
    
    
  
    

    ### upload signature logic
    observeEvent(input$open_upload_modal, {
      showModal(upload_modal_ui(ns, "Signature"))
    })
    
    
    observeEvent(input$upload_btn, {
      req(input$upload_file)
      
      tryCatch({
        rds_object <- readRDS(input$upload_file$datapath)
        
        SigRepo::addSignature(conn_handler = user_conn_handler(), omic_signature = rds_object)
        
        showNotification("Signature uploaded and added successfully!")
        
        
        # Trigger reactive update after upload
        update_trigger(isolate(update_trigger()) + 1)
        
      }, error = function(e) {
        showNotification(paste(
          "Error reading or uploading signature rds object",
          ":",
          e$message
        ),
        type = "error")
      })
      
      removeModal()
    })
    
    # delete signature logic ####
    
    observeEvent(input$delete_btn,{
      
      sig_id <- selected_sig()$signature_id
      
      showModal(modalDialog(
        title = "Confirm Delete",
        paste("Are you sure you want to delete signature ID", sig_id, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_signature"), "Delete", class = "btn-danger")
          
        )
      ))
      
      
    })
    
    # delete signature logic 
    
    observeEvent(input$confirm_delete_signature, {
      req(selected_sig())  # ensure selection exists
      
      sig_id <- selected_sig()$signature_id
      
      tryCatch({
        result <- SigRepo::deleteSignature(
          conn_handler = user_conn_handler(),
          signature_id = sig_id
        )
        
        showNotification("Signature deleted successfully.", type = "message")
        
        # close the modal
        removeModal()
        
        # Trigger table refresh
        signature_trigger(signature_trigger() + 1)
        
      }, error = function(e) {
        
        # Print to console for developer
        message("Error deleting signature: ", e$message)
        
        # Keep modal open on error and show error notification
        showNotification(
          paste("Failed to delete signature:", e$message),
          type = "error",
          duration = 8
        )
      })
    })
    
    
    # update signature logic ###
    
    observeEvent(input$update_btn, {
      
      sig_id <- selected_sig()$signature_id
      sig_name <- selected_sig()$signature_name
      
      showModal(modalDialog(
        title = "Update signature",
        paste("Signature to update:", sig_name),
        fileInput(ns("update_file_upload"), paste("Choose an RDS file"), accept = ".rds"),
        p("How it works: the selected signature will be updated with the new signature object you added to the file input"),
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })
    
    # add user logic
    
    observeEvent(input$access_btn, {
      
      sig_id <- selected_sig()$signature_id
      sig_name <- selected_sig()$signature_name
      
      user_tbl <- SigRepo::searchUser(conn_handler = user_conn_handler())
      
      showModal(modalDialog(
        title = paste("Manage Users for Signature:", sig_name),
        tabsetPanel(
          tabPanel("Add to Signature",
                   fluidRow(
                     column(6,
                            selectInput(
                              inputId = "user_selector",
                              label = "Select users to add:",
                              choices = user_tbl$user_name ,     # Replace with your actual function or vector
                              multiple = TRUE
                            )
                     )
                   ),
                   uiOutput("access_type_ui"), # Dynamic access dropdowns
                   actionButton("add_users_confirm", "Add Users", class = "btn-primary")
          ),
          tabPanel("Delete from Signature",
                   # Placeholder for delete logic
                   p("Delete user functionality goes here.")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
 # download omic signature object 
    
    output$download_btn <- downloadHandler(
      filename = function() {
        paste0("signature_", selected_sig()$signature_name, ".rds")
      },
      content = function(file) {
        req(selected_sig())
        
        sig_object <- getSignature(
          conn_handler = user_conn_handler(),
          signature_id = selected_sig()$signature_id
        )
        
        saveRDS(sig_object, file)
      }
    )
    
    
    
    
    
  }) ## ending bracket
  
  
  
}
