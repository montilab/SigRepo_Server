# Signature page module


# Signatures UI
signature_module_ui <- function(id) {
  ns <- NS(id)
  

  tagList(
    
    div(
      style = "margin-top: 70px;",
      shiny::actionButton(
        ns("open_upload_modal"),
        "Upload Signature",
        icon = icon("upload"),
        class = "btn-primary"
      ),
      br(),
      h4(shiny::textOutput(ns("selected_signature_label"))),
      

      shiny::div(
        id = ns("action_buttons_group_toggle"),
        style = "display: block;",  # Initial state is hidden
        shiny::actionButton(ns("view_btn"), "View"),
        shiny::actionButton(ns("update_btn"), "Update"),
        shiny::actionButton(ns("delete_btn"), "Delete"),
        shiny::actionButton(ns("access_btn"), "Access"),
        shiny::downloadButton(ns("download_btn"), "Download")
      ),
      
      DT::DTOutput(ns("signature_tbl"))
    )
  )
}



signature_module_server <- function(id, signature_db, user_conn_handler, signature_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Vals for signature 
    
    selected_sig <- reactiveVal(NULL)
    sig_object <- reactiveVal(NULL)
    signature_update_trigger <- reactiveVal(NULL)
    
    # reactive vales for user acces and perms
    
    users_to_add <- reactiveVal(NULL)
    user_perms <- reactiveVal(NULL)
    
   
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
    
    # deugging
    observeEvent(input$signature_tbl_rows_selected, {
      print("Row selected")
      
    })
    
  # using _rows_selected in shiny DT package
    observeEvent(input$signature_tbl_rows_selected, {
      shinyjs::show(ns(id = "action_buttons_group_toggle"))
      print("toggle block")
    })
    
  observeEvent(input$signature_tbl_rows_selected, {
  
    
    df <- signature_db()
    row <- input$signature_tbl_rows_selected
    sig <- df[row,]
    selected_sig(sig)
    
    # shinyjs::toggle(ns("action_buttons_group"))
    
    output$selected_signature_label <- renderText({
      sprintf("Actions for: %s", sig$signature_name)
    })
    
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
      
      
      sig <- selected_sig()
      
      # transpose row
      df <- data.frame(
        Field = names(sig),
        Value = unlist(sig[1,], use.names = FALSE),
        stringsAsFactors = FALSE
      )
      
      
      
      
      # rendering to a datatable
      
      DatatableFX(df,
                  rownames = TRUE)
     
    })
    
    
    
  
    

    ### upload signature logic
    observeEvent(input$open_upload_modal, {
      showModal(upload_modal_ui(ns, type = "Signature"))
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
      
      sig_name <- selected_sig()$signature_name
      
      # delete signature modal
      showModal(
      delete_modal_ui(ns, type = "Signature", name = sig_name)
      )
      
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
    
    # manage user modal
    
    observeEvent(input$access_btn, {
      
      sig_id <- selected_sig()$signature_id
      sig_name <- selected_sig()$signature_name
      user_tbl <- SigRepo::searchUser(conn_handler = user_conn_handler())
      
      # manage user modal for signature
      showModal(manage_users_modal_ui(ns, name = sig_name, user_tbl = user_tbl))
      
    })
    
    # add users to signature with their perms
    
    observeEvent(input$confirm_add_users, {
      
      # name, user_tbl, type, selected, user_conn_handler
      
      manage_users_modal_server(ns, input, output, session, name = sig_name, type = "Signature", user_tbl = user_tbl, user_conn_handler = user_conn_handler )
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
