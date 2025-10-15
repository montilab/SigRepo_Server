# Collection page module


# Collection UI
collection_module_ui <- function(id) {
  ns <- NS(id)
  tagList(
    div(
      style = "margin-top: 70px;",
      actionButton(
        ns("open_upload_modal"),
        "Upload collection",
        icon = icon("upload"),
        class = "btn-primary"
      ),
      br(),
      
      uiOutput(ns("action_buttons")),
      DTOutput(ns("collection_tbl")),
      
      
    )
  )
}

collection_module_server <- function(id, collection_db, user_conn_handler, collection_trigger) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Reactive Vals
    
    selected_collection <- reactiveVal(NULL)
    collection_object <- reactiveVal(NULL)
    collection_trigger <- reactiveVal(NULL)
    
    current_collection <- reactive({
      req(collection_object())
      collection_object()[[1]]  # extract the first and only Omic Collection object
    })
    
    
    output$collection_tbl <- renderDT({
      
      df <- collection_db()
      
      # grouping collections for easier viewing 
      df_grouped <- df %>%
        group_by(
          collection_id,
          collection_name,
          description,
          user_name,
          date_created,
          visibility
        ) %>%
        summarise(signatures = paste(signature_name, collapse = ", "),
                  .groups = "drop")
      
      # util function for datatable
      
      DatatableFX(df = df_grouped,
                  scrollY = "500px",
                  row_selection = "single")
    })
    
    
  
    

    
    # Action Buttons UI ####
    output$action_buttons <- renderUI({
      req(input$collection_tbl_rows_selected)
      row <- input$collection_tbl_rows_selected
      df <- collection_db()
      collection_selected <- df[row, ]
      
      # updating the reactive val
      selected_collection(collection_selected)
      
      
      # action buttons
      tagList(
        h4(
          paste("Actions for Collection:", selected_collection()$collection_name)
        ),
        actionButton(ns("view_btn"), "View"),
        actionButton(ns("update_btn"), "Update"),
        actionButton(ns("delete_btn"), "Delete"),
        actionButton(ns("access_btn"), "Access"),
        downloadButton(ns("download_btn"), "Download")
      )
    })
    
    
    
    # === View Button Clicked ===
    observeEvent(input$download_btn, {
      req(selected_collection())
      
      collection_id <- selected_collection()$collection_id
      
      
      
  
    })
    
    #### Show Modal when collection is selected, different from signatures ####
    observeEvent(input$view_btn, {
     
      
      showModal(
        modalDialog(
          title = paste("Collection:", selected_collection()$collection_name),
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          tabsetPanel(
            tabPanel("Metadata",
                     uiOutput(ns("collection_metadata"))
            ),
            tabPanel("Signatures",
                     actionButton(ns("add_to_collection"), "Add to collection"),
                     DTOutput(ns("collection_sig_tbl"))
            ),
            
          )
        )
      )
    })
    
    
    # Metadata UI
    output$collection_metadata <- renderUI({
      req(selected_collection())
      
      tagList(
        p(strong("Description"), selected_collection()$description),
        p(strong("Date Created:"), selected_collection()$date_created),
        p(strong("User:"), selected_collection()$user_name),
        p(strong("Total Number of signatures:"), selected_collection()$signature_count)
        
      )
    })
    
    
    output$collection_sig_tbl <- renderDataTable({
      req(selected_collection(), collection_db())
      
      selected <- selected_collection() 
      
      filtered_collection_tbl <- collection_db()[
        collection_db()$collection_name == selected$collection_name,
      ]
      
      DatatableFX(
        filtered_collection_tbl,
        hidden_columns = c(3,4)
        
      )
    })
    
    
    
    ### upload collection logic
    observeEvent(input$open_upload_modal, {
      showModal(upload_modal_ui(ns, "Collection"))
    })
    
    
    observeEvent(input$upload_btn, {
      req(input$upload_file)
      
      tryCatch({
        rds_object <- readRDS(input$upload_file$datapath)
        
        SigRepo::addCollection(conn_handler = user_conn_handler(), omic_collection = rds_object)
        
        showNotification("Collection uploaded and added successfully!")
        
        
        # Trigger reactive update after upload
        update_trigger(isolate(update_trigger()) + 1)
        
      }, error = function(e) {
        showNotification(paste(
          "Error reading or uploading collection rds object",
          ":",
          e$message
        ),
        type = "error")
      })
      
      removeModal()
    })
    
    # delete signature logic ####
    
    observeEvent(input$delete_btn,{
      
      collection_id <- selected_collection()$collection_id
      
      showModal(modalDialog(
        title = "Confirm Delete",
        paste("Are you sure you want to delete Collection: ID", collection_id, "?"),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_collection"), "Delete", class = "btn-danger")
          
        )
      ))
      
      
    })
    
    # delete collection logic 
    
    observeEvent(input$confirm_delete_collection, {
      req(selected_collection())  # ensure selection exists
      
      collection_id <- selected_collection()$collection_id
      
      tryCatch({
        result <- SigRepo::deleteCollection(
          conn_handler = user_conn_handler(),
          collection_id = collection_id
        )
        
        showNotification("Collection deleted successfully.", type = "message")
        
        # close the modal
        removeModal()
        
        # Trigger table refresh
        collection_trigger(collection_trigger() + 1)
        
      }, error = function(e) {
        
        # Print to console for developer
        message("Error deleting collection: ", e$message)
        
        # Keep modal open on error and show error notification
        showNotification(
          paste("Failed to delete collection:", e$message),
          type = "error",
          duration = 8
        )
      })
    })
    
    
    # update collection logic ###
    
    observeEvent(input$update_btn, {
      
      collection_id <- selected_collection()$collection_id
      collection_name <- selected_collection()$collection_name
      
      showModal(modalDialog(
        title = "Update collection",
        paste("Collection to update:", collection_name),
        fileInput(ns("update_file_upload"), paste("Choose an RDS file"), accept = ".rds"),
        p("How it works: the selected collection will be updated with the new collection object you added to the file input"),
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })
    
    # add user logic
    
    observeEvent(input$access_btn, {
      
      collection_id <- selected_collection()$collection_id
      collection_name <- selected_collection()$collection_name
      
      user_tbl <- SigRepo::searchUser(conn_handler = user_conn_handler())
      
      showModal(modalDialog(
        title = paste("Manage Users for Collection:", collection_name),
        tabsetPanel(
          tabPanel("Add to Collection",
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
          tabPanel("Delete from Collection",
                   # Placeholder for delete logic
                   p("Delete user functionality goes here.")
          )
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
    })
    
    # download handler
    
    output$download_btn <- downloadHandler(
      filename = function() {
        paste0("collection_", selected_collection()$collection_name, ".rds")
      },
      content = function(file) {
        req(selected_collection())
        
        collection_object <- getCollection(
          conn_handler = user_conn_handler(),
          collection_id = selected_collection()$collection_id
        )
        
        saveRDS(collection_object, file)
      }
    )
    
    
    
    
  }) ## ending bracket
  
  
  
}
