# r script for modals 


manage_users_modal_ui <- function(ns, name, user_tbl) {
  modalDialog(
    title = paste("Manage Users for :", name),
    size = "l",
    fluidPage(
      fluidRow(
        column(6,
               selectInput(
                 inputId = ns("user_selector"),
                 label = "Select a user to add:",
                 choices = user_tbl$user_name,
                 selected = NULL,
                 multiple = FALSE
               )
        )
      ),
      br(),
      fluidRow(
        column(12,
               uiOutput(ns("user_access_rows"))
        )
      ),
      br(),
      actionButton(ns("add_users_confirm"), "Add Users", class = "btn-primary")
    ),
    easyClose = TRUE,
    footer = modalButton("Close")
  )
}


manage_users_modal_server <- function(input, output, session, name, user_tbl, type, selected, user_conn_handler) {
  ns <- session$ns
  
  # reactive values to store single user and permission
  user_list <- reactiveVal(NULL)
  user_perms <- reactiveVal(NULL)
  
  # When Add User button is clicked in modal
  observeEvent(input$add_user_btn, {
    req(input$user_selector, input$user_perms)
    
    user_list(input$user_selector)
    user_perms(input$user_perms)
  })
  
  # Confirm add users
  observeEvent(input$add_users_confirm, {
    req(user_list(), user_perms())
    
    
    
    if (type == "Signature") {
      sig_id <- selected()$signature_id
      tryCatch({
        result <- SigRepo::addUserToSignature(
          conn_handler = user_conn_handler(),
          signature_id = sig_id,
          user_name = user_list(),
          access_type = user_perms()
        )
        showNotification("User(s) successfully added.", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding user:", e$message), type = "error")
      })
    }
    
    if(type == "Collection") {
      
      collection_id <- selected()$collection_id
      
      tryCatch({
        result <- SigRepo::addUserToSignature(
          conn_handler = user_con_handler(),
          collection_id = collection_id,
          user_name = user_list(),
          access_type = user_perms()
          )
        showNotification("User(s) successfully added.", type = "message")
        removeModal()
      }, error = function(e) {
        showNotification(paste("Error adding User(s):", e$message), type = "error")
      })
      
    }
    
  })
  
  
} # ending bracket
