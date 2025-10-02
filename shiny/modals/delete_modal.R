# delete modals for signature and collection

delete_modal_ui <- function(ns, name, type) {
  modalDialog(
    title = "Confirm Delete",
    sprintf(
      "Are you sure you want to delete %s %s?",
      htmltools::htmlEscape(type),
      htmltools::htmlEscape(name)
    ),
    easyClose = TRUE,
    footer = tagList(
      modalButton("Cancel"),
      actionButton(ns("confirm_delete_signature"), "Delete", class = "btn-danger")
    )
  )
}

# delete_modal_server <- function( input, output, session, name, type, id, user_conn_handler){
#   
#   
# }