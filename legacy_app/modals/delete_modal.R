# The delete confirmation the Signature tab opens. The caller observes
# confirm_delete_signature and does the deleting.

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
