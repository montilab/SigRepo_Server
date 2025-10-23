

# utils/DatatableFX.R

library(DT)

#' DatatableFX: A Customizable DT Wrapper
#'
#' @param df A data frame to render in a DataTable.
#' @param hidden_columns Integer vector of 0-based column indices to hide.
#' @param scrollY Vertical scroll height (default: "500px").
#' @param paging Logical, whether to enable pagination (default: FALSE).
#' @param row_selection Selection mode: "none", "single", or "multiple".
#'
#' @return A DT::datatable object.
#' @export
DatatableFX <- function(df,
                        hidden_columns = c(0, 6, 7, 8, 11, 14, 15, 16, 19, 24, 25, 26),
                        scrollY = "500px",
                        row_selection = "single",
                        rownames = FALSE) {
  
  # Check if df is valid
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(DT::datatable(
      data.frame(Message = "No data available."),
      class = "compact stripe hover nowrap",
      options = list(dom = 't'),  # minimal table
      rownames = FALSE
    ))
  }
  
  # Ensure hidden columns are within bounds
  max_index <- ncol(df) - 1  # 0-based indexing
  valid_hidden_columns <- hidden_columns[hidden_columns >= 0 & hidden_columns <= max_index]
  
  # Render the datatable
  DT::datatable(
    df,
    extensions = "Buttons",
    filter = "top",
    options = list(
      pageLength = 50,
      lengthMenu = c(10,25, 50, 100, 500, -1),
      scrollY = scrollY,
      scrollX = TRUE,
      paging = TRUE,
      ordering = FALSE,
      fixedHeader = TRUE,
      dom = 'frtipB',
      buttons = c('copy', 'csv', 'excel'),
      columnDefs = list(
        list(targets = valid_hidden_columns, visible = FALSE)
      )
    ),
    class = "compact stripe hover nowrap",
    selection = row_selection,
    rownames = rownames
  )
}

# === Reusable Delete Confirmation Modal ===
# delete_modal_server <- function(id, delete_id, delete_fn, update_trigger) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     selected_id <- reactiveVal(NULL)
#     
#     observeEvent(delete_id(), {
#       req(delete_id())
#       selected_id(delete_id())
#       
#       showModal(modalDialog(
#         title = "Confirm Delete",
#         paste("Are you sure you want to delete ID", delete_id(), "?"),
#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")
#         )
#       ))
#     })
#     
#     observeEvent(input$confirm_delete, {
#       req(selected_id())
#       tryCatch({
#         delete_fn(selected_id())
#         showNotification("Deleted successfully.", type = "message")
#         update_trigger(isolate(update_trigger()) + 1)
#       }, error = function(e) {
#         showNotification(paste("Error deleting:", e$message), type = "error")
#       })
#       
#       removeModal()
#     })
#   })
# }

# modals 




