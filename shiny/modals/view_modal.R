# functions for the view modal 


view_modal_ui <- function(ns, selected) {
  
  modalDialog(
    title = paste("Signature:", selected()$signature_name),
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
  
  
}