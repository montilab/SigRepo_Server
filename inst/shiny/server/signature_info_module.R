# signature_info_module.R
# A Shiny module for displaying signature metadata and differential expression results

# UI Module ----
signatureInfoUI <- function(id) {
  ns <- NS(id)
  fluidPage(
    titlePanel("Signature Details"),
    fluidRow(
      column(
        width = 6,
        div(
          style = "overflow-y: auto; max-height: 500px; border: 1px solid #ddd; padding: 10px;",
          h4("Signature Metadata"),
          DT::dataTableOutput(ns("signature_info_table"))
        )
      ),
      column(
        width = 6,
        div(
          style = "overflow-y: auto; max-height: 500px; border: 1px solid #ddd; padding: 10px;",
          h4("Differential Expression"),
          DT::dataTableOutput(ns("signature_difexp_table"))
        )
      )
    )
  )
}

# Server Module ----
signatureInfoServer <- function(id, conn_handler, sig_id, sig_name) {
  moduleServer(id, function(input, output, session) {
    
    # Load full signature object
    signature_obj <- reactive({
      req(conn_handler(), sig_id(''), sig_name(''))
      SigRepo::getSignature(
        conn_handler = conn_handler(),
        signature_id = sig_id(''),
        signature_name = sig_name('')
      )
    })
    
    # Extract metadata
    signature_metadata <- reactive({
      req(signature_obj())
      sig_data <- signature_obj()$metadata
      validate(need(!is.null(sig_data), "Signature data not found."))
      sig_data
    })
    
    # Extract difexp
    signature_difexp <- reactive({
      req(signature_obj())
      sig_data <- signature_obj()$difexp
      validate(need(!is.null(sig_data$difexp), "Differential expression data not found."))
      sig_data
    })
    
    # Render metadata table
    output$signature_info_table <- DT::renderDataTable({
      DT::datatable(
        signature_metadata(),
        options = list(pageLength = 5, scrollX = TRUE),
        rownames = FALSE
      )
    })
    
    # Render differential expression table
    output$signature_difexp_table <- DT::renderDataTable({
      DT::datatable(
        signature_difexp(),
        options = list(pageLength = 10, scrollX = TRUE),
        rownames = FALSE
      )
    })
  })
}
