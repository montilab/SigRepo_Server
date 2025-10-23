# modules/test_module.R

test_module_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    h3("Signature DB Test Output"),
    DT::dataTableOutput(ns("sig_table"))
  )
}

test_module_server <- function(id, signature_db) {
  moduleServer(id, function(input, output, session) {
    
    output$sig_table <- DT::renderDataTable({
      df <- signature_db()
      req(nrow(df) > 0)
      DT::datatable(df, options = list(pageLength = 10, scrollX = TRUE))
    })
    
  })
}

