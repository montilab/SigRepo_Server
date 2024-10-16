div(
  id = "search-collection-wrapper",
  fluidRow(
    column(
      width = 12,
      style = "border: 1px solid gray; padding: 0;",
      selectizeInput(
        inputId = "signature_collection",
        label = "Signature Collections:",
        choices = c("Collection 1"),
        selected = "signature_name",
        multiple = TRUE,
        width = "50%"
      ),
      actionButton(
        inputId = "search_collection",
        label = "Search",
        width = "auto"
      )
    )
  ),
  fluidRow(
    column(
      width = 6,
      DT::dataTableOutput("collection_table")
    ),
    column(
      width = 6,
      DT::dataTableOutput("collection_signature_info")
    )
  )
)
  
