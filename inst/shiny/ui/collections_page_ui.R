# ui page for signature collections

shiny::div(
  class = "search-collection-container",
  shiny::fluidRow(
    class = "search-collection-wrapper",
    
    shiny::column(
      width = 12,
      class = "search-collection-title",
      shiny::p("Search Collections")
    ),
    
    shiny::column(
      width = 4,
      class = "search-collection-sidebar",
      shiny::selectizeInput(
        inputId = "collection_options",
        label = "Search Options:",
        choices = c(
          "choose from a list of options below" = "",
          "collection_name",
          "organism",
          "phenotype",
          "sample_type",
          "platform_id",
          "assay_type"
        ),
        multiple = TRUE,
        width = "100%"
      ),
      shinyjs::hidden(
      shiny::selectizeInput(
        inputId = "collection_name",
        label = "collection_name",
        choices = c("Please choose from an option below" = ""),
        multiple = TRUE,
        width = "100%",
      )
    ),
    
  )
)
)