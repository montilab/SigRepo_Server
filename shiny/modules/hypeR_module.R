# modules for hypeR 


# hypeR genests ui rewrite
#' Shiny UI for MSigDB subcategory selection
#'
#' @param id Shiny module id
#' @return Shiny UI elements
#'
#' @importFrom shiny NS tagList selectInput uiOutput
#' @export
#' Shiny UI for MSigDB subcategory selection with fetch button
#'
#' @param id Shiny module id
#' @return Shiny UI elements
#'
#' @importFrom shiny NS tagList selectInput uiOutput actionButton
#' @importFrom DT DTOutput
#' @export
genesets_hypeR_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    selectInput(
      ns("collection"),
      "MSigDB Collection",
      choices = c("H", "C1", "C2", "C3", "C4", "C5", "C6", "C7", "C8")
    ),
    
    uiOutput(ns("subcategory_ui")),
    
    actionButton(ns("fetch_genesets"), "Fetch Genesets"),
    
    DT::DTOutput(ns("genesets_table")),
    
    uiOutput(ns("status"))
  )
}


# hypeR genesets server logic

#' Shiny server for MSigDB subcategory selection with fetch button
#'
#' @param id Shiny module id
#' @param species Reactive species name (e.g. reactive("Homo sapiens"))
#' @param clean Logical; clean geneset names
#' @return Reactive named list of genesets
#'
#' @importFrom shiny moduleServer renderUI observeEvent reactive req icon
#' @importFrom DT renderDT datatable
#' @export
genesets_hypeR_Server <- function(id, species, clean = FALSE) {
  moduleServer(id, function(input, output, session) {
    
    # Load MSigDB table reactively
    msigdb_tbl <- reactive({
      req(species())
      msigdbr::msigdbr(species = species()) |>
        dplyr::select(
          gs_name,
          gs_collection,
          gs_subcollection,
          gene_symbol
        )
    })
    
    # Build subcategory selector
    output$subcategory_ui <- renderUI({
      req(input$collection)
      subcats <- msigdb_tbl() |>
        dplyr::filter(gs_collection == input$collection) |>
        dplyr::distinct(gs_subcollection) |>
        dplyr::pull(gs_subcollection) |>
        na.omit()
      
      if (length(subcats) == 0) return(NULL)
      
      selectInput(
        session$ns("subcategory"),
        "Subcategory",
        choices = subcats
      )
    })
    
    # Reactive genesets list (updated on button press)
    reactive.genesets <- eventReactive(input$fetch_genesets, {
      req(input$collection)
      req(input$subcategory)
      
      gs <- msigdb_tbl() |>
        dplyr::filter(
          gs_collection == input$collection,
          gs_subcollection == input$subcategory
        ) |>
        (\(df) split(df, df$gs_name))() |>
        (\(lst) lapply(lst, function(x) unique(x$gene_symbol)))()
      
      if (clean) {
        names(gs) <- clean_genesets(names(gs))
      }
      
      gs
    })
    
    # Show genesets in a DT table
    output$genesets_table <- DT::renderDT({
      gs <- reactive.genesets()
      req(gs)
      
      # Convert named list to a data.frame
      df <- data.frame(
        Geneset = names(gs),
        Genes   = sapply(gs, function(x) paste(x, collapse = ", ")),
        stringsAsFactors = FALSE
      )
      
      DT::datatable(
        df,
        options = list(
          scrollX = TRUE,
          scrollY = "500px",
          pageLength = 5,
          columnDefs = list(
            list(visible = FALSE, targets = 0)
          )
        )
      )
      
    })
    
    # Status icon
    output$status <- renderUI({
      if (is.null(reactive.genesets()) || length(reactive.genesets()) == 0) {
        icon("times-circle", lib = "font-awesome")
      } else {
        icon("check-circle", lib = "font-awesome")
      }
    })
    
    return(reactive.genesets)
  })
}

