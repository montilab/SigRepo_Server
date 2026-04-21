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
#' @export
genesets_hypeR_UI <- function(id) {
  ns <- NS(id)
  
  tagList(
    div(
      class = "geneset-filter-group",
      tags$div(
        class = "geneset-filter-heading",
        tags$h4("Geneset Filters"),
        tags$p("Choose a species, collection, and subcollection before fetching genesets.")
      ),
      selectInput(
        ns("collection"),
        "Collection",
        choices = c(
          "Hallmark (H)" = "H",
          "Positional (C1)" = "C1",
          "Curated (C2)" = "C2",
          "Regulatory Target (C3)" = "C3",
          "Computational (C4)" = "C4",
          "Ontology (C5)" = "C5",
          "Oncogenic Signature (C6)" = "C6",
          "Immunologic Signature (C7)" = "C7",
          "Cell Type Signature (C8)" = "C8"
        ),
        selected = "H"
      ),
      uiOutput(ns("subcategory_ui")),
      div(
        class = "geneset-filter-actions",
        actionButton(ns("fetch_genesets"), "Fetch Genesets", class = "btn-primary"),
        uiOutput(ns("status"))
      )
    ),
    uiOutput(ns("geneset_summary"))
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
        as.character() |>
        (\(x) x[!is.na(x) & nzchar(x)])() |>
        stats::na.omit() |>
        unique()

      if (length(subcats) == 0) {
        return(
          selectInput(
            session$ns("subcategory"),
            "Subcollection",
            choices = c("No subcollection available" = ""),
            selected = ""
          )
        )
      }

      selectInput(
        session$ns("subcategory"),
        "Subcollection",
        choices = subcats,
        selected = subcats[[1]]
      )
    })
    
    # Reactive genesets list (updated on button press)
    reactive.genesets <- eventReactive(input$fetch_genesets, {
      req(input$collection)
      req(!is.null(input$subcategory))

      filtered_tbl <- msigdb_tbl() |>
        dplyr::filter(gs_collection == input$collection)

      if (!identical(input$subcategory, "")) {
        filtered_tbl <- filtered_tbl |>
          dplyr::filter(gs_subcollection == input$subcategory)
      }

      if (nrow(filtered_tbl) == 0) {
        showNotification("No genesets matched the selected filters.", type = "warning")
        return(list())
      }

      gs <- filtered_tbl |>
        (\(df) split(df, df$gs_name))() |>
        (\(lst) lapply(lst, function(x) unique(x$gene_symbol)))()
      
      if (clean) {
        names(gs) <- clean_genesets(names(gs))
      }
      
      gs
    })
    
    # Status message
    output$status <- renderUI({
      if (is.null(reactive.genesets()) || length(reactive.genesets()) == 0) {
        tags$div(
          class = "geneset-status geneset-status-pending",
          icon("circle-o", lib = "font-awesome"),
          tags$span("No genesets fetched")
        )
      } else {
        tags$div(
          class = "geneset-status geneset-status-ready",
          icon("check-circle", lib = "font-awesome"),
          tags$span(sprintf("%s genesets ready", length(reactive.genesets())))
        )
      }
    })

    output$geneset_summary <- renderUI({
      if (is.null(reactive.genesets()) || length(reactive.genesets()) == 0) {
        return(NULL)
      }

      tagList(
        tags$p(
          class = "geneset-summary-text",
          sprintf(
            "Loaded %s genesets for collection %s%s.",
            length(reactive.genesets()),
            input$collection,
            if (!identical(input$subcategory, "")) {
              sprintf(" / %s", input$subcategory)
            } else {
              ""
            }
          )
        )
      )
    })
    
    return(reactive.genesets)
  })
}
