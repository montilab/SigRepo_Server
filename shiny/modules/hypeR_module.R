# modules for hypeR 


msigdb_collection_metadata <- data.frame(
  gs_collection = c(
    "H", "C1",
    rep("C2", 8),
    rep("C3", 4),
    rep("C4", 3),
    rep("C5", 4),
    "C6",
    rep("C7", 2),
    "C8"
  ),
  gs_subcollection = c(
    "", "",
    "CGP", "CP", "CP:BIOCARTA", "CP:KEGG_LEGACY", "CP:KEGG_MEDICUS", "CP:PID", "CP:REACTOME", "CP:WIKIPATHWAYS",
    "MIR:MIRDB", "MIR:MIR_LEGACY", "TFT:GTRD", "TFT:TFT_LEGACY",
    "3CA", "CGN", "CM",
    "GO:BP", "GO:CC", "GO:MF", "HPO",
    "",
    "IMMUNESIGDB", "VAX",
    ""
  ),
  stringsAsFactors = FALSE
)


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
    selected_genesets <- reactiveVal(list())
    
    # Build subcategory selector
    output$subcategory_ui <- renderUI({
      req(input$collection)

      subcats <- msigdb_collection_metadata |>
        dplyr::filter(gs_collection == input$collection) |>
        dplyr::pull(gs_subcollection) |>
        (\(x) x[!is.na(x) & nzchar(x)])() |>
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

    observeEvent(
      list(species(), input$collection, input$subcategory),
      selected_genesets(list()),
      ignoreInit = TRUE
    )
    
    # Reactive genesets list (updated on button press)
    observeEvent(input$fetch_genesets, {
      req(species())
      req(input$collection)
      req(!is.null(input$subcategory))

      filtered_tbl <- msigdbr::msigdbr(species = species()) |>
        dplyr::select(
          gs_name,
          gs_collection,
          gs_subcollection,
          gene_symbol
        ) |>
        dplyr::filter(gs_collection == input$collection)

      if (!identical(input$subcategory, "")) {
        filtered_tbl <- filtered_tbl |>
          dplyr::filter(gs_subcollection == input$subcategory)
      }

      if (nrow(filtered_tbl) == 0) {
        showNotification("No genesets matched the selected filters.", type = "warning")
        selected_genesets(list())
        return()
      }

      gs <- filtered_tbl |>
        (\(df) split(df, df$gs_name))() |>
        (\(lst) lapply(lst, function(x) unique(x$gene_symbol)))()
      
      if (clean) {
        names(gs) <- clean_genesets(names(gs))
      }
      
      selected_genesets(gs)
    })
    
    # Status message
    output$status <- renderUI({
      gs <- selected_genesets()

      if (length(gs) == 0) {
        tags$div(
          class = "geneset-status geneset-status-pending",
          icon("circle-o", lib = "font-awesome"),
          tags$span("No genesets fetched")
        )
      } else {
        tags$div(
          class = "geneset-status geneset-status-ready",
          icon("check-circle", lib = "font-awesome"),
          tags$span(sprintf("%s genesets ready", length(gs)))
        )
      }
    })

    output$geneset_summary <- renderUI({
      gs <- selected_genesets()

      if (length(gs) == 0) {
        return(NULL)
      }

      tagList(
        tags$p(
          class = "geneset-summary-text",
          sprintf(
            "Loaded %s genesets for collection %s%s.",
            length(gs),
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
    
    return(reactive({
      selected_genesets()
    }))
  })
}
