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


fetch_msigdb_table <- function(species, collection, subcollection = "") {
  msigdbr_args <- list(species = species)
  msigdbr_formals <- names(formals(msigdbr::msigdbr))

  if ("collection" %in% msigdbr_formals) {
    msigdbr_args$collection <- collection
  } else if ("category" %in% msigdbr_formals) {
    msigdbr_args$category <- collection
  }

  if (!is.null(subcollection) && nzchar(subcollection)) {
    if ("subcollection" %in% msigdbr_formals) {
      msigdbr_args$subcollection <- subcollection
    } else if ("subcategory" %in% msigdbr_formals) {
      msigdbr_args$subcategory <- subcollection
    }
  }

  msigdb_tbl <- do.call(msigdbr::msigdbr, msigdbr_args)

  required_columns <- c("gs_name", "gs_collection", "gs_subcollection", "gene_symbol")
  missing_columns <- setdiff(required_columns, names(msigdb_tbl))

  if (length(missing_columns) > 0) {
    stop(
      sprintf(
        "MSigDB result is missing required columns: %s",
        paste(missing_columns, collapse = ", ")
      ),
      call. = FALSE
    )
  }

  msigdb_tbl |>
    dplyr::select(
      gs_name,
      gs_collection,
      gs_subcollection,
      gene_symbol
    )
}


msigdb_cache_dir <- function() {
  env_cache_dir <- Sys.getenv("MSIGDB_CACHE_DIR", unset = "")

  if (nzchar(env_cache_dir)) {
    return(env_cache_dir)
  }

  shiny_path <- getOption("sigrepo.shiny_path", default = "")
  server_root <- Sys.getenv("SIGREPO_SERVER_DIR", unset = "")

  cache_dir_candidates <- unique(c(
    if (nzchar(shiny_path)) file.path(shiny_path, "data", "msigdb_genesets"),
    if (nzchar(server_root)) file.path(server_root, "shiny", "data", "msigdb_genesets"),
    file.path(getwd(), "data", "msigdb_genesets"),
    file.path(getwd(), "shiny", "data", "msigdb_genesets"),
    file.path("data", "msigdb_genesets"),
    file.path("shiny", "data", "msigdb_genesets")
  ))

  existing_cache_dir <- cache_dir_candidates[dir.exists(cache_dir_candidates)]

  if (length(existing_cache_dir) > 0) {
    return(existing_cache_dir[[1]])
  }

  cache_dir_candidates[[1]]
}


msigdb_slugify <- function(x) {
  x <- gsub("[^A-Za-z0-9]+", "_", x)
  x <- gsub("_+", "_", x)
  gsub("^_|_$", "", x)
}


msigdb_cache_file <- function(species, collection, subcollection = "") {
  species_slug <- msigdb_slugify(species)
  subcollection_slug <- if (!is.null(subcollection) && nzchar(subcollection)) {
    msigdb_slugify(subcollection)
  } else {
    "all"
  }

  file.path(
    msigdb_cache_dir(),
    sprintf("%s__%s__%s.rds", species_slug, collection, subcollection_slug)
  )
}


load_cached_msigdb_genesets <- function(species, collection, subcollection = "") {
  cache_file <- msigdb_cache_file(species, collection, subcollection)

  if (!file.exists(cache_file)) {
    return(NULL)
  }

  readRDS(cache_file)
}


runtime_msigdb_fetch_allowed <- function() {
  value <- tolower(Sys.getenv("MSIGDB_ALLOW_RUNTIME_FETCH", unset = "false"))
  value %in% c("true", "1", "yes", "y")
}


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

      selected_genesets(list())

      cached_genesets <- tryCatch(
        load_cached_msigdb_genesets(
          species = species(),
          collection = input$collection,
          subcollection = input$subcategory
        ),
        error = function(err) {
          showNotification(
            sprintf("Failed to load cached genesets: %s", conditionMessage(err)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )

      if (!is.null(cached_genesets)) {
        if (clean) {
          names(cached_genesets) <- clean_genesets(names(cached_genesets))
        }

        selected_genesets(cached_genesets)
        showNotification("Loaded genesets from local cache.", type = "message")
        return()
      }

      if (!runtime_msigdb_fetch_allowed()) {
        showNotification(
          sprintf(
            "MSigDB cache file was not found: %s. Build the cache or set MSIGDB_CACHE_DIR.",
            msigdb_cache_file(species(), input$collection, input$subcategory)
          ),
          type = "error",
          duration = 12
        )
        return()
      }

      filtered_tbl <- tryCatch(
        {
          shiny::withProgress(
            message = "Fetching MSigDB genesets...",
            value = 0.25,
            {
              fetch_msigdb_table(
                species = species(),
                collection = input$collection,
                subcollection = input$subcategory
              )
            }
          )
        },
        error = function(err) {
          showNotification(
            sprintf("Failed to fetch genesets: %s", conditionMessage(err)),
            type = "error",
            duration = 10
          )
          NULL
        }
      )

      if (is.null(filtered_tbl)) {
        selected_genesets(list())
        return()
      }

      if (nrow(filtered_tbl) == 0) {
        showNotification("No genesets matched the selected filters.", type = "warning")
        selected_genesets(list())
        return()
      }

      gs <- split(filtered_tbl$gene_symbol, filtered_tbl$gs_name)
      gs <- lapply(gs, unique)
      
      if (clean) {
        names(gs) <- clean_genesets(names(gs))
      }
      
      selected_genesets(gs)
      rm(filtered_tbl)
      gc(verbose = FALSE)
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
