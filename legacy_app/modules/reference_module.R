reference_module_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("reference_page"),
    class = "sr-page",
    div(
      class = "sr-page-inner",

      tags$header(
        class = "page-titlebar",
        div(
          class = "page-titlebar-text",
          tags$h1("Reference browser"),
          span(
            class = "page-titlebar-sub",
            "Search the transcriptomic and proteomic reference features, then inspect a record."
          )
        )
      ),

      div(
        class = "sr-columns sr-columns-narrow-first",
        div(
          class = "sr-column",
          div(
            class = "card",
            tags$h3("Search filters"),
            p(
              class = "sr-helper",
              "Choose an organism and assay, then optionally narrow the results to one feature name."
            ),
            div(
              class = "sr-form-grid",
              selectInput(
                inputId = ns("ref_organism"),
                label = "Organism",
                choices = c("Homo sapiens", "Mus musculus"),
                selected = "Homo sapiens"
              ),
              selectInput(
                inputId = ns("ref_assay"),
                label = "Assay",
                choices = c(
                  "Transcriptomics" = "transcriptomics",
                  "Proteomics" = "proteomics"
                ),
                selected = "transcriptomics"
              )
            ),
            textInput(
              inputId = ns("feature_name"),
              label = "Feature name",
              placeholder = "Optional: filter to a specific feature"
            ),
            actionButton(
              ns("search_ref_btn"),
              "Search features",
              class = "btn-primary",
              icon = icon("magnifying-glass")
            )
          )
        ),
        div(
          class = "sr-column",
          div(
            class = "card",
            div(
              class = "sr-toolbar",
              tags$h3("Search results"),
              uiOutput(ns("results_status"))
            ),
            uiOutput(ns("results_summary")),
            DT::DTOutput(ns("ref_feature_tbl"))
          ),
          div(
            class = "card",
            tags$h3("Selected feature"),
            p(
              class = "sr-helper",
              "Selecting a row loads the feature's full record below."
            ),
            uiOutput(ns("feature_detail_panel"))
          )
        )
      )
    )
  )
}


reference_module_server <- function(id, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    selected_feature <- reactiveVal(NULL)

    reference_field_value <- function(feature_df, field, default = "Not available") {
      if (is.null(feature_df) || !field %in% names(feature_df)) {
        return(default)
      }

      value <- feature_df[[field]][1]
      if (is.null(value) || is.na(value) || identical(as.character(value), "")) {
        return(default)
      }

      as.character(value)
    }

    ref_features <- eventReactive(input$search_ref_btn, {
      selected_feature(NULL)

      tryCatch({
        features <- switch(
          input$ref_assay,
          transcriptomics = SigRepo::searchTranscriptomicsFeatureSet(
            conn_handler = user_conn_handler(),
            feature_name = input$feature_name,
            organism = input$ref_organism
          ),
          proteomics = SigRepo::searchProteomicsFeatureSet(
            conn_handler = user_conn_handler(),
            feature_name = input$feature_name,
            organism = input$ref_organism
          ),
          NULL
        )

        if (is.null(features)) {
          showNotification("Invalid assay type or no data returned.", type = "error")
          return(NULL)
        }

        if (nrow(features) == 0) {
          return(NULL)
        }

        features
      }, error = function(e) {
        showNotification(paste("Error:", e$message), type = "error")
        return(NULL)
      })
    })

    observeEvent(input$ref_feature_tbl_rows_selected, {
      features <- ref_features()
      row <- input$ref_feature_tbl_rows_selected

      if (is.null(features) || length(row) == 0) {
        selected_feature(NULL)
        return()
      }

      selected_feature(features[row, , drop = FALSE])
    })

    output$results_status <- renderUI({
      features <- ref_features()

      if (is.null(features) || nrow(features) == 0) {
        return(
          tags$span(
            class = "label label-default",
            "No results loaded"
          )
        )
      }

      tags$span(
        class = "label label-info",
        sprintf("%s results", nrow(features))
      )
    })

    output$results_summary <- renderUI({
      features <- ref_features()

      if (is.null(features) || nrow(features) == 0) {
        return(
          div(
            class = "sr-empty",
            "Run a search to browse reference features."
          )
        )
      }

      div(
        class = "sr-summary",
        div(
          class = "sr-summary-item",
          tags$strong("Assay"),
          tags$span(tools::toTitleCase(input$ref_assay))
        ),
        div(
          class = "sr-summary-item",
          tags$strong("Organism"),
          tags$span(input$ref_organism)
        ),
        div(
          class = "sr-summary-item",
          tags$strong("Results"),
          tags$span(nrow(features))
        ),
        div(
          class = "sr-summary-item",
          tags$strong("Feature Filter"),
          tags$span(if (nzchar(input$feature_name)) input$feature_name else "All features")
        )
      )
    })

    output$ref_feature_tbl <- renderDT({
      features <- ref_features()

      if (is.null(features) || nrow(features) == 0) {
        return(
          DatatableFX(
            data.frame(Message = "No reference features available for the selected search."),
            hidden_columns = integer(0),
            row_selection = "none"
          )
        )
      }

      DatatableFX(
        features,
        hidden_columns = integer(0),
        row_selection = "single"
      )
    })

    output$feature_detail_panel <- renderUI({
      feature <- selected_feature()

      if (is.null(feature)) {
        return(
          div(
            class = "sr-empty",
            "Select a feature from the results table to inspect its full record."
          )
        )
      }

      key_fields <- intersect(
        c("feature_name", "organism", "assay", "feature_type", "description"),
        names(feature)
      )

      summary_cards <- lapply(key_fields, function(field) {
        div(
          class = "sr-summary-item",
          tags$strong(gsub("_", " ", tools::toTitleCase(field))),
          tags$span(reference_field_value(feature, field))
        )
      })

      if (length(summary_cards) == 0) {
        summary_cards <- list(
          div(
            class = "sr-summary-item",
            tags$strong("Feature"),
            tags$span("Record loaded")
          )
        )
      }

      tagList(
        div(class = "sr-summary", summary_cards),
        DT::DTOutput(session$ns("feature_detail_tbl"))
      )
    })

    output$feature_detail_tbl <- DT::renderDataTable({
      feature <- selected_feature()
      req(feature)

      detail_df <- data.frame(
        Field = names(feature),
        Value = unlist(feature[1, ], use.names = FALSE),
        stringsAsFactors = FALSE
      )

      DatatableFX(
        detail_df,
        hidden_columns = integer(0),
        scrollY = "360px",
        row_selection = "none"
      )
    }, server = TRUE)
  })
}
