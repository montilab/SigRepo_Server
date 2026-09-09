reference_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("reference_page"))

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " {
        padding-top: 28px;
        padding-bottom: 32px;
      }

      ", page_selector, " .reference-hero {
        margin-bottom: 18px;
        padding: 22px 26px;
        border-radius: 14px;
        background: linear-gradient(135deg, #17485f 0%, #2d758d 100%);
        color: #ffffff;
        box-shadow: 0 10px 24px rgba(23, 72, 95, 0.16);
      }

      ", page_selector, " .reference-hero h2 {
        margin: 0 0 8px 0;
        font-weight: 700;
      }

      ", page_selector, " .reference-hero p {
        margin: 0;
        color: rgba(255, 255, 255, 0.88);
      }

      ", page_selector, " .reference-card {
        margin-bottom: 18px;
        padding: 20px 22px;
        border: 1px solid #d9e3ec;
        border-radius: 12px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }

      ", page_selector, " .reference-card h3,
      ", page_selector, " .reference-card h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .reference-filter-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 12px;
      }

      ", page_selector, " .reference-toolbar {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 16px;
        flex-wrap: wrap;
        margin-bottom: 14px;
      }

      ", page_selector, " .reference-summary-grid {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        margin-bottom: 18px;
      }

      ", page_selector, " .reference-summary-item {
        padding: 12px 14px;
        border-radius: 10px;
        background: #f6f9fc;
        border: 1px solid #e1ebf2;
      }

      ", page_selector, " .reference-summary-item strong {
        display: block;
        margin-bottom: 4px;
        color: #0f3b63;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .reference-summary-item span {
        color: #17324d;
        font-size: 15px;
        font-weight: 600;
      }

      ", page_selector, " .reference-empty {
        padding: 20px;
        border: 1px dashed #c5d5e3;
        border-radius: 10px;
        background: #f8fbfd;
        color: #4b647e;
      }

      ", page_selector, " .reference-helper {
        margin-bottom: 14px;
        color: #597189;
      }
    "))),

    div(
      id = ns("reference_page"),

      div(
        class = "reference-hero",
        tags$h2("Reference Browser"),
        tags$p(
          "Search transcriptomic or proteomic reference features, review the result set, and inspect individual feature records inline."
        )
      ),

      fluidRow(
        column(
          width = 4,
          div(
            class = "reference-card",
            tags$h3("Search Filters"),
            p(
              class = "reference-helper",
              "Choose an assay and organism, then optionally narrow the results to a specific feature name."
            ),
            div(
              class = "reference-filter-grid",
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
              label = "Feature Name",
              placeholder = "Optional: filter to a specific feature"
            ),
            actionButton(
              ns("search_ref_btn"),
              "Search Features",
              class = "btn-primary"
            )
          )
        ),
        column(
          width = 8,
          div(
            class = "reference-card",
            div(
              class = "reference-toolbar",
              tags$h3("Search Results"),
              uiOutput(ns("results_status"))
            ),
            uiOutput(ns("results_summary")),
            DT::DTOutput(ns("ref_feature_tbl"))
          ),
          div(
            class = "reference-card",
            tags$h3("Selected Feature"),
            p(
              class = "reference-helper",
              "Selecting a row loads the feature details below so you can inspect the full record without scanning every column in the table."
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
            class = "reference-empty",
            "Run a search to browse reference features."
          )
        )
      }

      div(
        class = "reference-summary-grid",
        div(
          class = "reference-summary-item",
          tags$strong("Assay"),
          tags$span(tools::toTitleCase(input$ref_assay))
        ),
        div(
          class = "reference-summary-item",
          tags$strong("Organism"),
          tags$span(input$ref_organism)
        ),
        div(
          class = "reference-summary-item",
          tags$strong("Results"),
          tags$span(nrow(features))
        ),
        div(
          class = "reference-summary-item",
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
            class = "reference-empty",
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
          class = "reference-summary-item",
          tags$strong(gsub("_", " ", tools::toTitleCase(field))),
          tags$span(reference_field_value(feature, field))
        )
      })

      if (length(summary_cards) == 0) {
        summary_cards <- list(
          div(
            class = "reference-summary-item",
            tags$strong("Feature"),
            tags$span("Record loaded")
          )
        )
      }

      tagList(
        div(class = "reference-summary-grid", summary_cards),
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
