annotate_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("annotate_page"))

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " {
        padding-top: 28px;
        padding-bottom: 32px;
      }

      ", page_selector, " .annotate-hero {
        margin-bottom: 18px;
        padding: 24px 28px;
        border-radius: 14px;
        background: linear-gradient(135deg, #0f3b63 0%, #1b5d8f 100%);
        color: #ffffff;
        box-shadow: 0 10px 24px rgba(15, 59, 99, 0.18);
      }

      ", page_selector, " .annotate-hero h2 {
        margin-top: 0;
        margin-bottom: 8px;
        font-weight: 700;
      }

      ", page_selector, " .annotate-hero p {
        margin-bottom: 0;
        color: rgba(255, 255, 255, 0.88);
      }

      ", page_selector, " .annotate-card {
        margin-bottom: 18px;
        padding: 20px 22px;
        border: 1px solid #d9e3ec;
        border-radius: 12px;
        background: #ffffff;
        box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }

      ", page_selector, " .annotate-card h3,
      ", page_selector, " .annotate-card h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .annotate-step-label {
        display: inline-block;
        margin-bottom: 10px;
        padding: 4px 10px;
        border-radius: 999px;
        background: #e9f2f9;
        color: #0f4d7c;
        font-size: 12px;
        font-weight: 700;
        letter-spacing: 0.04em;
        text-transform: uppercase;
      }

      ", page_selector, " .annotate-summary-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 12px;
      }

      ", page_selector, " .annotate-summary-item {
        padding: 12px 14px;
        border-radius: 10px;
        background: #f6f9fc;
        border: 1px solid #e1ebf2;
      }

      ", page_selector, " .annotate-summary-item strong {
        display: block;
        margin-bottom: 4px;
        color: #0f3b63;
        font-size: 12px;
        text-transform: uppercase;
        letter-spacing: 0.04em;
      }

      ", page_selector, " .annotate-summary-item span {
        color: #17324d;
        font-size: 15px;
        font-weight: 600;
      }

      ", page_selector, " .annotate-feedback {
        margin-top: 10px;
      }

      ", page_selector, " .annotate-actions {
        display: flex;
        gap: 10px;
        flex-wrap: wrap;
        margin-top: 14px;
      }

      ", page_selector, " .annotate-empty {
        padding: 18px;
        border: 1px dashed #c5d5e3;
        border-radius: 10px;
        background: #f8fbfd;
        color: #4b647e;
      }

      ", page_selector, " .annotate-results-header {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 16px;
      }

      ", page_selector, " .annotate-results-actions .btn {
        margin-left: 8px;
      }

      ", page_selector, " .geneset-filter-group {
        padding: 16px;
        border: 1px solid #d9e3ec;
        border-radius: 10px;
        background: #f8fbfd;
      }

      ", page_selector, " .geneset-filter-heading h4 {
        margin-top: 0;
        margin-bottom: 6px;
      }

      ", page_selector, " .geneset-filter-heading p {
        margin-bottom: 14px;
        color: #597189;
      }

      ", page_selector, " .geneset-filter-actions {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 12px;
      }

      ", page_selector, " .geneset-status {
        display: inline-flex;
        align-items: center;
        gap: 8px;
        padding: 8px 12px;
        border-radius: 999px;
        font-size: 13px;
        font-weight: 600;
      }

      ", page_selector, " .geneset-status-pending {
        background: #eef3f7;
        color: #4b647e;
      }

      ", page_selector, " .geneset-status-ready {
        background: #e7f5ec;
        color: #21663c;
      }

      ", page_selector, " .geneset-summary-text {
        margin-top: 10px;
        margin-bottom: 0;
        color: #3f5873;
        font-size: 13px;
      }
    "))),

    div(
      id = ns("annotate_page"),

      div(
        class = "annotate-hero",
        tags$h2("Annotate Signatures"),
        tags$p(
          "Configure an enrichment analysis, choose a geneset collection, review your selections, and run hypeR from a single workflow."
        )
      ),

      fluidRow(
        column(
          width = 4,

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 1"),
            tags$h3("Analysis Setup"),
            textInput(
              ns("experiment_label"),
              "Experiment Label",
              placeholder = "Example: Knockout Experiment"
            ),
            radioButtons(
              ns("enrichment_type"),
              "Enrichment Method",
              choices = c(
                "Hypergeometric" = "hypergeo",
                "KS Test" = "kstest",
                "GSEA" = "gsea"
              ),
              inline = FALSE
            ),
            helpText(
              "Hypergeometric uses feature lists. KS Test and GSEA expect ranked signatures."
            ),
            numericInput(
              ns("enrichment_thresh"),
              "FDR Threshold",
              value = 0.05,
              min = 0,
              max = 1,
              step = 0.01
            ),
            numericInput(
              ns("enrichment_bg"),
              "Background Gene Count",
              value = 36000,
              min = 1,
              step = 100
            )
          ),

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 2"),
            tags$h3("Geneset Selection"),
            tags$p(
              "Use the filter set below to define the genesets included in this enrichment run."
            ),
            selectInput(
              ns("species"),
              "Species",
              choices = msigdbr::msigdbr_species()$species_name,
              selected = "Homo sapiens"
            ),
            genesets_hypeR_UI("genesets")
          )
        ),

        column(
          width = 8,

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 3"),
            tags$h3("Select Signatures"),
            tags$p(
              "Choose one or more signatures from the repository, then add them to the analysis."
            ),
            DT::DTOutput(ns("signature_hypeR")),
            div(
              class = "annotate-actions",
              actionButton(
                ns("signature_add"),
                "Add Selected Signatures",
                class = "btn-primary"
              )
            ),
            div(
              class = "annotate-feedback",
              uiOutput(ns("signature_feedback"))
            )
          ),

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 4"),
            tags$h3("Review and Run"),
            uiOutput(ns("analysis_summary")),
            div(
              class = "annotate-actions",
              actionButton(
                ns("enrichment_do"),
                "Run Enrichment",
                class = "btn-primary"
              )
            )
          ),

          div(
            class = "annotate-card",
            tags$h3("Current Selection"),
            uiOutput(ns("signature_preview"))
          )
        )
      ),

      div(
        class = "annotate-card",
        div(
          class = "annotate-results-header",
          div(
            tagList(
              span(class = "annotate-step-label", "Results"),
              tags$h3("Experiment Results")
            )
          ),
          div(
            class = "annotate-results-actions",
            actionButton(ns("generate_report"), "HTML Report"),
            actionButton(ns("export_hyp"), "Export")
          )
        ),
        uiOutput(ns("enrichment")),
        plotOutput(ns("dotplot"), height = "400px", width = "100%")
      )
    )
  )
}


annotate_module_server <- function(id, signature_db, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    active_signatures <- reactiveVal(list())
    run_feedback <- reactiveVal(NULL)
    hyp_result <- reactiveVal(NULL)

    output$signature_hypeR <- renderDT({
      df <- signature_db()

      DatatableFX(
        df = df,
        hidden_columns = c(0, 6, 7, 8, 9, 11, 12, 14, 15, 16, 18, 19, 21, 22, 24, 25, 26),
        scrollY = "300px",
        row_selection = "multiple"
      )
    }, server = TRUE)

    genesets <- genesets_hypeR_Server(
      id = "genesets",
      species = reactive(input$species),
      clean = TRUE
    )

    observeEvent(input$signature_add, {
      selected_rows <- input$signature_hypeR_rows_selected

      if (length(selected_rows) == 0) {
        run_feedback(list(
          type = "warning",
          text = "Select at least one signature before adding it to the analysis."
        ))
        return()
      }

      df <- signature_db()
      req(!is.null(df))

      sig_rows <- df[selected_rows, , drop = FALSE]
      current <- active_signatures()
      added_count <- 0

      for (i in seq_len(nrow(sig_rows))) {
        sig_row <- sig_rows[i, ]
        key <- sig_row$signature_name
        if (!key %in% names(current)) {
          current[[key]] <- list(
            experiment = input$experiment_label,
            signature_name = sig_row$signature_name,
            signature_id = sig_row$signature_id
          )
          added_count <- added_count + 1
        }
      }

      active_signatures(current)

      run_feedback(list(
        type = if (added_count > 0) "success" else "info",
        text = if (added_count > 0) {
          sprintf("%s signature(s) added to the analysis.", added_count)
        } else {
          "All selected signatures are already in the current analysis."
        }
      ))
    })

    output$signature_feedback <- renderUI({
      feedback <- run_feedback()
      if (is.null(feedback)) {
        return(NULL)
      }

      class_name <- switch(
        feedback$type,
        success = "alert alert-success",
        warning = "alert alert-warning",
        info = "alert alert-info",
        "alert alert-info"
      )

      tags$div(class = class_name, feedback$text)
    })

    output$analysis_summary <- renderUI({
      sig_list <- active_signatures()
      gsets <- genesets()

      summary_value <- function(value, empty = "Not set") {
        if (is.null(value) || identical(value, "") || (length(value) == 0)) empty else as.character(value)
      }

      div(
        class = "annotate-summary-grid",
        div(
          class = "annotate-summary-item",
          tags$strong("Experiment"),
          tags$span(summary_value(input$experiment_label, "Untitled analysis"))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Method"),
          tags$span(
            dplyr::recode(
              input$enrichment_type,
              hypergeo = "Hypergeometric",
              kstest = "KS Test",
              gsea = "GSEA"
            )
          )
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Selected Signatures"),
          tags$span(length(sig_list))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Selected Genesets"),
          tags$span(length(gsets))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("FDR Threshold"),
          tags$span(summary_value(input$enrichment_thresh))
        ),
        div(
          class = "annotate-summary-item",
          tags$strong("Background"),
          tags$span(summary_value(input$enrichment_bg))
        )
      )
    })

    output$signature_preview <- renderUI({
      sig_list <- active_signatures()
      gsets <- genesets()

      if (length(sig_list) == 0) {
        return(
          div(
            class = "annotate-empty",
            "No signatures have been added yet. Select signatures from the table above to start the analysis."
          )
        )
      }

      sig_names <- vapply(sig_list, `[[`, character(1), "signature_name")
      df <- signature_db()
      req(df)

      selected_df <- df[df$signature_name %in% sig_names, , drop = FALSE]
      preview_df <- selected_df[, intersect(c("signature_name", "signature_id", "perturbation"), names(selected_df)), drop = FALSE]
      geneset_names <- names(gsets)

      tagList(
        tags$p(
          "Review the selected signatures and genesets before running enrichment."
        ),
        DT::datatable(
          preview_df,
          rownames = FALSE,
          options = list(
            scrollX = TRUE,
            pageLength = 5,
            dom = "tip"
          ),
          class = "compact stripe hover"
        ),
        tags$hr(),
        tags$h4("Geneset Collections"),
        if (length(geneset_names) > 0) {
          tags$ul(lapply(head(geneset_names, 10), tags$li))
        } else {
          div(
            class = "annotate-empty",
            "No genesets have been fetched yet. Choose a collection and subcategory, then fetch genesets."
          )
        },
        if (length(geneset_names) > 10) {
          tags$p(sprintf("Showing 10 of %s genesets selected.", length(geneset_names)))
        }
      )
    })

    observeEvent(input$enrichment_do, {
      sig_list <- active_signatures()
      gsets <- genesets()

      if (length(sig_list) == 0) {
        showNotification("Add at least one signature before running enrichment.", type = "error")
        return()
      }

      if (length(gsets) == 0) {
        showNotification("Fetch at least one geneset collection before running enrichment.", type = "error")
        return()
      }

      sig_ids <- vapply(sig_list, function(sig) sig$signature_id, character(1))

      sig_objs <- SigRepo::getSignature(
        conn_handler = user_conn_handler(),
        signature_id = sig_ids
      )

      signature_vectors <- lapply(sig_objs, function(x) {
        if (is.null(x$signature)) {
          return(NULL)
        }

        symbols <- as.character(x$difexp$symbol)
        symbols <- symbols[!is.na(symbols)]

        if (length(symbols) == 0) {
          return(NULL)
        }

        symbols
      })

      signature_vectors <- signature_vectors[!vapply(signature_vectors, is.null, logical(1))]

      if (length(signature_vectors) == 0) {
        showNotification("No valid signatures were available to run enrichment.", type = "error")
        return()
      }

      hyp <- hypeR::hypeR(
        signature = signature_vectors,
        genesets = gsets,
        test = input$enrichment_type,
        background = input$enrichment_bg,
        fdr = input$enrichment_thresh,
        plotting = FALSE,
        quiet = TRUE
      )

      hyp_result(hyp)
      showNotification("Enrichment analysis completed.", type = "message")
    })

    output$dotplot <- renderPlot({
      hyp <- hyp_result()
      req(hyp)

      hypeR::hyp_dots(
        hyp,
        merge = TRUE,
        fdr = input$enrichment_thresh,
        title = input$experiment_label
      )
    })

    output$enrichment <- renderUI({
      hyp <- hyp_result()

      if (is.null(hyp)) {
        return(
          div(
            class = "annotate-empty",
            "Results will appear here after you run an enrichment analysis."
          )
        )
      }

      tagList(
        tags$h4("Enrichment Results"),
        hypeR::rctbl_build(hyp)
      )
    })

    observeEvent(input$generate_report, {
      hyp <- hyp_result()
      if (is.null(hyp)) {
        showNotification("Run an enrichment analysis before generating a report.", type = "warning")
      }
    })

    observeEvent(input$export_hyp, {
      hyp <- hyp_result()
      if (is.null(hyp)) {
        showNotification("Run an enrichment analysis before exporting results.", type = "warning")
      }
    })
  })
}
