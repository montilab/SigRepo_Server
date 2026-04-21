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

      ", page_selector, " .annotate-results-body {
        display: grid;
        grid-template-columns: minmax(0, 1fr);
        gap: 22px;
      }

      ", page_selector, " .annotate-results-section h4 {
        margin-top: 0;
        margin-bottom: 12px;
        color: #17324d;
        font-weight: 600;
      }

      ", page_selector, " .annotate-signature-key {
        margin-top: 14px;
      }

      ", page_selector, " .annotate-signature-key .dataTables_wrapper {
        font-size: 12px;
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
            genesets_hypeR_UI(ns("genesets"))
          )
        ),

        column(
          width = 8,

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 3"),
            tags$h3("Select Signatures"),
            tags$p(
              "Choose up to 10 signatures from the repository, then add them to the analysis."
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
              ),
              actionButton(
                ns("experiment_reset"),
                "New Experiment",
                class = "btn-default"
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
            downloadButton(ns("export_hyp"), "Export Hype Object")
          )
        ),
        uiOutput(ns("enrichment"))
      )
    )
  )
}


hype_dotplot_data <- function(hyp, fdr_threshold, top = 30, abrv = 50) {
  if (is.null(fdr_threshold) || length(fdr_threshold) == 0 || is.na(fdr_threshold)) {
    fdr_threshold <- 1
  }

  empty_df <- data.frame(
    signature = character(),
    label = character(),
    fdr = numeric(),
    geneset_size = numeric(),
    stringsAsFactors = FALSE
  )

  hyp_entries <- if (methods::is(hyp, "multihyp")) {
    hyp$data
  } else {
    list(Enrichment = hyp)
  }

  if (is.null(names(hyp_entries)) || any(!nzchar(names(hyp_entries)))) {
    names(hyp_entries) <- paste("Signature", seq_along(hyp_entries))
  }

  plot_dfs <- lapply(seq_along(hyp_entries), function(i) {
    hyp_entry <- hyp_entries[[i]]
    hyp_df <- if (is.data.frame(hyp_entry)) hyp_entry else hyp_entry$data

    if (is.null(hyp_df) || !is.data.frame(hyp_df) || nrow(hyp_df) == 0) {
      return(NULL)
    }

    if (!all(c("label", "fdr") %in% names(hyp_df))) {
      return(NULL)
    }

    hyp_df$fdr <- suppressWarnings(as.numeric(hyp_df$fdr))
    hyp_df <- hyp_df[!is.na(hyp_df$fdr) & hyp_df$fdr <= fdr_threshold, , drop = FALSE]

    if (nrow(hyp_df) == 0) {
      return(NULL)
    }

    geneset_size <- rep(1, nrow(hyp_df))
    if ("geneset" %in% names(hyp_df)) {
      geneset_size <- suppressWarnings(as.numeric(hyp_df$geneset))
      geneset_size[is.na(geneset_size) | geneset_size <= 0] <- 1
    }

    data.frame(
      signature = names(hyp_entries)[[i]],
      label = substr(as.character(hyp_df$label), 1, abrv),
      fdr = hyp_df$fdr,
      geneset_size = geneset_size,
      stringsAsFactors = FALSE
    )
  })

  plot_dfs <- plot_dfs[!vapply(plot_dfs, is.null, logical(1))]

  if (length(plot_dfs) == 0) {
    return(empty_df)
  }

  plot_df <- do.call(rbind, plot_dfs)
  label_rank <- stats::aggregate(fdr ~ label, data = plot_df, FUN = min)
  label_rank <- label_rank[order(label_rank$fdr), , drop = FALSE]
  top_labels <- head(label_rank$label, top)
  plot_df <- plot_df[plot_df$label %in% top_labels, , drop = FALSE]
  plot_df[order(plot_df$fdr), , drop = FALSE]
}


annotate_module_server <- function(id, signature_db, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    max_signature_count <- 10
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
      skipped_limit_count <- 0

      for (i in seq_len(nrow(sig_rows))) {
        sig_row <- sig_rows[i, ]
        key <- sig_row$signature_name

        if (!key %in% names(current)) {
          if (length(current) >= max_signature_count) {
            skipped_limit_count <- skipped_limit_count + 1
            next
          }

          current[[key]] <- list(
            experiment = input$experiment_label,
            signature_name = sig_row$signature_name,
            signature_id = sig_row$signature_id
          )
          added_count <- added_count + 1
        }
      }

      active_signatures(current)
      hyp_result(NULL)

      run_feedback(list(
        type = if (skipped_limit_count > 0) "warning" else if (added_count > 0) "success" else "info",
        text = if (skipped_limit_count > 0) {
          sprintf(
            "%s signature(s) added. The analysis is limited to %s signatures, so %s selection(s) were skipped.",
            added_count,
            max_signature_count,
            skipped_limit_count
          )
        } else if (added_count > 0) {
          sprintf("%s signature(s) added to the analysis.", added_count)
        } else {
          "All selected signatures are already in the current analysis."
        }
      ))
    })

    observeEvent(input$experiment_reset, {
      active_signatures(list())
      hyp_result(NULL)
      run_feedback(NULL)

      updateTextInput(session, "experiment_label", value = "")
      updateRadioButtons(session, "enrichment_type", selected = "hypergeo")
      updateNumericInput(session, "enrichment_thresh", value = 0.05)
      updateNumericInput(session, "enrichment_bg", value = 36000)

      DT::selectRows(DT::dataTableProxy("signature_hypeR", session = session), NULL)
      showNotification("Experiment selections and results were reset.", type = "message")
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
          tags$span(sprintf("%s / %s", length(sig_list), max_signature_count))
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

      sig_ids <- vapply(sig_list, function(sig) as.numeric(sig$signature_id), numeric(1))

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

    dotplot_data <- reactive({
      hyp <- hyp_result()
      req(hyp)

      plot_df <- hype_dotplot_data(hyp, fdr_threshold = input$enrichment_thresh)
      validate(need(nrow(plot_df) > 0, "No enriched genesets passed the selected FDR threshold."))

      signature_lookup <- unique(plot_df["signature"])
      signature_lookup$signature_label <- paste0("S", seq_len(nrow(signature_lookup)))

      plot_df <- merge(plot_df, signature_lookup, by = "signature", all.x = TRUE, sort = FALSE)
      plot_df$signature_label <- factor(plot_df$signature_label, levels = signature_lookup$signature_label)
      plot_df$label <- factor(plot_df$label, levels = rev(unique(plot_df$label)))

      positive_fdr <- plot_df$fdr[plot_df$fdr > 0]
      min_positive_fdr <- if (length(positive_fdr) > 0) min(positive_fdr, na.rm = TRUE) else .Machine$double.xmin
      plot_df$fdr_plot <- pmax(plot_df$fdr, min_positive_fdr / 10)

      list(
        plot_df = plot_df,
        signature_lookup = signature_lookup[, c("signature_label", "signature"), drop = FALSE]
      )
    })

    output$dotplot <- renderPlot({
      plot_df <- dotplot_data()$plot_df

      ggplot2::ggplot(
        plot_df,
        ggplot2::aes(
          x = signature_label,
          y = label,
          color = fdr_plot,
          size = geneset_size
        )
      ) +
        ggplot2::geom_point(alpha = 0.86) +
        ggplot2::scale_color_continuous(
          low = "#E53935",
          high = "#114357",
          trans = "log10",
          guide = ggplot2::guide_colorbar(reverse = TRUE)
        ) +
        ggplot2::scale_size_continuous(trans = "log10") +
        ggplot2::labs(
          title = input$experiment_label,
          x = NULL,
          y = NULL,
          color = "FDR",
          size = "Geneset Size"
        ) +
        ggplot2::theme_minimal(base_size = 12) +
        ggplot2::theme(
          plot.title = ggplot2::element_text(hjust = 0.5, face = "bold"),
          axis.text.x = ggplot2::element_text(face = "bold"),
          panel.grid.major.y = ggplot2::element_line(color = "#e6edf3"),
          panel.grid.minor = ggplot2::element_blank()
        )
    })

    output$dotplot_signature_key <- DT::renderDT({
      key_df <- dotplot_data()$signature_lookup
      names(key_df) <- c("Plot ID", "Signature")

      DT::datatable(
        key_df,
        rownames = FALSE,
        options = list(
          pageLength = 10,
          dom = "tip",
          scrollX = TRUE
        ),
        class = "compact stripe hover"
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

      div(
        class = "annotate-results-body",
        div(
          class = "annotate-results-section",
          tags$h4("Results Table"),
          hypeR::rctbl_build(hyp)
        ),
        div(
          class = "annotate-results-section",
          tags$h4("Dotplot"),
          plotOutput(session$ns("dotplot"), height = "400px", width = "100%"),
          div(
            class = "annotate-signature-key",
            tags$h4("Signature Key"),
            DT::DTOutput(session$ns("dotplot_signature_key"))
          )
        )
      )
    })

    observeEvent(input$generate_report, {
      hyp <- hyp_result()
      if (is.null(hyp)) {
        showNotification("Run an enrichment analysis before generating a report.", type = "warning")
      }
    })

    output$export_hyp <- downloadHandler(
      filename = function() {
        label <- input$experiment_label
        if (is.null(label) || !nzchar(label)) {
          label <- "sigrepo_enrichment"
        }

        safe_label <- gsub("[^A-Za-z0-9_-]+", "_", label)
        sprintf("%s_hype_%s.rds", safe_label, format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        hyp <- hyp_result()
        req(hyp)
        saveRDS(hyp, file)
      }
    )
  })
}
