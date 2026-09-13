# compare page modules
#
# A front end to SigRepo::compareSignatures(). Each of its two signature lists
# is built from the same three sources the function takes -- database ids
# (rows picked from the signature table), database names, and uploaded
# OmicSignature objects -- and every comparison argument is exposed with the
# function's own default. The result is read back and plotted with
# OmicSignature::signature_similarity_heatmap(). The helpers these call live in
# utils/compare_utils.R.

COMPARE_METHOD_CHOICES <- c(
  "Overlap (Jaccard / Fisher)" = "overlap",
  "KS: rank position" = "ks_rank",
  "KS: score distribution" = "ks_score",
  "GSEA (fgsea)" = "gsea"
)

COMPARE_TABLE_COLUMNS <- c(
  "signature_id", "signature_name", "organism", "direction_type", "assay_type",
  "phenotype", "has_difexp", "user_name"
)

compare_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("compare_page"))

  list_panel <- function(k, title, description) {
    div(
      class = "compare-list",
      tags$h4(title),
      tags$p(class = "compare-muted", description),
      tags$label("Pick from the repository"),
      DT::DTOutput(ns(paste0("list", k, "_table"))),
      fluidRow(
        column(
          width = 6,
          textAreaInput(
            ns(paste0("list", k, "_names")),
            "Add by signature name",
            placeholder = "One per line, or comma separated",
            rows = 3,
            width = "100%"
          )
        ),
        column(
          width = 6,
          fileInput(
            ns(paste0("list", k, "_upload")),
            "Upload OmicSignature .rds",
            multiple = TRUE,
            accept = ".rds",
            width = "100%"
          ),
          helpText("An OmicSignature, a list of them, or an OmicSignatureCollection.")
        )
      ),
      uiOutput(ns(paste0("list", k, "_summary")))
    )
  }

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " { padding-top: 28px; padding-bottom: 32px; }
      ", page_selector, " .compare-hero {
        margin-bottom: 18px; padding: 24px 28px; border-radius: 14px;
        background: linear-gradient(135deg, #0f3b63 0%, #1b5d8f 100%);
        color: #ffffff; box-shadow: 0 10px 24px rgba(15, 59, 99, 0.18);
      }
      ", page_selector, " .compare-hero h2 { margin-top: 0; margin-bottom: 8px; font-weight: 700; }
      ", page_selector, " .compare-hero p { margin-bottom: 0; color: rgba(255, 255, 255, 0.88); }
      ", page_selector, " .compare-card {
        margin-bottom: 18px; padding: 20px 22px; border: 1px solid #d9e3ec; border-radius: 12px;
        background: #ffffff; box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }
      ", page_selector, " .compare-card h3, ", page_selector, " .compare-card h4 {
        margin-top: 0; margin-bottom: 12px; color: #17324d; font-weight: 600;
      }
      ", page_selector, " .compare-step-label {
        display: inline-block; margin-bottom: 10px; padding: 4px 10px; border-radius: 999px;
        background: #e9f2f9; color: #0f4d7c; font-size: 12px; font-weight: 700;
        letter-spacing: 0.04em; text-transform: uppercase;
      }
      ", page_selector, " .compare-list + .compare-list { margin-top: 22px; padding-top: 18px; border-top: 1px solid #e1ebf2; }
      ", page_selector, " .compare-muted { color: #597189; }
      ", page_selector, " .compare-list-summary {
        padding: 10px 14px; border-radius: 10px; background: #f6f9fc; border: 1px solid #e1ebf2;
      }
      ", page_selector, " .compare-list-summary ul { margin: 6px 0 0 0; padding-left: 18px; }
      ", page_selector, " .compare-source { color: #597189; font-size: 12px; }
      ", page_selector, " .compare-pairing-table { width: 100%; }
      ", page_selector, " .compare-pairing-table td { padding: 2px 8px 2px 0; vertical-align: middle; }
      ", page_selector, " .compare-pairing-table .form-group { margin-bottom: 4px; }
      ", page_selector, " .compare-actions { display: flex; gap: 10px; flex-wrap: wrap; align-items: center; margin-top: 14px; }
      ", page_selector, " .compare-summary-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); gap: 12px; margin-bottom: 14px; }
      ", page_selector, " .compare-summary-item { padding: 12px 14px; border-radius: 10px; background: #f6f9fc; border: 1px solid #e1ebf2; }
      ", page_selector, " .compare-summary-item strong {
        display: block; margin-bottom: 4px; color: #0f3b63; font-size: 12px;
        text-transform: uppercase; letter-spacing: 0.04em;
      }
      ", page_selector, " .compare-summary-item span { color: #17324d; font-size: 15px; font-weight: 600; }
      ", page_selector, " .compare-empty {
        padding: 18px; border: 1px dashed #c5d5e3; border-radius: 10px; background: #f8fbfd; color: #4b647e;
      }
      ", page_selector, " .compare-message { white-space: pre-wrap; margin-bottom: 8px; }
      ", page_selector, " .compare-heatmap-controls { display: flex; gap: 14px; flex-wrap: wrap; align-items: flex-end; }
      ", page_selector, " .compare-heatmap-controls .form-group { min-width: 150px; }
      ", page_selector, " .tab-content { padding-top: 16px; }
    "))),

    div(
      id = ns("compare_page"),

      div(
        class = "compare-hero",
        tags$h2("Compare Signatures"),
        tags$p(
          "Compare signatures by feature overlap or rank-based enrichment (KS and GSEA) with ",
          tags$code(style = "color: #ffffff; background: rgba(255,255,255,0.15);", "SigRepo::compareSignatures()"),
          ", either all against each other or a first list against a second."
        )
      ),

      fluidRow(
        column(
          width = 8,
          div(
            class = "compare-card",
            span(class = "compare-step-label", "Step 1"),
            tags$h3("Signatures"),
            list_panel(
              1, "List 1",
              "Each signature here is compared against every other one, or against List 2 when it is switched on. List 1 is the rows of every result."
            ),
            div(
              class = "compare-list",
              checkboxInput(ns("two_lists"), strong("Compare against a second list (List 2)"), value = FALSE),
              conditionalPanel(
                condition = "input.two_lists",
                ns = ns,
                list_panel(
                  2, "List 2",
                  "The columns of every result. For KS and GSEA this is the ranking side: its signatures must be bi-directional with a difexp table."
                )
              )
            )
          )
        ),

        column(
          width = 4,
          div(
            class = "compare-card",
            span(class = "compare-step-label", "Step 2"),
            tags$h3("Comparison"),
            selectInput(ns("method"), "Method", choices = COMPARE_METHOD_CHOICES, selected = "overlap"),
            uiOutput(ns("method_help")),
            fluidRow(
              column(6, numericInput(ns("score_cutoff"), "Score cutoff (|score| ≥)", value = 0, min = 0, step = 0.1)),
              column(6, numericInput(ns("adj_p_cutoff"), "Adj. p cutoff (≤)", value = 0.05, min = 0, max = 1, step = 0.01))
            ),
            fluidRow(
              column(6, numericInput(ns("min_features"), "Min features", value = 5, min = 3, step = 1)),
              column(6, numericInput(ns("max_feature"), "Max features", value = 500, min = 3, step = 10))
            ),
            fluidRow(
              column(6, selectInput(ns("alternative"), "Alternative", choices = c("greater", "less", "two.sided"), selected = "greater")),
              column(6, selectInput(ns("p_adjust_method"), "P-value adjustment", choices = stats::p.adjust.methods, selected = "BH"))
            ),
            checkboxInput(ns("adjust"), "Adjust p-values within each comparison", value = FALSE),
            conditionalPanel(
              condition = "input.method == 'gsea'",
              ns = ns,
              tags$h4("GSEA"),
              selectInput(ns("gsea_score"), "Score to report", choices = c("NES", "ES", "log2err", "size"), selected = "NES"),
              fluidRow(
                column(6, numericInput(ns("min_size"), "minSize", value = 1, min = 1, step = 1)),
                column(6, numericInput(ns("max_size"), "maxSize (blank = Inf)", value = NA, min = 1, step = 1))
              )
            ),
            tags$details(
              tags$summary(strong("Advanced: background, columns, label pairing")),
              br(),
              textAreaInput(
                ns("background"),
                "Background features (overlap tests)",
                placeholder = "Leave blank to use every feature in the compared signatures",
                rows = 3,
                width = "100%"
              ),
              fluidRow(
                column(6, textInput(ns("feature_col"), "Feature column", value = "feature_name")),
                column(6, textInput(ns("score_col"), "Score column", value = "score"))
              ),
              fluidRow(
                column(6, textInput(ns("adj_p_col"), "Adj. p column", value = "adj_p")),
                column(6, textInput(ns("p_value_col"), "P-value column", value = "p_value"))
              ),
              textInput(ns("group_col"), "Group label column", value = "group_label"),
              tags$h4("Label pairing"),
              helpText(
                "Levels are paired by position (level 1 with level 1, level 2 with level 2), using each signature's own",
                "group_label order unless you set both levels here. Uni-directional signatures have no levels to pair."
              ),
              uiOutput(ns("pairing1_ui")),
              conditionalPanel(condition = "input.two_lists", ns = ns, uiOutput(ns("pairing2_ui")))
            )
          ),

          div(
            class = "compare-card",
            span(class = "compare-step-label", "Step 3"),
            tags$h3("Run"),
            uiOutput(ns("run_readiness")),
            div(
              class = "compare-actions",
              actionButton(ns("run"), "Compare Signatures", class = "btn-primary", icon = icon("code-compare"))
            )
          )
        )
      ),

      div(
        class = "compare-card",
        span(class = "compare-step-label", "Results"),
        uiOutput(ns("run_messages")),
        conditionalPanel(
          condition = "!output.has_result",
          ns = ns,
          div(class = "compare-empty", "Run a comparison to see the heatmap, tables, and the equivalent R call.")
        ),
        conditionalPanel(
          condition = "output.has_result",
          ns = ns,
          uiOutput(ns("result_summary")),
          tabsetPanel(
            id = ns("result_tabs"),
            tabPanel(
              "Heatmap",
              uiOutput(ns("heatmap_controls")),
              plotOutput(ns("heatmap"), height = "auto"),
              div(
                class = "compare-actions",
                downloadButton(ns("download_heatmap_png"), "PNG"),
                downloadButton(ns("download_heatmap_pdf"), "PDF")
              )
            ),
            tabPanel(
              "Pairs",
              helpText("Every signature pair with every measure. Self-comparisons list each overlap pair once."),
              DT::DTOutput(ns("pairs_table"))
            ),
            tabPanel(
              "Matrices",
              uiOutput(ns("matrix_controls")),
              DT::DTOutput(ns("matrix_table"))
            ),
            tabPanel(
              "Label order",
              helpText("The group label each signature contributes at each level."),
              DT::DTOutput(ns("label_order_table"))
            ),
            tabPanel(
              "R code",
              helpText("The same comparison from R. The result download can be passed straight to OmicSignature::signature_similarity_heatmap()."),
              verbatimTextOutput(ns("r_call")),
              div(class = "compare-actions", downloadButton(ns("download_result"), "Download result (.rds)"))
            )
          )
        )
      )
    )
  )
}


compare_module_server <- function(id, signature_db, user_conn_handler) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # args, result, warnings, error of the last run; NULL before the first.
    run_state <- reactiveVal(NULL)

    # ---- signature pickers --------------------------------------------------

    db_table <- reactive({
      df <- signature_db()
      if (!is.data.frame(df) || !"signature_id" %in% names(df)) {
        return(data.frame(
          signature_id = numeric(), signature_name = character(), direction_type = character(),
          stringsAsFactors = FALSE
        ))
      }
      df
    })

    picker_table <- reactive({
      df <- db_table()
      df[, intersect(COMPARE_TABLE_COLUMNS, names(df)), drop = FALSE]
    })

    render_picker <- function() {
      DT::renderDT({
        DatatableFX(picker_table(), hidden_columns = integer(), scrollY = "260px", row_selection = "multiple")
      }, server = TRUE)
    }
    output$list1_table <- render_picker()
    output$list2_table <- render_picker()

    # One list's request from its three sources. Upload problems are kept so
    # they can be shown next to the list and stop a run.
    list_request <- function(k) {
      reactive({
        df <- db_table()
        rows <- input[[paste0("list", k, "_table_rows_selected")]]
        rows <- rows[rows >= 1 & rows <= nrow(df)]
        uploads <- tryCatch(
          list(signatures = compare_read_signature_uploads(input[[paste0("list", k, "_upload")]]), error = NULL),
          error = function(e) list(signatures = list(), error = conditionMessage(e))
        )
        list(
          signature_ids = df$signature_id[rows],
          signature_names = compare_parse_names(input[[paste0("list", k, "_names")]]),
          omic_signatures = uploads$signatures,
          upload_error = uploads$error
        )
      })
    }
    requests <- list(list_request(1), list_request(2))

    previews <- lapply(1:2, function(k) {
      reactive({
        r <- requests[[k]]()
        compare_preview_list(db_table(), r$signature_ids, r$signature_names, r$omic_signatures)
      })
    })

    lapply(1:2, function(k) {
      output[[paste0("list", k, "_summary")]] <- renderUI({
        r <- requests[[k]]()
        p <- previews[[k]]()
        missing <- attr(p, "missing")
        tagList(
          if (!is.null(r$upload_error)) div(class = "alert alert-danger compare-message", r$upload_error),
          if (length(missing) > 0) {
            div(
              class = "alert alert-warning compare-message",
              sprintf("Not among the signatures you can see, so they will be left out: %s", paste(missing, collapse = ", "))
            )
          },
          div(
            class = "compare-list-summary",
            strong(sprintf("%d signature%s in List %d", nrow(p), if (nrow(p) == 1) "" else "s", k)),
            if (nrow(p) > 0) {
              tags$ul(lapply(seq_len(nrow(p)), function(i) {
                tags$li(
                  p$name[i], " ",
                  span(class = "compare-source", sprintf("(%s%s)", p$source[i],
                                                         if (is.na(p$direction_type[i])) "" else paste(",", p$direction_type[i])))
                )
              }))
            }
          )
        )
      })
    })

    # ---- label pairing ------------------------------------------------------

    # Input ids for each pairable signature in list k, keyed by the name
    # compareSignatures() will give it. Ids hash the name, so a typed pairing
    # stays with its signature when others are added or removed.
    pairing_input_ids <- function(k) {
      p <- previews[[k]]()
      pairable <- p$name[is.na(p$direction_type) | p$direction_type != "uni-directional"]
      stats::setNames(lapply(pairable, function(nm) {
        key <- digest::digest(nm, algo = "crc32", serialize = FALSE)
        c(level1 = sprintf("pair%d_%s_1", k, key), level2 = sprintf("pair%d_%s_2", k, key))
      }), pairable)
    }

    lapply(1:2, function(k) {
      output[[paste0("pairing", k, "_ui")]] <- renderUI({
        ids <- pairing_input_ids(k)
        p <- previews[[k]]()
        if (length(ids) == 0) {
          return(helpText(sprintf("List %d has no bi-directional signatures to pair.", k)))
        }
        tagList(
          strong(sprintf("List %d", k)),
          tags$table(
            class = "compare-pairing-table",
            lapply(names(ids), function(nm) {
              hint <- p$labels_hint[match(nm, p$name)]
              tags$tr(
                tags$td(
                  div(nm),
                  if (!is.na(hint) && nzchar(hint)) span(class = "compare-source", hint)
                ),
                tags$td(textInput(ns(ids[[nm]][["level1"]]), NULL, value = isolate(input[[ids[[nm]][["level1"]]]]) %||% "",
                                  placeholder = "level 1", width = "110px")),
                tags$td(textInput(ns(ids[[nm]][["level2"]]), NULL, value = isolate(input[[ids[[nm]][["level2"]]]]) %||% "",
                                  placeholder = "level 2", width = "110px"))
              )
            })
          )
        )
      })
    })

    read_pairing <- function(k) {
      ids <- pairing_input_ids(k)
      compare_label_pairing(
        names(ids),
        vapply(ids, function(x) input[[x[["level1"]]]] %||% "", character(1)),
        vapply(ids, function(x) input[[x[["level2"]]]] %||% "", character(1))
      )
    }

    # ---- settings and readiness ---------------------------------------------

    output$method_help <- renderUI({
      text <- switch(
        input$method %||% "overlap",
        overlap = "Jaccard similarity and a Fisher test on the retained feature sets. No difexp needed.",
        ks_rank = "KS test on where each List 1 feature set falls in the ranking side's difexp ranking.",
        ks_score = "Two-sample KS test on the ranking scores of each feature set against the rest.",
        gsea = "fgsea of each List 1 feature set against the ranking side's difexp ranking."
      )
      ranking <- if (!identical(input$method %||% "overlap", "overlap")) {
        " The ranking side (List 2, or List 1 itself for a self-comparison) needs bi-directional signatures with a difexp table."
      }
      helpText(text, ranking)
    })

    # Input values, falling back to compareSignatures()'s defaults only where
    # an input has never been set. A cleared numeric box stays NA so the
    # function's own cutoff validation reports it.
    settings <- reactive({
      value <- function(x, default) if (is.null(x)) default else x
      max_size <- input$max_size
      list(
        method = value(input$method, "overlap"),
        background = compare_parse_background(input$background),
        score_cutoff = value(input$score_cutoff, 0),
        adj_p_cutoff = value(input$adj_p_cutoff, 0.05),
        min_features = value(input$min_features, 5),
        max_feature = value(input$max_feature, 500),
        feature_col = value(input$feature_col, "feature_name"),
        score_col = value(input$score_col, "score"),
        adj_p_col = value(input$adj_p_col, "adj_p"),
        p_value_col = value(input$p_value_col, "p_value"),
        group_col = value(input$group_col, "group_label"),
        adjust = value(input$adjust, FALSE),
        p_adjust_method = value(input$p_adjust_method, "BH"),
        alternative = value(input$alternative, "greater"),
        gsea_score = value(input$gsea_score, "NES"),
        minSize = value(input$min_size, 1),
        maxSize = if (is.null(max_size) || is.na(max_size)) Inf else max_size
      )
    })

    readiness <- reactive({
      n1 <- nrow(previews[[1]]())
      two <- isTRUE(input$two_lists)
      n2 <- if (two) nrow(previews[[2]]()) else 0
      if (two && (n1 < 1 || n2 < 1)) {
        return(list(ready = FALSE, message = "Add at least one signature to each list."))
      }
      if (!two && n1 < 2) {
        return(list(ready = FALSE, message = "Add at least two signatures to List 1, or switch on List 2."))
      }
      list(
        ready = TRUE,
        message = if (two) sprintf("%d signature(s) against %d.", n1, n2) else sprintf("%d signatures, each against every other.", n1)
      )
    })

    output$run_readiness <- renderUI({
      r <- readiness()
      helpText(r$message)
    })

    observe({
      shinyjs::toggleState("run", condition = readiness()$ready)
    })

    # ---- running ------------------------------------------------------------

    observeEvent(input$run, {
      two <- isTRUE(input$two_lists)

      prepared <- tryCatch({
        build_list <- function(k) {
          r <- requests[[k]]()
          if (!is.null(r$upload_error)) stop(r$upload_error, call. = FALSE)
          list(
            signature_ids = r$signature_ids,
            signature_names = r$signature_names,
            omic_signatures = r$omic_signatures,
            label_pairing = read_pairing(k)
          )
        }
        list1 <- build_list(1)
        list2 <- if (two) build_list(2) else NULL
        list(args = compare_build_args(user_conn_handler(), list1, list2, settings()), error = NULL)
      }, error = function(e) list(args = NULL, error = conditionMessage(e)))

      if (!is.null(prepared$error)) {
        run_state(list(args = NULL, result = NULL, warnings = character(), error = prepared$error))
        return()
      }

      outcome <- withProgress(message = "Comparing signatures", detail = "Fetching and comparing; this can take a while for GSEA.", value = 0.2, {
        compare_run(prepared$args)
      })
      run_state(c(list(args = prepared$args), outcome))
    })

    result <- reactive({
      run_state()$result
    })

    output$has_result <- reactive({
      !is.null(result())
    })
    outputOptions(output, "has_result", suspendWhenHidden = FALSE)

    output$run_messages <- renderUI({
      state <- run_state()
      req(state)
      tagList(
        if (!is.null(state$error)) {
          div(class = "alert alert-danger compare-message", strong("The comparison could not run. "), state$error)
        },
        if (length(state$warnings) > 0) {
          div(
            class = "alert alert-warning",
            strong(sprintf("%d warning%s from the comparison", length(state$warnings), if (length(state$warnings) == 1) "" else "s")),
            lapply(state$warnings, function(w) div(class = "compare-message", w))
          )
        }
      )
    })

    output$result_summary <- renderUI({
      res <- result()
      req(res)
      comparisons <- compare_comparisons(res)
      first <- compare_result_matrix(res, names(comparisons)[1], compare_table_matrices(res)[1])
      self <- is.null(res$label_order$sig_list2) && identical(rownames(first), colnames(first))
      method_label <- names(COMPARE_METHOD_CHOICES)[match(res$method, COMPARE_METHOD_CHOICES)]
      item <- function(label, value) div(class = "compare-summary-item", strong(label), span(value))
      div(
        class = "compare-summary-grid",
        item("Method", if (is.na(method_label)) res$method else method_label),
        item("List 1", sprintf("%d signatures", nrow(first))),
        item("List 2", if (self) "Self-comparison" else sprintf("%d signatures", ncol(first))),
        item("Comparisons", paste(names(comparisons), collapse = ", ")),
        item("Background", if (is.null(res$background)) "Not used" else format(length(res$background), big.mark = ","))
      )
    })

    # ---- heatmap ------------------------------------------------------------

    output$heatmap_controls <- renderUI({
      res <- result()
      req(res)
      measures <- compare_heatmap_measures(res)
      modes <- compare_heatmap_modes(res)
      keep <- function(value, choices) if (!is.null(value) && value %in% choices) value else choices[1]
      measure_labels <- c(jaccard = "Jaccard", score = "Score", pvalue = "-log10(p-value)")
      mode_labels <- c(separate = "Separate panels", combined = "Combined (split cells)", split = "Split triangles")
      div(
        class = "compare-heatmap-controls",
        selectInput(ns("heatmap_measure"), "Measure",
                    choices = stats::setNames(measures, measure_labels[measures]),
                    selected = keep(isolate(input$heatmap_measure), measures)),
        selectInput(ns("heatmap_mode"), "Mode",
                    choices = stats::setNames(modes, mode_labels[modes]),
                    selected = keep(isolate(input$heatmap_mode), modes)),
        if (compare_is_symmetric(res)) {
          selectInput(ns("heatmap_triangle"), "Triangle", choices = c("upper", "lower"),
                      selected = keep(isolate(input$heatmap_triangle), c("upper", "lower")))
        },
        selectInput(ns("heatmap_cluster"), "Clustering",
                    choices = c("ward.D", "ward.D2", "complete", "average", "single", "mcquitty", "median", "centroid"),
                    selected = isolate(input$heatmap_cluster) %||% "ward.D"),
        if (compare_is_rank_based(res)) {
          selectInput(ns("heatmap_na_style"), "Cells not computed", choices = c("grey", "hatch"),
                      selected = keep(isolate(input$heatmap_na_style), c("grey", "hatch")))
        }
      )
    })

    heatmap_settings <- reactive({
      res <- result()
      req(res)
      pick <- function(value, choices) if (!is.null(value) && value %in% choices) value else choices[1]
      list(
        measure = pick(input$heatmap_measure, compare_heatmap_measures(res)),
        mode = pick(input$heatmap_mode, compare_heatmap_modes(res)),
        triangle = pick(input$heatmap_triangle, c("upper", "lower")),
        cluster_method = input$heatmap_cluster %||% "ward.D",
        na_style = pick(input$heatmap_na_style, c("grey", "hatch"))
      )
    })

    draw_heatmap <- function(res, s) {
      do.call(OmicSignature::signature_similarity_heatmap, c(
        list(
          res,
          measure = s$measure,
          mode = s$mode,
          triangle = s$triangle,
          cluster_method = s$cluster_method,
          na_style = s$na_style,
          draw = TRUE
        ),
        compare_heatmap_name_args(res)
      ))
    }

    # Size in inches, from the matrix and the number of panels.
    heatmap_size <- reactive({
      res <- result()
      req(res)
      m <- compare_result_matrix(res, names(compare_comparisons(res))[1], compare_table_matrices(res)[1])
      panels <- if (identical(heatmap_settings()$mode, "separate")) length(compare_comparisons(res)) else 1
      longest <- max(nchar(c(rownames(m), colnames(m))), 10)
      label_in <- min(longest * 0.1, 7)
      list(
        width = min(4 + label_in + panels * max(ncol(m), 2) * 0.45, 40),
        height = min(2.5 + label_in + max(nrow(m), 2) * 0.45, 40)
      )
    })

    output$heatmap <- renderPlot({
      res <- result()
      req(res)
      shiny::validate(shiny::need(
        requireNamespace("ComplexHeatmap", quietly = TRUE) && requireNamespace("circlize", quietly = TRUE),
        "Heatmaps need the ComplexHeatmap and circlize packages, which are not installed on this server."
      ))
      s <- heatmap_settings()
      drawn <- tryCatch({
        draw_heatmap(res, s)
        NULL
      }, error = function(e) conditionMessage(e))
      shiny::validate(shiny::need(is.null(drawn), paste("The heatmap could not be drawn:", drawn)))
    }, height = function() round(heatmap_size()$height * 80), width = function() round(heatmap_size()$width * 80))

    heatmap_download <- function(device) {
      downloadHandler(
        filename = function() sprintf("signature_comparison_heatmap_%s.%s", format(Sys.time(), "%Y%m%d_%H%M%S"), device),
        content = function(file) {
          size <- heatmap_size()
          if (device == "png") {
            grDevices::png(file, width = size$width, height = size$height, units = "in", res = 150)
          } else {
            grDevices::pdf(file, width = size$width, height = size$height)
          }
          on.exit(grDevices::dev.off(), add = TRUE)
          draw_heatmap(result(), heatmap_settings())
        }
      )
    }
    output$download_heatmap_png <- heatmap_download("png")
    output$download_heatmap_pdf <- heatmap_download("pdf")

    # ---- tables -------------------------------------------------------------

    result_datatable <- function(df, rownames = FALSE) {
      dt <- DT::datatable(
        df,
        extensions = "Buttons",
        filter = if (rownames) "none" else "top",
        rownames = rownames,
        class = "compact stripe hover nowrap",
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, -1),
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = c("copy", "csv", "excel")
        )
      )
      numeric_cols <- names(df)[vapply(df, function(x) is.double(x), logical(1))]
      if (length(numeric_cols) > 0) {
        dt <- DT::formatSignif(dt, columns = numeric_cols, digits = 4)
      }
      dt
    }

    output$pairs_table <- DT::renderDT({
      res <- result()
      req(res)
      result_datatable(compare_pairs_table(res))
    }, server = FALSE)

    output$matrix_controls <- renderUI({
      res <- result()
      req(res)
      comparisons <- names(compare_comparisons(res))
      matrices <- compare_table_matrices(res)
      keep <- function(value, choices) if (!is.null(value) && value %in% choices) value else choices[1]
      div(
        class = "compare-heatmap-controls",
        selectInput(ns("table_comparison"), "Comparison", choices = comparisons,
                    selected = keep(isolate(input$table_comparison), comparisons)),
        selectInput(ns("table_matrix"), "Matrix", choices = matrices,
                    selected = keep(isolate(input$table_matrix), matrices))
      )
    })

    output$matrix_table <- DT::renderDT({
      res <- result()
      req(res)
      comparisons <- names(compare_comparisons(res))
      matrices <- compare_table_matrices(res)
      comparison <- if (isTRUE(input$table_comparison %in% comparisons)) input$table_comparison else comparisons[1]
      matrix_name <- if (isTRUE(input$table_matrix %in% matrices)) input$table_matrix else matrices[1]
      m <- compare_result_matrix(res, comparison, matrix_name)
      df <- as.data.frame(m, check.names = FALSE)
      if (matrix_name == "counts") {
        df[] <- lapply(df, as.integer)
      }
      result_datatable(df, rownames = TRUE)
    }, server = FALSE)

    output$label_order_table <- DT::renderDT({
      res <- result()
      req(res)
      lo <- compare_label_order_table(res)
      if (is.null(lo)) {
        lo <- data.frame(Message = "Every signature is uni-directional, so there are no group label levels.")
      }
      result_datatable(lo)
    }, server = FALSE)

    # ---- reproducing ---------------------------------------------------------

    output$r_call <- renderText({
      state <- run_state()
      req(state$args, state$result)
      compare_r_call(state$args)
    })

    output$download_result <- downloadHandler(
      filename = function() sprintf("signature_comparison_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S")),
      content = function(file) saveRDS(result(), file)
    )

    list(run_state = run_state, pairing_input_ids = pairing_input_ids)
  })
}
