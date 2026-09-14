# compare page modules
#
# A front end to SigRepo::compareSignatures(). Each of its two signature lists
# is built from database ids (rows picked from the signature table, narrowed by
# facet dropdowns) and uploaded OmicSignature objects, and every comparison
# argument is exposed with the function's own default, which its label prints.
# The result is read back and plotted with
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

  # A label with the compareSignatures() default it starts at.
  with_default <- function(label, name) {
    tagList(label, " ", span(class = "compare-default", compare_default_text(name)))
  }

  list_panel <- function(k, title, description) {
    div(
      class = "compare-list",
      tags$h4(title),
      tags$p(class = "compare-muted", description),
      tags$label("Pick from the repository"),
      div(
        class = "compare-facets",
        lapply(names(COMPARE_FACETS), function(facet) {
          selectInput(ns(paste0("list", k, "_facet_", facet)), COMPARE_FACETS[[facet]], choices = c("All" = "all"))
        })
      ),
      DT::DTOutput(ns(paste0("list", k, "_table"))),
      div(
        class = "compare-actions",
        actionLink(ns(paste0("list", k, "_clear")), "Clear picked signatures", icon = icon("xmark"))
      ),
      br(),
      fileInput(
        ns(paste0("list", k, "_upload")),
        "Upload OmicSignature .rds",
        multiple = TRUE,
        accept = ".rds",
        width = "100%"
      ),
      helpText("An OmicSignature, a list of them, or an OmicSignatureCollection."),
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
      ", page_selector, " .compare-default { color: #597189; font-size: 12px; font-weight: normal; white-space: nowrap; }
      ", page_selector, " .compare-facets { display: flex; gap: 10px; flex-wrap: wrap; }
      ", page_selector, " .compare-facets .form-group { flex: 1 1 140px; min-width: 140px; margin-bottom: 8px; }
      ", page_selector, " .compare-matrix { overflow-x: auto; margin-bottom: 14px; }
      ", page_selector, " .compare-matrix table.dataTable { width: auto !important; margin: 0; }
      ", page_selector, " .compare-matrix table.dataTable th,
      ", page_selector, " .compare-matrix table.dataTable td { min-width: 64px; padding: 6px 10px; text-align: center; }
      ", page_selector, " .compare-matrix table.dataTable tbody th,
      ", page_selector, " .compare-matrix table.dataTable tbody td:first-child { font-weight: 600; background: #f6f9fc; }
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
            selectInput(ns("method"), tagList("Method", " ", span(class = "compare-default", "default (Overlap)")),
                        choices = COMPARE_METHOD_CHOICES, selected = COMPARE_DEFAULTS$method),
            uiOutput(ns("method_help")),
            fluidRow(
              column(6, numericInput(ns("score_cutoff"), with_default("Score cutoff (|score| ≥)", "score_cutoff"),
                                     value = COMPARE_DEFAULTS$score_cutoff, min = 0, step = 0.1)),
              column(6, numericInput(ns("adj_p_cutoff"), with_default("Adj. p cutoff (≤)", "adj_p_cutoff"),
                                     value = COMPARE_DEFAULTS$adj_p_cutoff, min = 0, max = 1, step = 0.01))
            ),
            fluidRow(
              column(6, numericInput(ns("min_features"), with_default("Min features", "min_features"),
                                     value = COMPARE_DEFAULTS$min_features, min = 3, step = 1)),
              column(6, numericInput(ns("max_feature"), with_default("Max features", "max_feature"),
                                     value = COMPARE_DEFAULTS$max_feature, min = 3, step = 10))
            ),
            fluidRow(
              column(6, selectInput(ns("alternative"), with_default("Alternative", "alternative"),
                                    choices = c("greater", "less", "two.sided"), selected = COMPARE_DEFAULTS$alternative)),
              column(6, selectInput(ns("p_adjust_method"), with_default("P-value adjustment", "p_adjust_method"),
                                    choices = stats::p.adjust.methods, selected = COMPARE_DEFAULTS$p_adjust_method))
            ),
            checkboxInput(ns("adjust"), with_default("Adjust p-values within each comparison", "adjust"),
                          value = COMPARE_DEFAULTS$adjust),
            conditionalPanel(
              condition = "input.method == 'gsea'",
              ns = ns,
              tags$h4("GSEA"),
              selectInput(ns("gsea_score"), with_default("Score to report", "gsea_score"),
                          choices = c("NES", "ES", "log2err", "size"), selected = COMPARE_DEFAULTS$gsea_score),
              fluidRow(
                column(6, numericInput(ns("min_size"), with_default("minSize", "minSize"),
                                       value = COMPARE_DEFAULTS$minSize, min = 1, step = 1)),
                column(6, numericInput(ns("max_size"), tagList("maxSize", " ", span(class = "compare-default", "default (Inf: leave blank)")),
                                       value = NA, min = 1, step = 1))
              )
            ),
            tags$details(
              tags$summary(strong("Advanced: background, columns, label pairing")),
              br(),
              textAreaInput(
                ns("background"),
                with_default("Background features (overlap tests)", "background"),
                placeholder = "Leave blank to use every feature in the compared signatures",
                rows = 3,
                width = "100%"
              ),
              fluidRow(
                column(6, textInput(ns("feature_col"), with_default("Feature column", "feature_col"), value = COMPARE_DEFAULTS$feature_col)),
                column(6, textInput(ns("score_col"), with_default("Score column", "score_col"), value = COMPARE_DEFAULTS$score_col))
              ),
              fluidRow(
                column(6, textInput(ns("adj_p_col"), with_default("Adj. p column", "adj_p_col"), value = COMPARE_DEFAULTS$adj_p_col)),
                column(6, textInput(ns("p_value_col"), with_default("P-value column", "p_value_col"), value = COMPARE_DEFAULTS$p_value_col))
              ),
              helpText(
                "KS and GSEA rank by the p-value column. A signature without it is ranked by its pvalue column,",
                "or by the adjusted p-value column if it has neither; the comparison warns when that happens."
              ),
              textInput(ns("group_col"), with_default("Group label column", "group_col"), value = COMPARE_DEFAULTS$group_col),
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
              helpText(
                "Every signature pair with every measure. Self-comparisons list each overlap pair once.",
                "Copy, CSV and Excel export the rows that pass the column filters, at full precision."
              ),
              DT::DTOutput(ns("pairs_table"))
            ),
            tabPanel(
              "Matrices",
              uiOutput(ns("matrix_controls")),
              helpText(
                "Rows are List 1 (S1, S2, ...); columns are List 2 (R1, R2, ...), or List 1 again for a self-comparison.",
                "The key below gives each label's signature. Exports use the full signature names."
              ),
              div(class = "compare-matrix", DT::DTOutput(ns("matrix_table"))),
              tags$h4("Key"),
              DT::DTOutput(ns("matrix_key"))
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

    facet_id <- function(k, facet) paste0("list", k, "_facet_", facet)

    # Refill each facet dropdown from the signatures the user can see, keeping
    # a choice that is still offered.
    observe({
      df <- db_table()
      for (k in 1:2) {
        for (facet in names(COMPARE_FACETS)) {
          choices <- compare_facet_choices(df, facet)
          current <- isolate(input[[facet_id(k, facet)]])
          updateSelectInput(session, facet_id(k, facet), choices = choices,
                            selected = if (isTRUE(current %in% choices)) current else "all")
        }
      }
    })

    # KS and GSEA rank against the ranking side's difexp tables, so switching
    # to one points that side's picker at signatures that have them.
    observeEvent(list(input$method, input$two_lists), {
      if (identical(input$method %||% COMPARE_DEFAULTS$method, "overlap")) {
        return()
      }
      k <- if (isTRUE(input$two_lists)) 2 else 1
      updateSelectInput(session, facet_id(k, "direction_type"), selected = "bi-directional")
      updateSelectInput(session, facet_id(k, "has_difexp"), selected = "yes")
    }, ignoreInit = TRUE)

    # The rows a list's picker shows, and the signature ids picked in it. Picks
    # are kept by id, not by row, so they survive the facets changing the rows.
    views <- lapply(1:2, function(k) {
      reactive({
        facets <- lapply(stats::setNames(names(COMPARE_FACETS), names(COMPARE_FACETS)), function(facet) input[[facet_id(k, facet)]])
        compare_facet_filter(picker_table(), facets)
      })
    })
    picks <- list(reactiveVal(character()), reactiveVal(character()))

    lapply(1:2, function(k) {
      table_id <- paste0("list", k, "_table")
      output[[table_id]] <- DT::renderDT({
        view <- views[[k]]()
        selected <- which(as.character(view$signature_id) %in% isolate(picks[[k]]()))
        DatatableFX(view, hidden_columns = integer(), scrollY = "260px",
                    row_selection = list(mode = "multiple", selected = selected))
      }, server = TRUE)

      observeEvent(input[[paste0(table_id, "_rows_selected")]], {
        view <- isolate(views[[k]]())
        rows <- input[[paste0(table_id, "_rows_selected")]]
        rows <- rows[rows >= 1 & rows <= nrow(view)]
        picks[[k]](compare_update_picks(picks[[k]](), view$signature_id, view$signature_id[rows]))
      }, ignoreNULL = FALSE, ignoreInit = TRUE)

      proxy <- DT::dataTableProxy(table_id)
      observeEvent(input[[paste0("list", k, "_clear")]], {
        picks[[k]](character())
        DT::selectRows(proxy, NULL)
      })
    })

    # One list's request from its two sources. Upload problems are kept so
    # they can be shown next to the list and stop a run.
    list_request <- function(k) {
      reactive({
        df <- db_table()
        picked <- picks[[k]]()
        uploads <- tryCatch(
          list(signatures = compare_read_signature_uploads(input[[paste0("list", k, "_upload")]]), error = NULL),
          error = function(e) list(signatures = list(), error = conditionMessage(e))
        )
        list(
          signature_ids = df$signature_id[match(picked, as.character(df$signature_id), nomatch = 0)],
          omic_signatures = uploads$signatures,
          upload_error = uploads$error
        )
      })
    }
    requests <- list(list_request(1), list_request(2))

    previews <- lapply(1:2, function(k) {
      reactive({
        r <- requests[[k]]()
        compare_preview_list(db_table(), r$signature_ids, r$omic_signatures)
      })
    })

    lapply(1:2, function(k) {
      output[[paste0("list", k, "_summary")]] <- renderUI({
        r <- requests[[k]]()
        p <- previews[[k]]()
        tagList(
          if (!is.null(r$upload_error)) div(class = "alert alert-danger compare-message", r$upload_error),
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
      input_ids <- c(minSize = "min_size", maxSize = "max_size")
      out <- lapply(stats::setNames(names(COMPARE_DEFAULTS), names(COMPARE_DEFAULTS)), function(nm) {
        id <- if (nm %in% names(input_ids)) input_ids[[nm]] else nm
        input[[id]] %||% COMPARE_DEFAULTS[[nm]]
      })
      out$background <- compare_parse_background(input$background)
      if (is.na(out$maxSize)) {
        out$maxSize <- COMPARE_DEFAULTS$maxSize
      }
      out
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
      run_state(c(
        list(
          args = prepared$args,
          previews = if (two) list(previews[[1]](), previews[[2]]()) else list(previews[[1]]()),
          stamp = format(Sys.time(), "%Y%m%d_%H%M%S")
        ),
        outcome
      ))
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
      self <- compare_is_self(res)
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

    # Copy, CSV and Excel buttons that save as `filename` (the extension is
    # added by the button) with no title row above the header. Buttons export
    # the 'display' rendering by default, which is where DT::formatSignif()
    # rounds; asking for 'export' gets the unrounded values, but then an NA
    # cell arrives as null and would be written as the text "null".
    #
    # A table whose view is headed by short labels passes the full names:
    # `header_names` for every column including the row names, `row_names`
    # for column 0. With ordering and search off, export row i is data row i.
    export_buttons <- function(filename, header_names = NULL, row_names = NULL) {
      row_js <- if (is.null(row_names)) "" else sprintf("if (column === 0) return %s[row]; ", jsonlite::toJSON(row_names))
      format <- list(body = DT::JS(sprintf(
        "function(data, row, column) { %sreturn data === null || data === undefined ? '' : data; }", row_js
      )))
      if (!is.null(header_names)) {
        format$header <- DT::JS(sprintf("function(data, column) { return %s[column]; }", jsonlite::toJSON(header_names)))
      }
      lapply(c("copy", "csv", "excel"), function(kind) {
        button <- list(extend = kind, title = "", exportOptions = list(orthogonal = "export", format = format))
        if (kind != "copy") {
          button$filename <- filename
        }
        button
      })
    }

    export_name <- function(...) {
      paste(c("signature_comparison", ..., run_state()$stamp), collapse = "_")
    }

    result_datatable <- function(df, filename) {
      dt <- DT::datatable(
        df,
        extensions = "Buttons",
        filter = "top",
        rownames = FALSE,
        class = "compact stripe hover nowrap",
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, -1),
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = export_buttons(filename)
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
      result_datatable(compare_pairs_table(res), export_name("pairs"))
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
      labels <- compare_matrix_labels(res)
      df <- as.data.frame(m, check.names = FALSE)
      if (matrix_name == "counts") {
        df[] <- lapply(df, as.integer)
      }
      rownames(df) <- unname(labels$rows[rownames(m)])
      colnames(df) <- unname(labels$cols[colnames(m)])

      dt <- DT::datatable(
        df,
        extensions = "Buttons",
        rownames = TRUE,
        selection = "none",
        class = "compact cell-border nowrap",
        # No scrollX: DataTables would split the header into its own table and
        # misalign it with the narrow cells. .compare-matrix scrolls instead.
        options = list(
          dom = "Bt",
          paging = FALSE,
          ordering = FALSE,
          searching = FALSE,
          info = FALSE,
          autoWidth = FALSE,
          buttons = export_buttons(export_name("matrix", comparison, matrix_name),
                                   header_names = c("signature", colnames(m)), row_names = rownames(m))
        )
      )
      if (matrix_name != "counts") {
        dt <- DT::formatSignif(dt, columns = colnames(df), digits = 3)
      }
      shading <- compare_matrix_shading(m, matrix_name, self = compare_is_self(res))
      if (!is.null(shading)) {
        dt <- DT::formatStyle(dt, columns = colnames(df), backgroundColor = DT::styleInterval(shading$cuts, shading$colors))
      }
      dt
    }, server = FALSE)

    output$matrix_key <- DT::renderDT({
      res <- result()
      req(res)
      result_datatable(compare_matrix_key(res, run_state()$previews), export_name("matrix_key"))
    }, server = FALSE)

    output$label_order_table <- DT::renderDT({
      res <- result()
      req(res)
      lo <- compare_label_order_table(res)
      if (is.null(lo)) {
        lo <- data.frame(Message = "Every signature is uni-directional, so there are no group label levels.")
      }
      result_datatable(lo, export_name("label_order"))
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

    list(run_state = run_state, pairing_input_ids = pairing_input_ids, previews = previews)
  })
}
