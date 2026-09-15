# annotate page modules
#
# A front end to SigRepo::runHypeR(). Signatures come from one source per run
# (repository picks, uploaded OmicSignature objects, or hypeR-native gene
# lists), genesets from the MSigDB cache, hyperdb rgsets or custom files, and
# every argument of the chosen test is exposed with the function's own default,
# which its label prints. prepareHypeRSignatures() previews the queries a run
# would test. The result is read back with SigRepo's hypeR plot and data
# functions and hypeR's own table. The helpers these call live in
# utils/annotate_utils.R (and, shared with the Compare tab, utils/compare_utils.R).

ANNOTATE_TABLE_COLUMNS <- c(
  "signature_id", "signature_name", "organism", "direction_type", "assay_type",
  "phenotype", "has_difexp", "user_name"
)

ANNOTATE_SOURCE_CHOICES <- c(
  "Repository" = "repository",
  "Upload .rds" = "upload",
  "Gene lists" = "genes"
)

ANNOTATE_TEST_HELP <- list(
  hypergeometric = paste(
    "Tests whether each query's genes overlap a geneset more than chance in a background population.",
    "With split on, every group_label (e.g. the up and down arms of a bi-directional signature) is its own query;",
    "categorical signatures are also split by score sign. Queries with fewer than min_query_genes genes found in",
    "the genesets are skipped."
  ),
  kstest = paste(
    "Ranks each signature's difexp table by score and tests whether a geneset's genes sit toward the top of the",
    "ranking (hypeR's one-sided KS test). Direction down ranks by the negated score, both tests each end as its own",
    "query, and a categorical signature is ranked within each category. The p-value does not depend on power."
  ),
  fgsea = paste(
    "Runs fgsea::fgseaMultilevel() once per difexp ranking and splits pathways by the sign of their enrichment",
    "score: up (ES > 0) and down (ES < 0). Power is fgsea's gseaParam and changes the p-values; each run is seeded",
    "so results repeat."
  )
)

annotate_module_ui <- function(id) {
  ns <- NS(id)
  page_selector <- paste0("#", ns("annotate_page"))

  # A label with the runHypeR() default it starts at.
  with_default <- function(label, name, test = NULL) {
    tagList(label, " ", span(class = "annotate-default", annotate_default_text(name, test)))
  }
  runtime_fetch <- runtime_msigdb_fetch_allowed()
  geneset_sources <- c("MSigDB" = "msigdb", if (runtime_fetch) c("hypeR rgsets (hierarchies)" = "rgsets"), "Custom" = "custom")
  species_choices <- unique(c(
    "Homo sapiens", "Mus musculus",
    tryCatch(msigdbr::msigdbr_species()$species_name, error = function(e) character())
  ))

  tagList(
    tags$style(HTML(paste0("
      ", page_selector, " { padding-top: 28px; padding-bottom: 32px; }
      ", page_selector, " .annotate-hero {
        margin-bottom: 18px; padding: 24px 28px; border-radius: 14px;
        background: linear-gradient(135deg, #0f3b63 0%, #1b5d8f 100%);
        color: #ffffff; box-shadow: 0 10px 24px rgba(15, 59, 99, 0.18);
      }
      ", page_selector, " .annotate-hero h2 { margin-top: 0; margin-bottom: 8px; font-weight: 700; }
      ", page_selector, " .annotate-hero p { margin-bottom: 0; color: rgba(255, 255, 255, 0.88); }
      ", page_selector, " .annotate-card {
        margin-bottom: 18px; padding: 20px 22px; border: 1px solid #d9e3ec; border-radius: 12px;
        background: #ffffff; box-shadow: 0 6px 18px rgba(15, 32, 56, 0.06);
      }
      ", page_selector, " .annotate-card h3, ", page_selector, " .annotate-card h4 {
        margin-top: 0; margin-bottom: 12px; color: #17324d; font-weight: 600;
      }
      ", page_selector, " .annotate-step-label {
        display: inline-block; margin-bottom: 10px; padding: 4px 10px; border-radius: 999px;
        background: #e9f2f9; color: #0f4d7c; font-size: 12px; font-weight: 700;
        letter-spacing: 0.04em; text-transform: uppercase;
      }
      ", page_selector, " .annotate-section + .annotate-section { margin-top: 18px; padding-top: 16px; border-top: 1px solid #e1ebf2; }
      ", page_selector, " .annotate-muted { color: #597189; }
      ", page_selector, " .annotate-default { color: #597189; font-size: 12px; font-weight: normal; white-space: nowrap; }
      ", page_selector, " .annotate-facets { display: flex; gap: 10px; flex-wrap: wrap; }
      ", page_selector, " .annotate-facets .form-group { flex: 1 1 140px; min-width: 140px; margin-bottom: 8px; }
      ", page_selector, " .annotate-list-summary { padding: 10px 14px; border-radius: 10px; background: #f6f9fc; border: 1px solid #e1ebf2; margin-top: 12px; }
      ", page_selector, " .annotate-list-summary ul { margin: 6px 0 0 0; padding-left: 18px; }
      ", page_selector, " .annotate-source { color: #597189; font-size: 12px; }
      ", page_selector, " .annotate-actions { display: flex; gap: 10px; flex-wrap: wrap; align-items: center; margin-top: 14px; }
      ", page_selector, " .annotate-summary-grid { display: grid; grid-template-columns: repeat(auto-fit, minmax(160px, 1fr)); gap: 12px; margin-bottom: 14px; }
      ", page_selector, " .annotate-summary-item { padding: 12px 14px; border-radius: 10px; background: #f6f9fc; border: 1px solid #e1ebf2; }
      ", page_selector, " .annotate-summary-item strong {
        display: block; margin-bottom: 4px; color: #0f3b63; font-size: 12px; text-transform: uppercase; letter-spacing: 0.04em;
      }
      ", page_selector, " .annotate-summary-item span { color: #17324d; font-size: 15px; font-weight: 600; }
      ", page_selector, " .annotate-empty { padding: 18px; border: 1px dashed #c5d5e3; border-radius: 10px; background: #f8fbfd; color: #4b647e; }
      ", page_selector, " .annotate-message { white-space: pre-wrap; margin-bottom: 8px; }
      ", page_selector, " .annotate-controls { display: flex; gap: 14px; flex-wrap: wrap; align-items: flex-end; }
      ", page_selector, " .annotate-controls .form-group { min-width: 130px; }
      ", page_selector, " .annotate-controls .checkbox { margin-bottom: 18px; }
      ", page_selector, " .annotate-status { display: inline-flex; align-items: center; gap: 8px; padding: 7px 12px; border-radius: 999px; font-size: 13px; font-weight: 600; }
      ", page_selector, " .annotate-status-pending { background: #eef3f7; color: #4b647e; }
      ", page_selector, " .annotate-status-ready { background: #e7f5ec; color: #21663c; }
      ", page_selector, " .annotate-status-error { background: #fdecea; color: #a12622; }
      ", page_selector, " .annotate-readiness ul { margin: 4px 0 8px 0; padding-left: 18px; }
      ", page_selector, " .annotate-readiness .annotate-problem { color: #a12622; }
      ", page_selector, " .annotate-readiness .annotate-note { color: #8a5a00; }
      ", page_selector, " .annotate-readiness .annotate-ok { color: #21663c; font-weight: 600; }
      ", page_selector, " .annotate-genes { display: flex; flex-wrap: wrap; gap: 4px; margin-top: 6px; max-height: 220px; overflow-y: auto; }
      ", page_selector, " .annotate-genes span { padding: 2px 7px; border-radius: 6px; background: #e9f2f9; color: #0f3b63; font-size: 12px; font-family: monospace; }
      ", page_selector, " .annotate-measures { width: 100%; margin-bottom: 12px; }
      ", page_selector, " .annotate-measures td { padding: 4px 8px 4px 0; vertical-align: top; border-bottom: 1px solid #eef3f7; }
      ", page_selector, " .annotate-measures td:first-child { color: #597189; white-space: nowrap; }
      ", page_selector, " .annotate-background-table { width: 100%; }
      ", page_selector, " .annotate-background-table td { padding: 2px 8px 2px 0; vertical-align: middle; }
      ", page_selector, " .annotate-background-table .form-group { margin-bottom: 4px; }
      ", page_selector, " .annotate-plot-scroll { overflow-x: auto; }
      ", page_selector, " details > summary { cursor: pointer; margin: 6px 0; }
      ", page_selector, " .tab-content { padding-top: 16px; }
    "))),

    div(
      id = ns("annotate_page"),

      div(
        class = "annotate-hero",
        tags$h2("Annotate Signatures"),
        tags$p(
          "Enrich signatures against genesets with hypergeometric, KS or GSEA (fgsea) tests using ",
          tags$code(style = "color: #ffffff; background: rgba(255,255,255,0.15);", "SigRepo::runHypeR()"),
          ", then explore the results with SigRepo's hypeR plots and get the R code that reproduces them."
        )
      ),

      fluidRow(
        column(
          width = 8,

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 1"),
            tags$h3("Signatures"),
            radioButtons(ns("source"), "Where the signatures come from", choices = ANNOTATE_SOURCE_CHOICES, inline = TRUE),
            conditionalPanel(
              condition = "input.source == 'repository'",
              ns = ns,
              tags$p(class = "annotate-muted", sprintf(
                "Pick up to %d signatures. Picks are kept by signature, so they stay when the filters change.", ANNOTATE_MAX_SIGNATURES
              )),
              div(
                class = "annotate-facets",
                lapply(names(COMPARE_FACETS), function(facet) {
                  selectInput(ns(paste0("facet_", facet)), COMPARE_FACETS[[facet]], choices = c("All" = "all"))
                })
              ),
              DT::DTOutput(ns("signature_table")),
              div(class = "annotate-actions", actionLink(ns("clear_picks"), "Clear picked signatures", icon = icon("xmark")))
            ),
            conditionalPanel(
              condition = "input.source == 'upload'",
              ns = ns,
              fileInput(ns("upload"), "Upload OmicSignature .rds", multiple = TRUE, accept = ".rds", width = "100%"),
              helpText(
                "An OmicSignature, a named list of them, or an OmicSignatureCollection. List names become the query labels.",
                "Gene symbols missing from a signature are looked up in the repository's reference tables."
              )
            ),
            conditionalPanel(
              condition = "input.source == 'genes'",
              ns = ns,
              textAreaInput(
                ns("gene_text"), "Gene lists", rows = 8, width = "100%",
                placeholder = "# Up in treatment\nTP53\nMYC, CDK4\n\n# Ranked by t statistic\nIL6 4.2\nTNF 3.1\nCXCL8 -2.7"
              ),
              fileInput(ns("gene_file"), "Or upload gene lists", accept = c(".txt", ".csv", ".tsv", ".gmt"), width = "100%"),
              helpText(
                "Start each list with '# name'. Genes can be separated by commas, spaces or new lines.",
                "Lines of 'GENE score' make a ranked list, used in the order given: the KS test accepts plain or ranked lists,",
                "GSEA needs ranked lists and the hypergeometric test plain ones.",
                "Files: GMT, a CSV/TSV with gene (and optional score and list) columns, or text in the same format."
              )
            ),
            uiOutput(ns("signature_summary"))
          ),

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 2"),
            tags$h3("Genesets"),
            radioButtons(ns("geneset_source"), NULL, choices = geneset_sources, inline = TRUE),
            conditionalPanel(
              condition = "input.geneset_source == 'msigdb'",
              ns = ns,
              div(
                class = "annotate-facets",
                selectInput(ns("species"), "Species", choices = species_choices, selected = "Homo sapiens"),
                selectInput(ns("collection"), "Collection", choices = annotate_msigdb_collections("Homo sapiens"), selected = "H"),
                selectInput(ns("subcollection"), "Subcollection", choices = c("None" = ""))
              ),
              helpText(
                "Collections load from this server's MSigDB cache.",
                if (runtime_fetch) "Anything not cached is fetched from msigdbr, which takes longer the first time." else
                  "Collections that are not cached cannot be loaded on this server.",
                "Mouse collections (MH, M1-M8) use the mouse database; other species map human collections to orthologs."
              )
            ),
            if (runtime_fetch) {
              conditionalPanel(
                condition = "input.geneset_source == 'rgsets'",
                ns = ns,
                selectInput(ns("rgsets"), "Hierarchy", choices = c("Loading..." = "")),
                helpText("Genesets with a parent-child hierarchy from hypeR's hyperdb, downloaded when loaded. Results can be drawn as hierarchy maps.")
              )
            },
            conditionalPanel(
              condition = "input.geneset_source == 'custom'",
              ns = ns,
              fileInput(ns("custom_file"), "GMT or CSV (geneset_name, gene_symbol)", accept = c(".gmt", ".csv"), width = "100%"),
              tags$details(
                tags$summary("Or type a single geneset"),
                textInput(ns("custom_name"), "Geneset name"),
                textAreaInput(ns("custom_genes"), "Genes", rows = 4, width = "100%", placeholder = "TP53, MYC, CDK4")
              )
            ),
            checkboxInput(ns("clean"), with_default("Clean geneset labels (e.g. HALLMARK_MYC_TARGETS_V1 -> Myc Targets V1)", "msigdb_clean"),
                          value = ANNOTATE_DEFAULTS$msigdb_clean),
            div(
              class = "annotate-actions",
              actionButton(ns("load_genesets"), "Load genesets", class = "btn-primary", icon = icon("layer-group")),
              uiOutput(ns("genesets_status"), inline = TRUE)
            )
          )
        ),

        column(
          width = 4,
          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 3"),
            tags$h3("Test"),
            selectInput(ns("test"), with_default("Test", "test"), choices = ANNOTATE_TEST_CHOICES, selected = ANNOTATE_DEFAULTS$test),
            uiOutput(ns("test_help")),

            conditionalPanel(
              condition = "input.test == 'hypergeometric'",
              ns = ns,
              conditionalPanel(
                condition = "input.source != 'genes'",
                ns = ns,
                checkboxInput(ns("split"), with_default("Split signatures by group label", "split"), value = ANNOTATE_DEFAULTS$split)
              ),
              numericInput(ns("min_query_genes"), with_default("Min query genes in genesets", "min_query_genes"),
                           value = ANNOTATE_DEFAULTS$min_query_genes, min = 1, step = 1)
            ),

            conditionalPanel(
              condition = "input.test != 'hypergeometric'",
              ns = ns,
              conditionalPanel(
                condition = "input.test == 'fgsea' || input.source != 'genes'",
                ns = ns,
                selectInput(ns("direction"), with_default("Direction", "direction"), choices = c("up", "down", "both"),
                            selected = ANNOTATE_DEFAULTS$direction)
              ),
              conditionalPanel(
                condition = "input.source != 'genes'",
                ns = ns,
                fluidRow(
                  column(6, selectInput(ns("ks_source"), with_default("Ranked table", "ks_source"), choices = c("difexp", "signature"),
                                        selected = ANNOTATE_DEFAULTS$ks_source)),
                  column(6, textInput(ns("score_col"), with_default("Score column", "score_col"), value = ANNOTATE_DEFAULTS$score_col))
                )
              ),
              fluidRow(
                column(6, numericInput(ns("power"), with_default("Power", "power"), value = ANNOTATE_DEFAULTS$power, min = 0, step = 0.5)),
                column(6, conditionalPanel(
                  condition = "input.test == 'kstest'",
                  ns = ns,
                  checkboxInput(ns("absolute"), with_default("Absolute", "absolute"), value = ANNOTATE_DEFAULTS$absolute)
                ))
              )
            ),

            conditionalPanel(
              condition = "input.test == 'fgsea'",
              ns = ns,
              tags$h4("fgsea"),
              fluidRow(
                column(6, numericInput(ns("seed"), with_default("Seed", "seed"), value = ANNOTATE_DEFAULTS$seed, step = 1)),
                column(6, numericInput(ns("sample_size"), with_default("sampleSize", "sampleSize"),
                                       value = ANNOTATE_FGSEA_DEFAULTS$sampleSize, min = 1, step = 10))
              ),
              fluidRow(
                column(6, numericInput(ns("min_size"), with_default("minSize", "minSize"), value = ANNOTATE_FGSEA_DEFAULTS$minSize, min = 1, step = 1)),
                column(6, numericInput(ns("max_size"), tagList("maxSize", " ", span(class = "annotate-default", "default (Inf: leave blank)")),
                                       value = NA, min = 1, step = 10))
              )
            ),

            tags$h4("Significance"),
            selectInput(ns("fdr_scope"), with_default("FDR adjusted across", "fdr_scope"),
                        choices = c("The whole run" = "run", "Each query" = "query"), selected = ANNOTATE_DEFAULTS$fdr_scope),
            fluidRow(
              column(6, numericInput(ns("pval"), with_default("Keep p ≤", "pval"), value = ANNOTATE_DEFAULTS$pval, min = 0, max = 1, step = 0.01)),
              column(6, numericInput(ns("fdr"), with_default("Keep FDR ≤", "fdr"), value = ANNOTATE_DEFAULTS$fdr, min = 0, max = 1, step = 0.01))
            ),
            helpText("These cutoffs filter the stored result. Leave them at 1 to keep every geneset and filter the plots instead."),

            tags$details(
              tags$summary(strong("Background")),
              selectInput(
                ns("background_mode"), with_default("Background", "background"),
                choices = c(
                  "Auto (difexp when complete, else 23467)" = "default",
                  "Population size" = "number",
                  "Each signature's difexp genes" = "difexp",
                  "Gene universe" = "genes",
                  "Per signature" = "per_signature"
                )
              ),
              conditionalPanel(
                condition = "input.background_mode == 'number'",
                ns = ns,
                numericInput(ns("background_number"), "Population size", value = 23467, min = 1, step = 100)
              ),
              conditionalPanel(
                condition = "input.background_mode == 'genes'",
                ns = ns,
                textAreaInput(ns("background_genes"), "Background genes", rows = 3, width = "100%",
                              placeholder = "Every gene measured, separated by commas or new lines")
              ),
              conditionalPanel(
                condition = "input.background_mode == 'per_signature'",
                ns = ns,
                uiOutput(ns("background_table"))
              ),
              helpText(
                "Auto uses each signature's measured difexp genes for the hypergeometric test when the difexp looks complete,",
                "and 23467 with a warning when it looks filtered. KS and GSEA rank the whole table, so a population size",
                "does not change them; a gene universe or difexp background reduces queries and genesets to those genes."
              )
            )
          ),

          div(
            class = "annotate-card",
            span(class = "annotate-step-label", "Step 4"),
            tags$h3("Preview and run"),
            uiOutput(ns("readiness")),
            div(
              class = "annotate-actions",
              actionButton(ns("preview"), "Preview queries", icon = icon("list-check")),
              actionButton(ns("run"), "Run enrichment", class = "btn-primary", icon = icon("play"))
            ),
            uiOutput(ns("preview_output"))
          )
        )
      ),

      div(
        class = "annotate-card",
        span(class = "annotate-step-label", "Results"),
        uiOutput(ns("run_messages")),
        conditionalPanel(
          condition = "!output.has_result",
          ns = ns,
          div(class = "annotate-empty", "Run an enrichment to see the dot plot, enrichment curves, maps, tables and the equivalent R code.")
        ),
        conditionalPanel(
          condition = "output.has_result",
          ns = ns,
          uiOutput(ns("result_summary")),
          tabsetPanel(
            id = ns("result_tabs"),
            tabPanel(
              "Dot plot",
              uiOutput(ns("dot_controls")),
              uiOutput(ns("dot_hint")),
              div(class = "annotate-plot-scroll", plotOutput(ns("dot_plot"), height = "auto", width = "auto")),
              div(
                class = "annotate-actions",
                downloadButton(ns("download_dots_png"), "PNG"),
                downloadButton(ns("download_dots_pdf"), "PDF")
              ),
              tags$h4(style = "margin-top: 18px;", "Signature key"),
              DT::DTOutput(ns("signature_key"))
            ),
            tabPanel(
              "Enrichment",
              uiOutput(ns("enrichment_controls")),
              fluidRow(
                column(7, plotOutput(ns("enrichment_plot"), height = "480px")),
                column(5, uiOutput(ns("enrichment_details")))
              ),
              div(
                class = "annotate-actions",
                downloadButton(ns("download_enrichment_png"), "PNG"),
                downloadButton(ns("download_enrichment_pdf"), "PDF")
              )
            ),
            tabPanel(
              "Maps",
              uiOutput(ns("map_controls")),
              uiOutput(ns("map_message")),
              visNetwork::visNetworkOutput(ns("map"), height = "640px")
            ),
            tabPanel(
              "Results",
              helpText(
                "Every query's hypeR table. Select a row to open its enrichment plot.",
                "Copy, CSV and Excel export the rows that pass the column filters, at full precision."
              ),
              DT::DTOutput(ns("results_table"))
            ),
            tabPanel(
              "Provenance",
              helpText(
                "How each query was built and tested: hypeR's own record of the test and the keys runHypeR() adds",
                "(symbol source, background used, genes removed, genesets dropped, FDR scope)."
              ),
              DT::DTOutput(ns("provenance_table"))
            ),
            tabPanel(
              "hypeR table",
              helpText("The result rendered by hypeR::rctbl_build(): it is hypeR's own hyp/multihyp, so every hypeR function works on it."),
              uiOutput(ns("hyper_table"))
            ),
            tabPanel(
              "R code",
              helpText("The same run and the plots as currently set, from R."),
              verbatimTextOutput(ns("r_code")),
              div(
                class = "annotate-actions",
                downloadButton(ns("download_result"), "Result (.rds)"),
                downloadButton(ns("download_excel"), "Excel (hypeRToExcel)"),
                downloadButton(ns("download_genesets"), "Genesets (.rds)"),
                uiOutput(ns("download_gene_lists_ui"), inline = TRUE)
              )
            )
          )
        )
      )
    )
  )
}


# `runner`, `previewer` and `geneset_loader` are the calls that do the work;
# tests replace them. `geneset_loader(request)` takes the Step 2 request (source
# plus that source's inputs) and returns list(genesets, description).
annotate_module_server <- function(id, signature_db, user_conn_handler,
                                   runner = SigRepo::runHypeR,
                                   previewer = SigRepo::prepareHypeRSignatures,
                                   geneset_loader = annotate_default_geneset_loader) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    # args, result, warnings, error of the last run; NULL before the first.
    run_state <- reactiveVal(NULL)
    preview_state <- reactiveVal(NULL)
    # genesets, description, error of the last load.
    genesets_state <- reactiveVal(NULL)
    pick_message <- reactiveVal(NULL)

    # ---- repository picker ----------------------------------------------------

    db_table <- reactive({
      df <- signature_db()
      if (!is.data.frame(df) || !"signature_id" %in% names(df)) {
        return(data.frame(signature_id = numeric(), signature_name = character(), stringsAsFactors = FALSE))
      }
      df
    })

    observe({
      df <- db_table()
      for (facet in names(COMPARE_FACETS)) {
        choices <- compare_facet_choices(df, facet)
        current <- isolate(input[[paste0("facet_", facet)]])
        updateSelectInput(session, paste0("facet_", facet), choices = choices,
                          selected = if (isTRUE(current %in% choices)) current else "all")
      }
    })

    # Ranked tests on the difexp table need signatures that have one.
    observeEvent(list(input$test, input$ks_source), {
      if (!identical(input$test %||% "hypergeometric", "hypergeometric") && identical(input$ks_source %||% "difexp", "difexp")) {
        updateSelectInput(session, "facet_has_difexp", selected = "yes")
      }
    }, ignoreInit = TRUE)

    view <- reactive({
      df <- db_table()
      facets <- lapply(stats::setNames(names(COMPARE_FACETS), names(COMPARE_FACETS)), function(facet) input[[paste0("facet_", facet)]])
      compare_facet_filter(df[, intersect(ANNOTATE_TABLE_COLUMNS, names(df)), drop = FALSE], facets)
    })
    picks <- reactiveVal(character())

    output$signature_table <- DT::renderDT({
      v <- view()
      selected <- which(as.character(v$signature_id) %in% isolate(picks()))
      DatatableFX(v, hidden_columns = integer(), scrollY = "280px",
                  row_selection = list(mode = "multiple", selected = selected))
    }, server = TRUE)

    proxy <- DT::dataTableProxy("signature_table")

    observeEvent(input$signature_table_rows_selected, {
      v <- isolate(view())
      rows <- input$signature_table_rows_selected
      rows <- rows[rows >= 1 & rows <= nrow(v)]
      updated <- compare_update_picks(picks(), v$signature_id, v$signature_id[rows])
      if (length(updated) > ANNOTATE_MAX_SIGNATURES) {
        pick_message(sprintf("A run takes at most %d signatures, so the last %d picked were left out.",
                             ANNOTATE_MAX_SIGNATURES, length(updated) - ANNOTATE_MAX_SIGNATURES))
        updated <- updated[seq_len(ANNOTATE_MAX_SIGNATURES)]
        DT::selectRows(proxy, which(as.character(v$signature_id) %in% updated))
      } else {
        pick_message(NULL)
      }
      picks(updated)
    }, ignoreNULL = FALSE, ignoreInit = TRUE)

    observeEvent(input$clear_picks, {
      picks(character())
      pick_message(NULL)
      DT::selectRows(proxy, NULL)
    })

    picked_rows <- reactive({
      df <- db_table()
      df[match(picks(), as.character(df$signature_id), nomatch = 0), , drop = FALSE]
    })

    # ---- uploads and gene lists ---------------------------------------------------

    uploads <- reactive({
      tryCatch(
        list(signatures = compare_read_signature_uploads(input$upload), error = NULL),
        error = function(e) list(signatures = list(), error = conditionMessage(e))
      )
    })

    gene_lists <- reactive({
      tryCatch({
        typed <- annotate_parse_gene_lists(input$gene_text)
        filed <- if (!is.null(input$gene_file)) annotate_read_gene_list_file(input$gene_file$datapath, input$gene_file$name) else list()
        both <- c(typed, filed)
        if (anyDuplicated(names(both))) {
          stop(sprintf("Gene list names must be unique: %s is both typed and in the file.",
                       paste(sprintf("'%s'", unique(names(both)[duplicated(names(both))])), collapse = ", ")), call. = FALSE)
        }
        list(lists = both, error = NULL)
      }, error = function(e) list(lists = list(), error = conditionMessage(e)))
    })

    output$signature_summary <- renderUI({
      source <- input$source %||% "repository"
      item <- function(name, detail) tags$li(name, " ", span(class = "annotate-source", detail))
      box <- function(title, items, error = NULL, message = NULL) {
        tagList(
          if (!is.null(error)) div(class = "alert alert-danger annotate-message", error),
          if (!is.null(message)) div(class = "alert alert-warning annotate-message", message),
          div(class = "annotate-list-summary", strong(title), if (length(items) > 0) tags$ul(items))
        )
      }
      plural <- function(n, word) sprintf("%d %s%s", n, word, if (n == 1) "" else "s")

      if (identical(source, "repository")) {
        rows <- picked_rows()
        box(
          sprintf("%s picked (of %d)", plural(nrow(rows), "signature"), ANNOTATE_MAX_SIGNATURES),
          lapply(seq_len(nrow(rows)), function(i) {
            item(rows$signature_name[i], sprintf("(id %s, %s, %s%s)", rows$signature_id[i], rows$organism[i] %||% "",
                                                 rows$direction_type[i] %||% "",
                                                 if (isTRUE(as.integer(rows$has_difexp[i]) == 1L)) ", difexp" else ", no difexp"))
          }),
          message = pick_message()
        )
      } else if (identical(source, "upload")) {
        u <- uploads()
        box(
          sprintf("%s uploaded", plural(length(u$signatures), "signature")),
          lapply(names(u$signatures), function(nm) {
            meta <- u$signatures[[nm]]$metadata
            item(nm, sprintf("(%s, %s%s)", meta$organism %||% "organism unknown", meta$direction_type %||% "direction unknown",
                             if (is.null(u$signatures[[nm]]$difexp)) ", no difexp" else ", difexp"))
          }),
          error = u$error
        )
      } else {
        g <- gene_lists()
        p <- annotate_gene_list_preview(g$lists)
        box(
          sprintf("%s", plural(nrow(p), "gene list")),
          lapply(seq_len(nrow(p)), function(i) item(p$name[i], sprintf("(%s, %s genes)", p$kind[i], p$n_genes[i]))),
          error = g$error
        )
      }
    })

    # ---- genesets ---------------------------------------------------------------

    observeEvent(input$species, {
      choices <- annotate_msigdb_collections(input$species)
      current <- isolate(input$collection)
      updateSelectInput(session, "collection", choices = choices, selected = if (isTRUE(current %in% choices)) current else choices[1])
    }, ignoreInit = TRUE)

    observeEvent(input$collection, {
      subs <- annotate_msigdb_subcollections(input$collection)
      choices <- if (identical(subs, "")) c("None" = "") else stats::setNames(subs, subs)
      updateSelectInput(session, "subcollection", choices = choices, selected = unname(choices)[1])
    })

    rgsets_loaded <- reactiveVal(FALSE)
    observeEvent(input$geneset_source, {
      if (!identical(input$geneset_source, "rgsets") || rgsets_loaded()) {
        return()
      }
      available <- tryCatch(hypeR::hyperdb_available(), error = function(e) NULL)
      choices <- annotate_rgsets_choices(available)
      updateSelectInput(session, "rgsets", choices = stats::setNames(paste(choices$source, choices$version, sep = "|"), choices$label))
      rgsets_loaded(TRUE)
    })

    # A change to what would be loaded drops what was loaded, so a run never
    # uses genesets the picker no longer shows.
    observeEvent(list(input$geneset_source, input$species, input$collection, input$subcollection, input$rgsets,
                      input$custom_file, input$clean), {
      if (!is.null(genesets_state())) {
        genesets_state(NULL)
      }
    }, ignoreInit = TRUE)

    geneset_request <- function() {
      source <- input$geneset_source %||% "msigdb"
      switch(
        source,
        msigdb = list(source = source, species = input$species, collection = input$collection,
                      subcollection = input$subcollection %||% "", clean = isTRUE(input$clean)),
        rgsets = {
          parts <- strsplit(input$rgsets %||% "", "|", fixed = TRUE)[[1]]
          list(source = source, rgsets = parts[1], version = parts[2])
        },
        custom = list(source = source, file = input$custom_file, name = input$custom_name,
                      genes = input$custom_genes, clean = isTRUE(input$clean))
      )
    }

    observeEvent(input$load_genesets, {
      request <- geneset_request()
      loaded <- withProgress(message = "Loading genesets", value = 0.3, {
        tryCatch(c(geneset_loader(request), list(error = NULL)), error = function(e) list(genesets = NULL, description = NULL, error = conditionMessage(e)))
      })
      genesets_state(loaded)
    })

    output$genesets_status <- renderUI({
      state <- genesets_state()
      if (is.null(state)) {
        span(class = "annotate-status annotate-status-pending", icon("circle"), "No genesets loaded")
      } else if (!is.null(state$error)) {
        span(class = "annotate-status annotate-status-error", icon("triangle-exclamation"), state$error)
      } else {
        span(class = "annotate-status annotate-status-ready", icon("circle-check"), annotate_genesets_label(state$description))
      }
    })

    # ---- test settings ------------------------------------------------------------

    output$test_help <- renderUI({
      helpText(ANNOTATE_TEST_HELP[[input$test %||% "hypergeometric"]])
    })

    # fgsea tests both tails in one run, so its direction defaults to both.
    observeEvent(input$test, {
      test <- input$test
      if (!identical(test, "hypergeometric")) {
        updateSelectInput(
          session, "direction",
          label = paste("Direction", annotate_default_text("direction", test)),
          selected = if (identical(test, "fgsea")) "both" else if (identical(isolate(input$direction), "both")) "both" else "up"
        )
      }
    }, ignoreInit = TRUE)

    # The keys a per-signature background is given for: signature ids for
    # repository picks, list names for uploads.
    background_keys <- reactive({
      source <- input$source %||% "repository"
      if (identical(source, "repository")) {
        rows <- picked_rows()
        data.frame(key = as.character(rows$signature_id), label = rows$signature_name, stringsAsFactors = FALSE)
      } else if (identical(source, "upload")) {
        nms <- names(uploads()$signatures)
        data.frame(key = nms, label = nms, stringsAsFactors = FALSE)
      } else {
        data.frame(key = character(), label = character(), stringsAsFactors = FALSE)
      }
    })

    background_input_id <- function(key, part) {
      sprintf("bg_%s_%s", digest::digest(key, algo = "crc32", serialize = FALSE), part)
    }

    output$background_table <- renderUI({
      keys <- background_keys()
      if (nrow(keys) == 0) {
        return(helpText("Pick or upload signatures to set a background for each one. Gene lists take a single background."))
      }
      tags$table(
        class = "annotate-background-table",
        lapply(seq_len(nrow(keys)), function(i) {
          mode_id <- background_input_id(keys$key[i], "mode")
          value_id <- background_input_id(keys$key[i], "value")
          tags$tr(
            tags$td(div(keys$label[i])),
            tags$td(selectInput(ns(mode_id), NULL, choices = c("Size" = "number", "difexp" = "difexp"),
                                selected = isolate(input[[mode_id]]) %||% "number", width = "100px")),
            tags$td(numericInput(ns(value_id), NULL, value = isolate(input[[value_id]]) %||% 23467, min = 1, width = "110px"))
          )
        })
      )
    })

    background <- reactive({
      mode <- input$background_mode %||% "default"
      per_signature <- if (identical(mode, "per_signature")) {
        keys <- background_keys()
        data.frame(
          key = keys$key,
          mode = vapply(keys$key, function(k) input[[background_input_id(k, "mode")]] %||% "number", character(1)),
          value = vapply(keys$key, function(k) as.character(input[[background_input_id(k, "value")]] %||% 23467), character(1)),
          stringsAsFactors = FALSE
        )
      }
      tryCatch(
        list(value = annotate_build_background(mode, number = input$background_number, genes_text = input$background_genes,
                                               per_signature = per_signature), error = NULL),
        error = function(e) list(value = NULL, error = conditionMessage(e))
      )
    })

    settings <- reactive({
      test <- input$test %||% ANNOTATE_DEFAULTS$test
      max_size <- input$max_size
      list(
        test = test,
        split = input$split %||% ANNOTATE_DEFAULTS$split,
        direction = input$direction %||% annotate_default_direction(test),
        ks_source = input$ks_source %||% ANNOTATE_DEFAULTS$ks_source,
        score_col = trimws(input$score_col %||% ANNOTATE_DEFAULTS$score_col),
        min_query_genes = input$min_query_genes %||% ANNOTATE_DEFAULTS$min_query_genes,
        fdr_scope = input$fdr_scope %||% ANNOTATE_DEFAULTS$fdr_scope,
        seed = input$seed %||% ANNOTATE_DEFAULTS$seed,
        fgsea_args = list(
          sampleSize = input$sample_size %||% ANNOTATE_FGSEA_DEFAULTS$sampleSize,
          minSize = input$min_size %||% ANNOTATE_FGSEA_DEFAULTS$minSize,
          maxSize = if (is.null(max_size) || is.na(max_size)) Inf else max_size
        ),
        background = background()$value,
        power = input$power %||% ANNOTATE_DEFAULTS$power,
        absolute = input$absolute %||% ANNOTATE_DEFAULTS$absolute,
        pval = input$pval %||% ANNOTATE_DEFAULTS$pval,
        fdr = input$fdr %||% ANNOTATE_DEFAULTS$fdr
      )
    })

    # ---- readiness -------------------------------------------------------------

    # problems stop a run (preview_problems stop a preview); notes are worth
    # knowing but the client handles them.
    readiness <- reactive({
      source <- input$source %||% "repository"
      s <- settings()
      problems <- character()
      notes <- character()

      if (identical(source, "repository")) {
        rows <- picked_rows()
        if (nrow(rows) == 0) problems <- c(problems, "Pick at least one signature from the repository.")
        ranked_difexp <- !identical(s$test, "hypergeometric") && identical(s$ks_source, "difexp")
        no_difexp <- rows$signature_name[!is.na(rows$has_difexp) & as.integer(rows$has_difexp) != 1L]
        if (ranked_difexp && length(no_difexp) > 0) {
          notes <- c(notes, sprintf("%s has no difexp table and will be skipped; rank the signature table instead (Ranked table: signature).",
                                    paste(sprintf("'%s'", no_difexp), collapse = ", ")))
        }
        state <- genesets_state()
        species <- state$description$species
        if (!is.null(species) && nrow(rows) > 0 && "organism" %in% names(rows)) {
          other <- rows$signature_name[!is.na(rows$organism) & tolower(rows$organism) != tolower(species)]
          if (length(other) > 0) {
            notes <- c(notes, sprintf("%s %s not %s, the species of the loaded genesets.",
                                      paste(sprintf("'%s'", other), collapse = ", "), if (length(other) == 1) "is" else "are", species))
          }
        }
      } else if (identical(source, "upload")) {
        u <- uploads()
        if (!is.null(u$error)) problems <- c(problems, u$error)
        if (length(u$signatures) == 0 && is.null(u$error)) problems <- c(problems, "Upload at least one OmicSignature .rds file.")
        if (length(u$signatures) > ANNOTATE_MAX_SIGNATURES) {
          problems <- c(problems, sprintf("A run takes at most %d signatures; the uploads hold %d.", ANNOTATE_MAX_SIGNATURES, length(u$signatures)))
        }
      } else {
        g <- gene_lists()
        problems <- c(problems, if (!is.null(g$error)) g$error else annotate_check_gene_lists(g$lists, s$test))
      }
      preview_problems <- problems

      if (identical(source, "genes") && identical(input$background_mode, "per_signature")) {
        problems <- c(problems, "A per-signature background needs repository or uploaded signatures; gene lists take one background.")
      }
      if (!is.null(background()$error)) problems <- c(problems, background()$error)
      if (!identical(s$test, "hypergeometric") && identical(source, "repository") && !nzchar(s$score_col)) {
        problems <- c(problems, "Enter the score column to rank by.")
      }
      state <- genesets_state()
      if (is.null(state$genesets)) problems <- c(problems, "Load genesets in Step 2.")

      list(ready = length(problems) == 0, preview_ready = length(preview_problems) == 0,
           problems = unique(problems), notes = unique(notes))
    })

    output$readiness <- renderUI({
      r <- readiness()
      div(
        class = "annotate-readiness",
        if (r$ready) {
          div(class = "annotate-ok", icon("circle-check"), " Ready to run.")
        } else {
          tags$ul(lapply(r$problems, function(p) tags$li(class = "annotate-problem", p)))
        },
        if (length(r$notes) > 0) tags$ul(lapply(r$notes, function(n) tags$li(class = "annotate-note", n)))
      )
    })

    observe({
      r <- readiness()
      shinyjs::toggleState("run", condition = r$ready)
      shinyjs::toggleState("preview", condition = r$preview_ready)
    })

    # ---- building, previewing and running ------------------------------------

    build_args <- function(genesets = genesets_state()$genesets) {
      source <- input$source %||% "repository"
      annotate_build_args(
        source,
        conn_handler = user_conn_handler(),
        signature_ids = if (identical(source, "repository")) picked_rows()$signature_id,
        omic_signatures = if (identical(source, "upload")) uploads()$signatures,
        gene_lists = if (identical(source, "genes")) gene_lists()$lists,
        genesets = genesets,
        settings = settings()
      )
    }

    # What a run depends on, to tell when the inputs have moved on from it.
    run_key <- function(args, description) {
      keep <- setdiff(names(args), c("conn_handler", "genesets", "omic_signature"))
      digest::digest(list(args[keep], names(args$omic_signature), description), algo = "md5")
    }

    observeEvent(input$preview, {
      req(readiness()$preview_ready)
      args <- build_args(genesets = NULL)
      outcome <- withProgress(message = "Building queries", value = 0.3, annotate_preview(args, runner = previewer))
      preview_state(c(outcome, list(stamp = format(Sys.time(), "%H:%M:%S"))))
    })

    output$preview_output <- renderUI({
      state <- preview_state()
      req(state)
      tagList(
        tags$h4(style = "margin-top: 16px;", sprintf("Query preview (%s)", state$stamp)),
        if (!is.null(state$error)) div(class = "alert alert-danger annotate-message", state$error),
        if (length(state$warnings) > 0) div(class = "alert alert-warning", lapply(state$warnings, function(w) div(class = "annotate-message", w))),
        if (!is.null(state$result)) {
          tagList(
            DT::DTOutput(ns("preview_info")),
            if (nrow(state$result$skipped) > 0) tagList(tags$h4("Skipped"), DT::DTOutput(ns("preview_skipped")))
          )
        }
      )
    })

    small_table <- function(df) {
      DT::datatable(df, rownames = FALSE, selection = "none", class = "compact stripe nowrap",
                    options = list(dom = "t", paging = FALSE, scrollX = TRUE, ordering = FALSE))
    }
    output$preview_info <- DT::renderDT({
      state <- preview_state()
      req(state$result)
      small_table(state$result$info)
    })
    output$preview_skipped <- DT::renderDT({
      state <- preview_state()
      req(state$result)
      small_table(state$result$skipped)
    })

    observeEvent(input$run, {
      req(readiness()$ready)
      loaded <- genesets_state()
      args <- build_args(genesets = loaded$genesets)
      outcome <- withProgress(
        message = "Running enrichment",
        detail = if (identical(args$test, "fgsea")) "fgsea runs one ranking at a time; large collections take a while." else "Fetching signatures and testing genesets.",
        value = 0.2,
        annotate_run(args, runner = runner)
      )
      run_state(c(
        list(
          args = args,
          source = input$source %||% "repository",
          genesets_description = loaded$description,
          key = run_key(args, loaded$description),
          # Two runs in the same second are still two runs to observers.
          run = input$run,
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
      stale <- !is.null(state$result) && tryCatch(
        !identical(run_key(build_args(genesets = NULL), genesets_state()$description), state$key),
        error = function(e) TRUE
      )
      tagList(
        if (!is.null(state$error)) {
          div(class = "alert alert-danger annotate-message", strong("The enrichment could not run. "), state$error)
        },
        if (length(state$warnings) > 0) {
          div(
            class = "alert alert-warning",
            strong(sprintf("%d warning%s from runHypeR()", length(state$warnings), if (length(state$warnings) == 1) "" else "s")),
            lapply(state$warnings, function(w) div(class = "annotate-message", w))
          )
        },
        if (stale) {
          div(class = "alert alert-info", icon("circle-info"), " The settings have changed since this run. Run again to update the results.")
        }
      )
    })

    output$result_summary <- renderUI({
      res <- result()
      req(res)
      s <- annotate_summary(res, fdr = 0.05)
      item <- function(label, value) div(class = "annotate-summary-item", strong(label), span(value))
      div(
        class = "annotate-summary-grid",
        item("Test", ANNOTATE_TEST_LABELS[[s$test]] %||% s$test),
        item("Queries", s$n_queries),
        item("Genesets", sprintf("%s: %s", s$genesets_name, format(s$n_genesets, big.mark = ","))),
        item("Rows kept", format(s$n_rows, big.mark = ",")),
        item("Genesets at FDR ≤ 0.05", s$n_significant),
        item("FDR scope", paste(s$fdr_scope, collapse = ", ")),
        item("Background", paste(s$backgrounds, collapse = ", "))
      )
    })

    # A new result starts on the dot plot with fresh selections.
    observeEvent(run_state(), {
      updateTabsetPanel(session, "result_tabs", selected = "Dot plot")
    }, ignoreInit = TRUE)

    export_name <- function(...) {
      paste(c("sigrepo_annotate", ..., run_state()$stamp), collapse = "_")
    }

    # Outputs draw with tryCatch(..., error = conditionMessage), so a string is
    # a failure to show in place. need() evaluates its message even when the
    # check passes, so the message is only built for a failure.
    # Runs a drawing step and returns its error message instead of raising it.
    # req() stops with Shiny's silent error, which has to reach Shiny rather
    # than be shown as a failure.
    attempt <- function(expr) {
      tryCatch(expr, error = function(e) {
        if (inherits(e, "shiny.silent.error")) stop(e)
        conditionMessage(e)
      })
    }

    stop_if_failed <- function(value, what) {
      failed <- is.character(value) && !inherits(value, "shiny.tag")
      shiny::validate(shiny::need(!failed, if (failed) paste(what, value)))
    }

    keep_choice <- function(value, choices, fallback = choices[1]) {
      if (!is.null(value) && isTRUE(value %in% choices)) value else fallback
    }

    # ---- dot plot ----------------------------------------------------------------

    output$dot_controls <- renderUI({
      res <- result()
      req(res)
      color_choices <- c("Significance" = "significance", if (annotate_is_ranked(res)) c("Score (NES / ES)" = "score"))
      div(
        class = "annotate-controls",
        selectInput(ns("dot_val"), "Rank by", choices = c("FDR" = "fdr", "p-value" = "pval"),
                    selected = keep_choice(isolate(input$dot_val), c("fdr", "pval")), width = "110px"),
        numericInput(ns("dot_cutoff"), "Cutoff (≤)", value = isolate(input$dot_cutoff) %||% 0.05, min = 0, max = 1, step = 0.01, width = "100px"),
        numericInput(ns("dot_top"), "Top genesets", value = isolate(input$dot_top) %||% 20, min = 1, max = 100, step = 1, width = "110px"),
        selectInput(ns("dot_color"), "Colour", choices = color_choices,
                    selected = keep_choice(isolate(input$dot_color), color_choices), width = "170px"),
        selectInput(ns("dot_size"), "Dot size", choices = c("Geneset size" = "geneset", "Overlap" = "overlap", "None" = "none"),
                    selected = keep_choice(isolate(input$dot_size), c("geneset", "overlap", "none")), width = "140px"),
        numericInput(ns("dot_abrv"), "Label length", value = isolate(input$dot_abrv) %||% 50, min = 10, step = 5, width = "110px"),
        checkboxInput(ns("dot_key"), "Short signature codes", value = isolate(input$dot_key) %||% TRUE)
      )
    })

    dot_settings <- reactive({
      res <- result()
      req(res)
      val <- keep_choice(input$dot_val, c("fdr", "pval"))
      cutoff <- input$dot_cutoff
      if (is.null(cutoff) || is.na(cutoff)) cutoff <- 1
      color_choices <- c("significance", if (annotate_is_ranked(res)) "score")
      out <- list(
        val = val,
        pval = if (identical(val, "pval")) cutoff else 1,
        fdr = if (identical(val, "fdr")) cutoff else 1,
        top = annotate_number_or(input$dot_top, 20, min = 1),
        color_by = keep_choice(input$dot_color, color_choices),
        size_by = keep_choice(input$dot_size, c("geneset", "overlap", "none")),
        signature_key = input$dot_key %||% TRUE,
        abrv = annotate_number_or(input$dot_abrv, 50, min = 10)
      )
      out
    })

    dot_data <- reactive({
      s <- dot_settings()
      tryCatch(
        SigRepo::hypeRDotData(result(), val = s$val, pval = s$pval, fdr = s$fdr, top = s$top, size_by = s$size_by,
                              signature_key = s$signature_key, abrv = s$abrv),
        error = function(e) NULL
      )
    })

    # plotHypeRDots() draws a blank panel when nothing passes; say what to change.
    output$dot_hint <- renderUI({
      dots <- dot_data()
      req(!is.null(dots), nrow(dots) == 0)
      s <- dot_settings()
      cutoff <- if (identical(s$val, "fdr")) s$fdr else s$pval
      div(class = "alert alert-info annotate-message", sprintf(
        "No geneset has %s ≤ %s in any query. Raise the cutoff (up to 1) to see the strongest genesets anyway.",
        if (identical(s$val, "fdr")) "FDR" else "p-value", format(cutoff)
      ))
    })

    dot_size <- reactive({
      annotate_dot_size(dot_data(), length(annotate_result_hyps(result())))
    })

    draw_dots <- function() {
      do.call(SigRepo::plotHypeRDots, c(list(result()), dot_settings()))
    }

    output$dot_plot <- renderPlot({
      req(result())
      plot <- attempt(draw_dots())
      stop_if_failed(plot, "The dot plot could not be drawn:")
      plot
    }, res = 96, height = function() round(dot_size()$height * 96), width = function() round(dot_size()$width * 96))

    plot_download <- function(stem, device, size, draw) {
      downloadHandler(
        filename = function() paste0(export_name(stem), ".", device),
        content = function(file) {
          s <- size()
          ggplot2::ggsave(file, plot = draw(), device = device, width = s$width, height = s$height, units = "in", dpi = 150)
        }
      )
    }
    output$download_dots_png <- plot_download("dotplot", "png", dot_size, draw_dots)
    output$download_dots_pdf <- plot_download("dotplot", "pdf", dot_size, draw_dots)

    output$signature_key <- DT::renderDT({
      res <- result()
      req(res)
      key <- SigRepo::hypeRSignatureKey(res)
      DT::datatable(key, rownames = FALSE, selection = "none", class = "compact stripe",
                    options = list(dom = "t", paging = FALSE, ordering = FALSE))
    })

    # ---- enrichment ----------------------------------------------------------------

    # The query and geneset last picked from the Results table. The controls
    # are rebuilt from it rather than updated, since they may not have been
    # rendered yet (the tab has never been opened) when a row is picked.
    enrichment_pick <- reactiveVal(NULL)
    observeEvent(run_state(), enrichment_pick(NULL))

    output$enrichment_controls <- renderUI({
      res <- result()
      req(res)
      queries <- names(annotate_result_hyps(res))
      pick <- enrichment_pick()
      query <- keep_choice(pick$query %||% isolate(input$enrichment_query), queries)
      genesets <- annotate_query_genesets(res, query)
      wanted <- if (identical(pick$query, query)) pick$geneset else isolate(input$enrichment_geneset)
      div(
        class = "annotate-controls",
        selectInput(ns("enrichment_query"), "Query", choices = queries, selected = query, width = "440px"),
        selectInput(ns("enrichment_geneset"), "Geneset", choices = genesets, selected = keep_choice(wanted, genesets), width = "440px")
      )
    })

    # A different query lists its own genesets, keeping the picked one for it.
    observeEvent(input$enrichment_query, {
      res <- result()
      req(res)
      genesets <- annotate_query_genesets(res, input$enrichment_query)
      pick <- enrichment_pick()
      wanted <- if (identical(pick$query, input$enrichment_query)) pick$geneset else isolate(input$enrichment_geneset)
      updateSelectInput(session, "enrichment_geneset", choices = genesets, selected = keep_choice(wanted, genesets))
    })

    enrichment_selection <- reactive({
      res <- result()
      req(res, input$enrichment_query, input$enrichment_geneset)
      req(input$enrichment_query %in% names(annotate_result_hyps(res)))
      req(input$enrichment_geneset %in% annotate_query_genesets(res, input$enrichment_query))
      list(query = input$enrichment_query, geneset = input$enrichment_geneset)
    })

    draw_enrichment <- function() {
      sel <- enrichment_selection()
      SigRepo::plotHypeREnrichment(result(), sel$geneset, query = sel$query)
    }

    output$enrichment_plot <- renderPlot({
      plot <- attempt(draw_enrichment())
      stop_if_failed(plot, "The enrichment plot could not be drawn:")
      plot
    }, res = 96)

    output$enrichment_details <- renderUI({
      sel <- enrichment_selection()
      details <- attempt(annotate_enrichment_summary(result(), sel$query, sel$geneset))
      stop_if_failed(details, "The enrichment details could not be computed:")
      tagList(
        tags$table(
          class = "annotate-measures",
          lapply(seq_len(nrow(details$table)), function(i) tags$tr(tags$td(details$table$Measure[i]), tags$td(details$table$Value[i])))
        ),
        strong(sprintf("%s (%d)", details$genes_label, length(details$genes))),
        if (length(details$genes) > 0) div(class = "annotate-genes", lapply(details$genes, tags$span))
      )
    })

    enrichment_size <- function() list(width = 9, height = 6.5)
    output$download_enrichment_png <- plot_download("enrichment", "png", enrichment_size, draw_enrichment)
    output$download_enrichment_pdf <- plot_download("enrichment", "pdf", enrichment_size, draw_enrichment)

    # ---- maps ------------------------------------------------------------------------

    output$map_controls <- renderUI({
      res <- result()
      req(res)
      queries <- names(annotate_result_hyps(res))
      types <- c("Enrichment map" = "emap", if (annotate_uses_rgsets(res)) c("Hierarchy map" = "hmap"))
      div(
        class = "annotate-controls",
        selectInput(ns("map_type"), "Map", choices = types, selected = keep_choice(isolate(input$map_type), types), width = "160px"),
        selectInput(ns("map_query"), "Query", choices = queries, selected = keep_choice(isolate(input$map_query), queries), width = "440px"),
        selectInput(ns("map_val"), "Colour by", choices = c("FDR" = "fdr", "p-value" = "pval"),
                    selected = keep_choice(isolate(input$map_val), c("fdr", "pval")), width = "110px"),
        numericInput(ns("map_cutoff"), "FDR ≤", value = isolate(input$map_cutoff) %||% 0.05, min = 0, max = 1, step = 0.01, width = "90px"),
        numericInput(ns("map_top"), "Top", value = isolate(input$map_top) %||% 25, min = 2, step = 1, width = "80px"),
        conditionalPanel(
          condition = "input.map_type == 'emap'",
          ns = ns,
          div(
            class = "annotate-controls",
            selectInput(ns("map_metric"), "Similarity", choices = c("Jaccard" = "jaccard_similarity", "Overlap" = "overlap_similarity"),
                        selected = keep_choice(isolate(input$map_metric), c("jaccard_similarity", "overlap_similarity")), width = "120px"),
            numericInput(ns("map_similarity"), "Similarity cutoff", value = isolate(input$map_similarity) %||% 0.2,
                         min = 0, max = 1, step = 0.05, width = "130px")
          )
        )
      )
    })

    map_settings <- reactive({
      res <- result()
      req(res)
      queries <- names(annotate_result_hyps(res))
      cutoff <- input$map_cutoff
      list(
        type = keep_choice(input$map_type, c("emap", if (annotate_uses_rgsets(res)) "hmap")),
        query = keep_choice(input$map_query, queries),
        val = keep_choice(input$map_val, c("fdr", "pval")),
        fdr = if (is.null(cutoff) || is.na(cutoff)) 1 else cutoff,
        top = annotate_number_or(input$map_top, 25, min = 2),
        similarity_metric = keep_choice(input$map_metric, c("jaccard_similarity", "overlap_similarity")),
        similarity_cutoff = annotate_number_or(input$map_similarity, 0.2, min = 0)
      )
    })

    # plotHypeRMap() returns NULL with a warning when there is nothing to draw.
    map_outcome <- reactive({
      s <- map_settings()
      compare_run(c(list(result()), s), runner = SigRepo::plotHypeRMap)
    })

    output$map_message <- renderUI({
      m <- map_outcome()
      text <- c(m$error, m$warnings)
      if (length(text) == 0 && is.null(m$result)) text <- "Nothing to draw for these settings."
      if (length(text) == 0) return(NULL)
      div(class = "alert alert-info annotate-message", paste(text, collapse = "\n"))
    })

    output$map <- visNetwork::renderVisNetwork({
      m <- map_outcome()
      req(m$result)
      m$result
    })

    # ---- tables ------------------------------------------------------------------------

    results_table <- reactive({
      res <- result()
      req(res)
      annotate_results_table(res)
    })

    output$results_table <- DT::renderDT({
      df <- results_table()
      dt <- DT::datatable(
        df,
        extensions = "Buttons",
        filter = "top",
        rownames = FALSE,
        selection = "single",
        class = "compact stripe hover nowrap",
        options = list(
          pageLength = 25,
          lengthMenu = c(10, 25, 50, 100, -1),
          scrollX = TRUE,
          dom = "Bfrtip",
          buttons = annotate_export_buttons(export_name("results")),
          columnDefs = list(list(targets = which(names(df) %in% c("hits", "le")) - 1, render = DT::JS(
            "function(data, type) { return type === 'display' && data && data.length > 60 ? data.substr(0, 60) + '…' : data; }"
          )))
        )
      )
      numeric_cols <- names(df)[vapply(df, is.double, logical(1))]
      if (length(numeric_cols) > 0) {
        dt <- DT::formatSignif(dt, columns = numeric_cols, digits = 4)
      }
      dt
    }, server = FALSE)

    observeEvent(input$results_table_rows_selected, {
      df <- isolate(results_table())
      row <- input$results_table_rows_selected
      req(length(row) == 1, row <= nrow(df))
      enrichment_pick(list(query = df$query[row], geneset = df$label[row]))
      updateTabsetPanel(session, "result_tabs", selected = "Enrichment")
    })

    output$provenance_table <- DT::renderDT({
      res <- result()
      req(res)
      DT::datatable(
        annotate_provenance_table(res),
        extensions = "Buttons",
        rownames = FALSE,
        selection = "none",
        class = "compact stripe hover",
        options = list(dom = "Bt", paging = FALSE, scrollX = TRUE, ordering = FALSE,
                       buttons = annotate_export_buttons(export_name("provenance")))
      )
    }, server = FALSE)

    output$hyper_table <- renderUI({
      res <- result()
      req(res)
      table <- attempt(hypeR::rctbl_build(res))
      stop_if_failed(table, "hypeR could not build its table:")
      table
    })

    # ---- R code and downloads ------------------------------------------------------------

    output$r_code <- renderText({
      state <- run_state()
      req(state$args, state$result)
      dots <- isolate(tryCatch(dot_settings(), error = function(e) NULL))
      enrichment <- tryCatch(enrichment_selection(), error = function(e) NULL)
      map <- tryCatch(map_settings(), error = function(e) NULL)
      plots <- list(
        dots = if (!is.null(dots)) annotate_changed_args(
          dots, list(val = "fdr", pval = 1, fdr = 1, top = 20, color_by = "significance", size_by = "geneset", signature_key = TRUE, abrv = 50)
        ),
        enrichment = if (!is.null(enrichment)) list(geneset = enrichment$geneset, query = enrichment$query),
        map = if (!is.null(map)) c(list(query = map$query), annotate_changed_args(
          map[setdiff(names(map), "query")],
          list(type = "emap", val = "fdr", pval = 1, fdr = 1, top = 25, similarity_metric = "jaccard_similarity", similarity_cutoff = 0.2)
        ))
      )
      annotate_r_code(state$args, state$genesets_description, plots = plots)
    })

    output$download_result <- downloadHandler(
      filename = function() paste0(export_name("result"), ".rds"),
      content = function(file) saveRDS(result(), file)
    )

    output$download_excel <- downloadHandler(
      filename = function() paste0(export_name("results"), ".xlsx"),
      content = function(file) SigRepo::hypeRToExcel(result(), file_path = file)
    )

    output$download_genesets <- downloadHandler(
      filename = function() "annotate_genesets.rds",
      content = function(file) saveRDS(run_state()$args$genesets, file)
    )

    output$download_gene_lists_ui <- renderUI({
      req(identical(run_state()$source, "genes"))
      downloadButton(ns("download_gene_lists"), "Gene lists (.rds)")
    })
    output$download_gene_lists <- downloadHandler(
      filename = function() "annotate_gene_lists.rds",
      content = function(file) saveRDS(run_state()$args$signature, file)
    )

    list(
      run_state = run_state,
      preview_state = preview_state,
      genesets_state = genesets_state,
      picks = picks,
      readiness = readiness,
      settings = settings
    )
  })
}


# Load Step 2's genesets for real: the MSigDB cache (or msigdbr), hyperdb, or
# custom files and text.
annotate_default_geneset_loader <- function(request) {
  switch(
    request$source,
    msigdb = annotate_load_msigdb(request$species, request$collection, request$subcollection, clean = request$clean,
                                  cache_dir = annotate_msigdb_cache_dir()),
    rgsets = {
      if (is.null(request$rgsets) || is.na(request$rgsets) || !nzchar(request$rgsets)) {
        stop("Choose a hierarchy to load.", call. = FALSE)
      }
      annotate_load_rgsets(request$rgsets, request$version)
    },
    custom = {
      sets <- list()
      if (!is.null(request$file)) {
        sets <- annotate_parse_custom_geneset_file(request$file$datapath, request$file$name)
      }
      if (nzchar(trimws(request$name %||% "")) || nzchar(trimws(request$genes %||% ""))) {
        sets <- c(sets, annotate_parse_custom_geneset_text(request$name, request$genes))
      }
      if (length(sets) == 0) {
        stop("Upload a GMT or CSV file, or type a geneset.", call. = FALSE)
      }
      annotate_load_custom(sets, clean = request$clean)
    }
  )
}
