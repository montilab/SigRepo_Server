# Signature page module

signature_module_ui <- function(id) {
  ns <- NS(id)

  div(
    id = ns("signature_page"),
    class = "sr-page",
    div(
      class = "sr-page-inner",

      tags$header(
        class = "page-titlebar",
        div(
          class = "page-titlebar-text",
          tags$h1("Signatures"),
          span(
            class = "page-titlebar-sub",
            "Browse the repository, then open a signature's metadata, features and differential expression."
          )
        ),
        div(
          class = "page-titlebar-actions",
          uiOutput(ns("basket_toggle")),
          # The Create Signature button is withdrawn for now: the create path
          # is untested. Its modal and the observers behind it are left in
          # place below, so restoring the button is all that is needed to
          # bring it back.
          actionButton(
            ns("open_upload_modal"),
            "Upload signature",
            icon = icon("upload"),
            class = "btn-primary"
          )
        )
      ),

      div(
        class = "card",
        uiOutput(ns("signature_actions")),
        p(
          class = "sr-helper",
          "Highlight one or more rows to add them to the basket. The most recently clicked row becomes the active selection, and View loads its full contents on demand."
        ),
        DT::DTOutput(ns("signature_tbl"))
      ),

      div(
        class = "card",
        tags$h3("Selected signature"),
        p(
          class = "sr-helper",
          "Selecting a row updates the active signature. Use View to load the full metadata and data tables below."
        ),
        uiOutput(ns("signature_detail_panel"))
      )
    )
  )
}


# search_user_fn, grant_fn and access_fn are injected so the Manage Access path
# can be tested without a database (tests/testthat/test-shiny-signature-access.R);
# add_signature_fn likewise for the upload path (test-shiny-signature-upload.R).
signature_module_server <- function(id,
                                    signature_db,
                                    user_conn_handler,
                                    signature_trigger,
                                    search_user_fn = SigRepo::searchUser,
                                    grant_fn = grant_signature_access,
                                    access_fn = fetch_signature_access,
                                    add_signature_fn = SigRepo::addSignature) {
  if (is.null(grant_fn)) grant_fn <- grant_signature_access
  if (is.null(search_user_fn)) search_user_fn <- SigRepo::searchUser
  if (is.null(access_fn)) access_fn <- fetch_signature_access
  if (is.null(add_signature_fn)) add_signature_fn <- SigRepo::addSignature

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    selected_sig <- reactiveVal(NULL)
    signature_feature_set <- reactiveVal(NULL)
    signature_difexp <- reactiveVal(NULL)
    access_user_tbl <- reactiveVal(NULL)
    current_access <- reactiveVal(NULL)
    basket_signatures <- reactiveVal(data.frame())
    last_clicked_row <- reactiveVal(NULL)
    create_upload_df <- reactiveVal(NULL)
    create_upload_error <- reactiveVal(NULL)
    create_detected_columns <- reactiveVal(NULL)

    action_dropdown_ui <- function(id_suffix = "primary", items) {
      dropdown_id <- ns(paste0("signature_actions_menu_", id_suffix))

      tags$div(
        class = "btn-group",
        tags$button(
          type = "button",
          class = "btn btn-default dropdown-toggle",
          `data-toggle` = "dropdown",
          `aria-haspopup` = "true",
          `aria-expanded` = "false",
          "More ",
          tags$span(class = "caret")
        ),
        tags$ul(
          class = "dropdown-menu dropdown-menu-right",
          id = dropdown_id,
          lapply(items, function(item) {
            item_class <- item$class
            if (is.null(item_class) || !nzchar(item_class)) {
              item_class <- ""
            }

            tags$li(
              actionLink(
                inputId = ns(item$id),
                label = item$label,
                class = paste("action-button", item_class)
              )
            )
          })
        )
      )
    }

    current_signature_feature_set <- reactive({
      req(signature_feature_set())
      signature_feature_set()
    })

    fetch_selected_signature <- function(sig_id) {
      SigRepo::getSignature(
        conn_handler = user_conn_handler(),
        signature_id = sig_id
      )
    }

    fetch_signature_feature_set <- function(sig_id) {
      feature_set <- SigRepo::getSignatureFeatureSet(
        conn_handler = user_conn_handler(),
        signature_id = sig_id
      )

      if (is.list(feature_set) && length(feature_set) == 1 && is.data.frame(feature_set[[1]])) {
        return(feature_set[[1]])
      }

      feature_set
    }

    empty_string_to_null <- function(x) {
      if (is.null(x)) {
        return(NULL)
      }

      x <- trimws(as.character(x))
      if (!nzchar(x)) {
        return(NULL)
      }

      x
    }

    parse_optional_numeric <- function(x) {
      x <- empty_string_to_null(x)
      if (is.null(x)) {
        return(NULL)
      }

      suppressWarnings(as.numeric(x))
    }

    guess_column <- function(columns, candidates) {
      if (length(columns) == 0) {
        return("")
      }

      normalized_columns <- tolower(gsub("[^a-z0-9]+", "", columns))
      normalized_candidates <- tolower(gsub("[^a-z0-9]+", "", candidates))
      match_idx <- match(normalized_candidates, normalized_columns, nomatch = 0)
      match_idx <- match_idx[match_idx > 0]

      if (length(match_idx) == 0) {
        return("")
      }

      columns[[match_idx[[1]]]]
    }

    normalize_column_name <- function(x) {
      tolower(gsub("[^a-z0-9]+", "", x))
    }

    auto_detect_column_mapping <- function(df, source_type) {
      columns <- names(df)
      normalized_columns <- stats::setNames(columns, normalize_column_name(columns))

      get_detected <- function(candidates) {
        candidate_idx <- match(normalize_column_name(candidates), names(normalized_columns), nomatch = 0)
        candidate_idx <- candidate_idx[candidate_idx > 0]

        if (length(candidate_idx) == 0) {
          return(NULL)
        }

        unname(normalized_columns[[candidate_idx[[1]]]])
      }

      mapping <- list(
        feature_name = get_detected(c(
          "feature_name", "gene", "gene_name", "genes", "symbol", "external_gene_name",
          "hgnc_symbol", "mgi_symbol", "ensembl", "ensembl_gene_id", "entrez", "entrezgene"
        )),
        symbol = get_detected(c(
          "symbol", "gene_symbol", "hgnc_symbol", "mgi_symbol", "gene", "gene_name", "external_gene_name"
        )),
        probe_id = get_detected(c(
          "probe_id", "probe", "entrez_id", "gene_id", "ensembl", "ensembl_gene_id"
        )),
        score = get_detected(c(
          "score", "stat", "t", "t_stat", "tstat", "waldstat", "wald", "zscore", "z",
          "signedlogp", "signed_log_p", "rank_score", "logfc", "log2foldchange", "logfoldchange"
        )),
        logfc = get_detected(c(
          "logfc", "log2foldchange", "logfoldchange", "avg_log2fc", "foldchange", "lfc"
        )),
        adj_p = get_detected(c(
          "adj_p", "padj", "adjp", "adjpval", "adjpvalue", "adjpval", "adjpvalue",
          "adjpvalfdr", "fdr", "qvalue", "adjpvalbh", "adjpvalbonf", "adjpvalholm",
          "adj.P.Val"
        )),
        p_value = get_detected(c(
          "p_value", "pvalue", "pval", "p", "P.Value"
        )),
        group_label = get_detected(c(
          "group_label", "group", "label", "contrast", "comparison", "subset"
        )),
        direction = get_detected(c(
          "direction", "sign", "regulation"
        ))
      )

      replaced_names <- suppressWarnings(OmicSignature::replaceDifexpCol(columns))
      if (length(replaced_names) == length(columns)) {
        for (i in seq_along(columns)) {
          canonical_name <- replaced_names[[i]]
          if (canonical_name %in% names(mapping) && is.null(mapping[[canonical_name]])) {
            mapping[[canonical_name]] <- columns[[i]]
          }
        }
      }

      if (identical(source_type, "signature")) {
        if (is.null(mapping$feature_name) && !is.null(mapping$symbol)) {
          mapping$feature_name <- mapping$symbol
        }
      }

      mapping
    }

    parse_uploaded_csv <- function(file_info) {
      req(file_info)
      utils::read.csv(
        file_info$datapath,
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
    }

    build_metadata_list <- function() {
      keywords_value <- empty_string_to_null(input$create_keywords)
      others_text <- empty_string_to_null(input$create_other_metadata)
      others_list <- NULL

      if (!is.null(others_text)) {
        parsed_pairs <- strsplit(others_text, "\n", fixed = TRUE)[[1]]
        parsed_pairs <- trimws(parsed_pairs)
        parsed_pairs <- parsed_pairs[nzchar(parsed_pairs)]

        if (length(parsed_pairs) > 0) {
          pair_list <- lapply(parsed_pairs, function(line) {
            parts <- strsplit(line, "=", fixed = TRUE)[[1]]
            if (length(parts) < 2) {
              stop("Additional metadata must use one key=value pair per line.")
            }

            key <- trimws(parts[[1]])
            value <- trimws(paste(parts[-1], collapse = "="))

            if (!nzchar(key) || !nzchar(value)) {
              stop("Additional metadata keys and values cannot be blank.")
            }

            stats::setNames(list(value), key)
          })

          others_list <- unlist(pair_list, recursive = FALSE, use.names = TRUE)
        }
      }

      metadata <- list(
        signature_name = empty_string_to_null(input$create_signature_name),
        organism = empty_string_to_null(input$create_organism),
        type = empty_string_to_null(input$create_type),
        assay_type = empty_string_to_null(input$create_assay_type),
        phenotype = empty_string_to_null(input$create_phenotype),
        covariates = empty_string_to_null(input$create_covariates),
        description = empty_string_to_null(input$create_description),
        platform = empty_string_to_null(input$create_platform),
        sample_type = empty_string_to_null(input$create_sample_type),
        logfc_cutoff = parse_optional_numeric(input$create_logfc_cutoff),
        p_value_cutoff = parse_optional_numeric(input$create_p_value_cutoff),
        adj_p_cutoff = parse_optional_numeric(input$create_adj_p_cutoff),
        score_cutoff = parse_optional_numeric(input$create_score_cutoff),
        keywords = if (!is.null(keywords_value)) trimws(strsplit(keywords_value, ",", fixed = TRUE)[[1]]) else NULL,
        cutoff_description = empty_string_to_null(input$create_cutoff_description),
        author = empty_string_to_null(input$create_author),
        PMID = parse_optional_numeric(input$create_pmid),
        year = parse_optional_numeric(input$create_year),
        others = others_list
      )

      required_fields <- c("signature_name", "organism", "type", "assay_type", "phenotype")
      missing_fields <- required_fields[vapply(metadata[required_fields], is.null, logical(1))]

      if (length(missing_fields) > 0) {
        stop(sprintf("Missing required metadata: %s", paste(missing_fields, collapse = ", ")))
      }

      if (!is.null(metadata$keywords)) {
        metadata$keywords <- metadata$keywords[nzchar(metadata$keywords)]
        if (length(metadata$keywords) == 0) {
          metadata$keywords <- NULL
        }
      }

      metadata
    }

    standardize_uploaded_table <- function(df, source_type) {
      col_mapping <- auto_detect_column_mapping(df, source_type = source_type)
      create_detected_columns(col_mapping)

      renamed_df <- data.frame(stringsAsFactors = FALSE)

      for (target_name in names(col_mapping)) {
        source_name <- col_mapping[[target_name]]
        if (!is.null(source_name) && nzchar(source_name) && source_name %in% names(df)) {
          renamed_df[[target_name]] <- df[[source_name]]
        }
      }

      if (!"feature_name" %in% names(renamed_df)) {
        stop("Could not automatically detect a feature column. Expected something like gene, gene_name, symbol, or ensembl.")
      }

      renamed_df$feature_name <- as.character(renamed_df$feature_name)
      renamed_df <- renamed_df[!is.na(renamed_df$feature_name) & nzchar(renamed_df$feature_name), , drop = FALSE]

      if (nrow(renamed_df) == 0) {
        stop("The uploaded file did not contain any valid feature rows after column mapping.")
      }

      if ("symbol" %in% names(renamed_df)) {
        renamed_df$symbol <- as.character(renamed_df$symbol)
      }

      if ("group_label" %in% names(renamed_df)) {
        renamed_df$group_label <- as.character(renamed_df$group_label)
      }

      numeric_cols <- intersect(c("score", "logfc", "adj_p", "p_value"), names(renamed_df))
      for (col_name in numeric_cols) {
        renamed_df[[col_name]] <- suppressWarnings(as.numeric(renamed_df[[col_name]]))
      }

      if (identical(source_type, "difexp") && !"score" %in% names(renamed_df)) {
        stop("Could not automatically detect a score or ranking column. Expected something like stat, t, score, or logFC.")
      }

      if (identical(source_type, "signature")) {
        if (!"score" %in% names(renamed_df) && !"direction" %in% names(renamed_df)) {
          stop("Could not automatically detect score or direction columns for the signature CSV.")
        }

        if (!"score" %in% names(renamed_df) && "direction" %in% names(renamed_df)) {
          direction_values <- trimws(as.character(renamed_df$direction))
          renamed_df$score <- ifelse(direction_values %in% c("-", "down", "Down", "DOWN"), -1, 1)
        }
      }

      renamed_df
    }

    build_signature_from_difexp <- function(difexp_df, metadata) {
      working_df <- difexp_df
      keep_rows <- rep(TRUE, nrow(working_df))

      if (!is.null(metadata$score_cutoff) && "score" %in% names(working_df)) {
        keep_rows <- keep_rows & !is.na(working_df$score) & abs(working_df$score) >= metadata$score_cutoff
      }

      if (!is.null(metadata$adj_p_cutoff) && "adj_p" %in% names(working_df)) {
        keep_rows <- keep_rows & !is.na(working_df$adj_p) & working_df$adj_p <= metadata$adj_p_cutoff
      }

      if (!is.null(metadata$p_value_cutoff) && "p_value" %in% names(working_df)) {
        keep_rows <- keep_rows & !is.na(working_df$p_value) & working_df$p_value <= metadata$p_value_cutoff
      }

      if (!is.null(metadata$logfc_cutoff) && "logfc" %in% names(working_df)) {
        keep_rows <- keep_rows & !is.na(working_df$logfc) & abs(working_df$logfc) >= metadata$logfc_cutoff
      }

      signature_df <- working_df[keep_rows, , drop = FALSE]

      if (nrow(signature_df) == 0) {
        stop("The selected cutoffs produced an empty signature. Relax the thresholds or verify the mapped columns.")
      }

      signature_df$direction <- ifelse(signature_df$score >= 0, "+", "-")
      signature_cols <- intersect(c("probe_id", "feature_name", "symbol", "score", "direction", "group_label"), names(signature_df))
      signature_df[, signature_cols, drop = FALSE]
    }

    build_signature_object_from_modal <- function() {
      metadata <- build_metadata_list()
      source_type <- input$create_source_type
      uploaded_df <- create_upload_df()

      if (is.null(uploaded_df) || !is.data.frame(uploaded_df) || nrow(uploaded_df) == 0) {
        stop("Upload a CSV file before creating the signature.")
      }

      standardized_df <- standardize_uploaded_table(uploaded_df, source_type = source_type)

      if (identical(source_type, "difexp")) {
        difexp_df <- standardized_df
        signature_df <- build_signature_from_difexp(difexp_df, metadata)
      } else {
        signature_df <- standardized_df
        if ("score" %in% names(signature_df) && !"direction" %in% names(signature_df)) {
          signature_df$direction <- ifelse(signature_df$score >= 0, "+", "-")
        }

        signature_cols <- intersect(c("probe_id", "feature_name", "symbol", "score", "direction", "group_label"), names(signature_df))
        signature_df <- signature_df[, signature_cols, drop = FALSE]
        difexp_df <- NULL
      }

      OmicSignature::OmicSignature$new(
        metadata = metadata,
        signature = signature_df,
        difexp = difexp_df
      )
    }

    output$signature_tbl <- renderDT({
      # Readable headers, Public/Private instead of 1/0, a search box rather
      # than a slider on the id, and the noisy columns folded behind the
      # column-visibility button. See signature_table_widget().
      signature_table_widget(signature_db())
    }, server = TRUE)

    observeEvent(input$signature_tbl_row_last_clicked, {
      row <- input$signature_tbl_row_last_clicked
      if (!is.null(row) && length(row) == 1) {
        last_clicked_row(row)
      }
    })

    observeEvent(input$signature_tbl_rows_selected, {
      rows <- input$signature_tbl_rows_selected

      if (length(rows) == 0) {
        selected_sig(NULL)
        signature_feature_set(NULL)
        signature_difexp(NULL)
        last_clicked_row(NULL)
        return()
      }

      detail_row <- last_clicked_row()
      if (is.null(detail_row) || !detail_row %in% rows) {
        detail_row <- rows[[length(rows)]]
        last_clicked_row(detail_row)
      }

      df <- signature_db()
      sig <- df[detail_row, , drop = FALSE]
      selected_sig(sig)
      signature_feature_set(NULL)
      signature_difexp(NULL)
    })

    output$signature_actions <- renderUI({
      sig <- selected_sig()

      if (is.null(sig)) {
        return(
          div(
            class = "sr-selected",
            tags$span(class = "sr-selected-label", "Selection"),
            tags$span(class = "sr-selected-name", "No signature selected")
          )
        )
      }

      dropdown_items <- list(
        list(id = "update_btn", label = "Update Signature", class = ""),
        list(id = "access_btn", label = "Manage Access", class = ""),
        list(id = "delete_btn", label = "Delete Signature", class = "sr-danger")
      )

        div(
          class = "sr-toolbar",
          div(
            class = "sr-selected",
            tags$span(class = "sr-selected-label", "Selected signature"),
            tags$span(class = "sr-selected-name", field_value(sig, "signature_name"))
          ),
          div(
            class = "sr-actions",
            actionButton(ns("view_btn"), "View", class = "btn-primary"),
            downloadButton(ns("download_btn"), "Download"),
            action_dropdown_ui(
              id_suffix = "local",
              items = dropdown_items
            ),
            actionButton(ns("add_to_basket_btn"), "Add to Basket"),
            actionButton(ns("add_selected_to_basket_btn"), "Add Selected Rows")
          )
        )
    })

    output$basket_toggle <- renderUI({
      basket_df <- basket_signatures()

      div(
        actionButton(
          ns("open_basket_btn"),
          label = sprintf(
            "Basket (%s)",
            nrow(basket_df)
          ),
          icon = icon("shopping-basket")
        )
      )
    })

    output$basket_actions <- renderUI({
      div(
        class = "sr-actions",
        actionButton(ns("remove_from_basket_btn"), "Remove Last"),
        actionButton(ns("clear_basket_btn"), "Clear Basket"),
        downloadButton(ns("download_basket_btn"), "Download Basket")
      )
    })

    output$basket_table <- DT::renderDataTable({
      basket_df <- basket_signatures()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        return(DatatableFX(data.frame(), hidden_columns = integer(0), row_selection = "none"))
      }

      display_columns <- intersect(
        c(
          "signature_name",
          "user_name",
          "visibility",
          "organism",
          "phenotype",
          "type",
          "date_created"
        ),
        names(basket_df)
      )

      # Headers come from the shared labeller rather than a hand-kept list: the
      # list above is an intersect(), so a missing column used to leave the
      # rename assigning seven names to fewer columns.
      basket_display <- signature_display_frame(basket_df[, display_columns, drop = FALSE])

      DatatableFX(
        basket_display,
        hidden_columns = integer(0),
        scrollY = "320px",
        row_selection = "none",
        column_labels = prettify_colnames(names(basket_display))
      )
    }, server = TRUE)

    output$signature_detail_panel <- renderUI({
      sig <- selected_sig()

      if (is.null(sig)) {
        return(
          div(
            class = "sr-empty",
            "Choose a signature from the table above to inspect it."
          )
        )
      }

      if (is.null(signature_feature_set())) {
        return(
          tagList(
            div(
              class = "sr-summary",
              div(
                class = "sr-summary-item",
                tags$strong("Signature"),
                tags$span(field_value(sig, "signature_name"))
              ),
              div(
                class = "sr-summary-item",
                tags$strong("Owner"),
                tags$span(field_value(sig, "user_name"))
              ),
              div(
                class = "sr-summary-item",
                tags$strong("Visibility"),
                tags$span(field_value(sig, "visibility"))
              ),
              div(
                class = "sr-summary-item",
                tags$strong("Created"),
                tags$span(field_value(sig, "date_created"))
              )
            ),
            div(
              class = "sr-empty",
              "The selected signature has not been loaded yet. Click View to fetch its signature feature set."
            )
          )
        )
      }

      tagList(
        div(
          class = "sr-summary",
          div(
            class = "sr-summary-item",
            tags$strong("Signature"),
            tags$span(field_value(sig, "signature_name"))
          ),
          div(
            class = "sr-summary-item",
            tags$strong("Owner"),
            tags$span(field_value(sig, "user_name"))
          ),
          div(
            class = "sr-summary-item",
            tags$strong("Visibility"),
            tags$span(field_value(sig, "visibility"))
          ),
          div(
            class = "sr-summary-item",
            tags$strong("Created"),
            tags$span(field_value(sig, "date_created"))
          )
        ),
        tabsetPanel(
          tabPanel(
            "Metadata",
            div(class = "sr-table", DT::DTOutput(session$ns("signature_metadata_table")))
          ),
          tabPanel("Signature", DT::DTOutput(session$ns("signature_file_table"))),
          tabPanel(
            "Differential Expression",
            div(
              class = "sr-helper",
              "Differential expression is loaded separately to keep the signature view responsive."
            ),
            div(
              class = "sr-actions",
              actionButton(ns("load_difexp_btn"), "Get Difexp")
            ),
            uiOutput(ns("difexp_panel"))
          )
        )
      )
    })

    output$signature_metadata_table <- DT::renderDataTable({
      req(selected_sig())

      df <- signature_metadata_frame(selected_sig())

      DatatableFX(
        df,
        hidden_columns = integer(0),
        scrollY = "360px"
      )
    }, server = TRUE)

    output$signature_file_table <- DT::renderDataTable({
      req(current_signature_feature_set())

      signature_detail_table_widget(current_signature_feature_set())
    }, server = TRUE)

    output$difexp_panel <- renderUI({
      if (is.null(signature_difexp())) {
        return(
          div(
            class = "sr-empty",
            "Differential expression has not been loaded yet."
          )
        )
      }

      DT::DTOutput(session$ns("difexp_file_table"))
    })

    output$difexp_file_table <- DT::renderDataTable({
      req(signature_difexp())

      signature_detail_table_widget(signature_difexp())
    }, server = TRUE)

    observeEvent(input$view_btn, {
      req(selected_sig())

      tryCatch({
        signature_feature_set(fetch_signature_feature_set(selected_sig()$signature_id[[1]]))
        signature_difexp(NULL)
        showNotification("Signature feature set loaded.", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Failed to load signature feature set:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$load_difexp_btn, {
      req(selected_sig(), signature_feature_set())

      tryCatch({
        sig_obj <- fetch_selected_signature(selected_sig()$signature_id[[1]])

        if (is.list(sig_obj) && length(sig_obj) >= 1 && !is.null(sig_obj[[1]]$difexp)) {
          signature_difexp(sig_obj[[1]]$difexp)
        } else if (!is.null(sig_obj$difexp)) {
          signature_difexp(sig_obj$difexp)
        } else {
          stop("No differential expression table was returned for this signature.")
        }

        showNotification("Differential expression loaded.", type = "message")
      }, error = function(e) {
        showNotification(
          paste("Failed to load differential expression:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$add_to_basket_btn, {
      req(selected_sig())

      basket_df <- basket_signatures()
      sig <- selected_sig()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        basket_signatures(sig)
        showNotification("Signature added to basket.", type = "message")
        return()
      }

      if (sig$signature_id[[1]] %in% basket_df$signature_id) {
        showNotification("That signature is already in the basket.", type = "warning")
        return()
      }

      basket_signatures(rbind(basket_df, sig))
      showNotification("Signature added to basket.", type = "message")
    })

    observeEvent(input$add_selected_to_basket_btn, {
      selected_rows <- input$signature_tbl_rows_selected
      df <- signature_db()

      if (length(selected_rows) == 0) {
        showNotification("Highlight one or more signature rows first.", type = "warning")
        return()
      }

      selected_df <- df[selected_rows, , drop = FALSE]

      basket_df <- basket_signatures()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        basket_signatures(selected_df)
        showNotification(sprintf("%s signature(s) added to basket.", nrow(selected_df)), type = "message")
        return()
      }

      new_rows <- selected_df[!selected_df$signature_id %in% basket_df$signature_id, , drop = FALSE]

      if (nrow(new_rows) == 0) {
        showNotification("All highlighted signatures are already in the basket.", type = "warning")
        return()
      }

      basket_signatures(rbind(basket_df, new_rows))
      showNotification(sprintf("%s signature(s) added to basket.", nrow(new_rows)), type = "message")
    })

    observeEvent(input$open_basket_btn, {
      showModal(
        modalDialog(
          title = "Download Basket",
          size = "l",
          easyClose = TRUE,
          footer = modalButton("Close"),
          p(
            class = "sr-helper",
            "Review the current basket, remove items if needed, or download everything together as a zip archive."
          ),
          uiOutput(ns("basket_actions")),
          DT::DTOutput(ns("basket_table"))
        )
      )
    })

    observeEvent(input$remove_from_basket_btn, {
      basket_df <- basket_signatures()

      if (is.null(basket_df) || nrow(basket_df) == 0) {
        showNotification("The basket is already empty.", type = "warning")
        return()
      }

      basket_signatures(basket_df[-nrow(basket_df), , drop = FALSE])
      showNotification("Removed the most recent signature from the basket.", type = "message")
    })

    observeEvent(input$clear_basket_btn, {
      basket_signatures(data.frame())
      showNotification("Basket cleared.", type = "message")
    })

    observeEvent(input$open_upload_modal, {
      showModal(upload_modal_ui(session$ns, type = "Signature"))
    })

    output$create_column_mapping <- renderUI({
      detected <- create_detected_columns()

      if (is.null(detected)) {
        return(
          div(
            class = "sr-empty",
            "Upload a CSV file to let the app detect the relevant columns automatically."
          )
        )
      }

      detected_df <- data.frame(
        SigRepo_Field = names(detected),
        Uploaded_Column = vapply(detected, function(x) if (is.null(x) || !nzchar(x)) "Not detected" else x, character(1)),
        stringsAsFactors = FALSE
      )

      tagList(
        p(
          class = "sr-helper",
          "Detected the most likely columns from the uploaded file. Common DESeq2, limma, and generic differential expression outputs should work without manual mapping."
        ),
        DT::DTOutput(ns("create_detected_columns_table"))
      )
    })

    output$create_detected_columns_table <- DT::renderDataTable({
      detected <- create_detected_columns()
      req(detected)

      detected_df <- data.frame(
        SigRepo_Field = names(detected),
        Uploaded_Column = vapply(detected, function(x) if (is.null(x) || !nzchar(x)) "Not detected" else x, character(1)),
        stringsAsFactors = FALSE
      )

      DatatableFX(
        detected_df,
        hidden_columns = integer(0),
        scrollY = "220px"
      )
    }, server = TRUE)

    output$create_upload_preview <- DT::renderDataTable({
      df <- create_upload_df()
      req(df)

      preview_df <- utils::head(df, 10)
      DatatableFX(
        preview_df,
        hidden_columns = integer(0),
        scrollY = "220px"
      )
    }, server = TRUE)

    observeEvent(input$create_upload_file, {
      create_upload_error(NULL)

      tryCatch({
        uploaded_df <- parse_uploaded_csv(input$create_upload_file)
        create_upload_df(uploaded_df)
        source_type <- input$create_source_type
        if (is.null(source_type) || !nzchar(source_type)) {
          source_type <- "difexp"
        }
        create_detected_columns(auto_detect_column_mapping(uploaded_df, source_type = source_type))
      }, error = function(e) {
        create_upload_df(NULL)
        create_detected_columns(NULL)
        create_upload_error(e$message)
      })
    })

    observeEvent(input$create_source_type, {
      df <- create_upload_df()
      if (is.null(df)) {
        return()
      }

      source_type <- input$create_source_type
      if (is.null(source_type) || !nzchar(source_type)) {
        source_type <- "difexp"
      }

      create_detected_columns(auto_detect_column_mapping(df, source_type = source_type))
    }, ignoreInit = TRUE)

    observeEvent(input$open_create_modal, {
      create_upload_df(NULL)
      create_upload_error(NULL)
      create_detected_columns(NULL)

      showModal(
        modalDialog(
          title = "Create Signature",
          size = "l",
          easyClose = TRUE,
          div(
            class = "sr-helper",
            "Enter the signature metadata, upload a CSV, map the input columns, and the app will generate an OmicSignature object for you."
          ),
          div(
            class = "sr-form-grid",
            textInput(ns("create_signature_name"), "Signature Name"),
            selectizeInput(
              ns("create_organism"),
              "Organism",
              choices = c("Homo sapiens", "Mus musculus"),
              selected = "Homo sapiens",
              options = list(create = TRUE)
            ),
            selectizeInput(
              ns("create_type"),
              "Direction",
              choices = c("bi-directional", "up", "down"),
              selected = "bi-directional",
              options = list(create = TRUE)
            ),
            selectizeInput(
              ns("create_assay_type"),
              "Assay Type",
              choices = c("transcriptomics", "proteomics", "metabolomics", "epigenomics"),
              selected = "transcriptomics",
              options = list(create = TRUE)
            ),
            textInput(ns("create_phenotype"), "Phenotype"),
            textInput(ns("create_sample_type"), "Sample Type"),
            textInput(ns("create_platform"), "Platform"),
            textInput(ns("create_covariates"), "Covariates"),
            textAreaInput(ns("create_description"), "Description", rows = 3),
            textInput(ns("create_keywords"), "Keywords", placeholder = "comma,separated,keywords"),
            textInput(ns("create_author"), "Author"),
            textInput(ns("create_pmid"), "PMID"),
            textInput(ns("create_year"), "Year"),
            textInput(ns("create_cutoff_description"), "Cutoff Description"),
            radioButtons(
              ns("create_visibility"),
              "Visibility",
              choices = c("Private" = FALSE, "Public" = TRUE),
              selected = FALSE,
              inline = TRUE
            )
          ),
          div(
            class = "sr-section",
            tags$h4("Input File"),
            p(
              class = "sr-helper",
              "Choose whether you are uploading a differential expression table that should be converted into a signature, or a prebuilt signature CSV."
            ),
            radioButtons(
              ns("create_source_type"),
              "CSV Type",
              choices = c("Differential Expression CSV" = "difexp", "Signature CSV" = "signature"),
              selected = "difexp",
              inline = TRUE
            ),
            fileInput(
              ns("create_upload_file"),
              "Choose CSV File",
              accept = c(".csv", "text/csv", "text/comma-separated-values,text/plain")
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'difexp'", ns("create_source_type")),
              ns = ns,
              div(
                class = "sr-form-grid",
                textInput(ns("create_score_cutoff"), "Score Cutoff", value = "0"),
                textInput(ns("create_adj_p_cutoff"), "Adjusted P Cutoff", value = "0.05"),
                textInput(ns("create_p_value_cutoff"), "P Value Cutoff"),
                textInput(ns("create_logfc_cutoff"), "LogFC Cutoff"),
                textAreaInput(
                  ns("create_other_metadata"),
                  "Additional Metadata",
                  rows = 3,
                  placeholder = "one key=value pair per line"
                )
              )
            ),
            conditionalPanel(
              condition = sprintf("input['%s'] === 'signature'", ns("create_source_type")),
              ns = ns,
              textAreaInput(
                ns("create_other_metadata"),
                "Additional Metadata",
                rows = 3,
                placeholder = "one key=value pair per line"
              )
            )
          ),
          div(
            class = "sr-section",
            tags$h4("Column Mapping"),
            uiOutput(ns("create_column_mapping"))
          ),
          div(
            class = "sr-section",
            tags$h4("File Preview"),
            uiOutput(ns("create_upload_error_ui")),
            DT::DTOutput(ns("create_upload_preview"))
          ),
          footer = tagList(
            modalButton("Cancel"),
            actionButton(ns("create_signature_btn"), "Create and Save Signature", class = "btn-primary")
          )
        )
      )
    })

    output$create_upload_error_ui <- renderUI({
      err <- create_upload_error()
      if (is.null(err)) {
        return(NULL)
      }

      div(class = "sr-empty", paste("Could not read uploaded CSV:", err))
    })

    observeEvent(input$create_signature_btn, {
      tryCatch({
        omic_signature <- build_signature_object_from_modal()
        visibility <- as.logical(input$create_visibility)
        created_signature_name <- omic_signature$metadata$signature_name

        SigRepo::addSignature(
          conn_handler = user_conn_handler(),
          omic_signature = omic_signature,
          visibility = visibility
        )

        showNotification(
          sprintf("Signature '%s' created and uploaded successfully.", created_signature_name),
          type = "message"
        )
        signature_trigger(isolate(signature_trigger()) + 1)
        removeModal()
      }, error = function(e) {
        showNotification(
          paste("Failed to create signature:", e$message),
          type = "error",
          duration = 10
        )
      })
    })

    observeEvent(input$upload_btn, {
      req(input$upload_file)

      tryCatch({
        rds_object <- readRDS(input$upload_file$datapath)

        # The dialog's Private/Public choice arrives as "FALSE"/"TRUE".
        add_signature_fn(
          conn_handler = user_conn_handler(),
          omic_signature = rds_object,
          visibility = isTRUE(as.logical(input$visibility))
        )

        showNotification("Signature uploaded and added successfully!", type = "message")
        signature_trigger(isolate(signature_trigger()) + 1)
      }, error = function(e) {
        showNotification(
          paste("Error reading or uploading signature rds object:", e$message),
          type = "error"
        )
      })

      removeModal()
    })

    observeEvent(input$delete_btn, {
      req(selected_sig())

      showModal(
        delete_modal_ui(
          session$ns,
          type = "Signature",
          name = selected_sig()$signature_name[[1]]
        )
      )
    })

    observeEvent(input$confirm_delete_signature, {
      req(selected_sig())

      sig_id <- selected_sig()$signature_id[[1]]

      tryCatch({
        SigRepo::deleteSignature(
          conn_handler = user_conn_handler(),
          signature_id = sig_id
        )

        showNotification("Signature deleted successfully.", type = "message")
        removeModal()
        selected_sig(NULL)
        signature_feature_set(NULL)
        signature_difexp(NULL)
        signature_trigger(signature_trigger() + 1)
      }, error = function(e) {
        message("Error deleting signature: ", e$message)
        showNotification(
          paste("Failed to delete signature:", e$message),
          type = "error",
          duration = 8
        )
      })
    })

    observeEvent(input$update_btn, {
      req(selected_sig())

      sig_name <- selected_sig()$signature_name[[1]]

      showModal(modalDialog(
        title = "Update signature",
        paste("Signature to update:", sig_name),
        fileInput(session$ns("update_file_upload"), "Choose an RDS file", accept = ".rds"),
        p("The selected signature will be updated with the new signature object you upload."),
        footer = tagList(
          modalButton("Cancel")
        )
      ))
    })

    selected_signature_id <- reactive({
      sig <- selected_sig()
      if (is.null(sig) || !"signature_id" %in% names(sig)) {
        return(NULL)
      }
      sig$signature_id[[1]]
    })

    # Registered once, when the module starts. It used to be registered inside
    # the confirm handler, which stacked a new copy of its observers per click.
    manage_users_modal_server(
      input = input,
      output = output,
      session = session,
      entity_id = selected_signature_id,
      entity_name = reactive(field_value(selected_sig(), "signature_name")),
      user_conn_handler = user_conn_handler,
      grant_fn = grant_fn,
      current_access = current_access
    )

    observeEvent(input$access_btn, {
      req(selected_sig())

      user_tbl <- tryCatch(
        search_user_fn(conn_handler = user_conn_handler()),
        error = function(e) {
          showNotification(paste("Could not load the user list:", e$message), type = "error")
          NULL
        }
      )
      req(user_tbl)

      access_user_tbl(user_tbl)
      current_access(access_fn(user_conn_handler(), selected_signature_id()))

      showModal(
        manage_users_modal_ui(
          session$ns,
          entity_label = "signature",
          entity_name = field_value(selected_sig(), "signature_name"),
          user_tbl = user_tbl
        )
      )
    })

    output$download_btn <- downloadHandler(
      filename = function() {
        req(selected_sig())
        paste0("signature_", selected_sig()$signature_name[[1]], ".rds")
      },
      content = function(file) {
        req(selected_sig())

        sig_download <- fetch_selected_signature(selected_sig()$signature_id[[1]])
        saveRDS(sig_download, file)
      }
    )

    output$download_basket_btn <- downloadHandler(
      filename = function() {
        paste0("signature_basket_", format(Sys.Date(), "%Y%m%d"), ".zip")
      },
      content = function(file) {
        basket_df <- basket_signatures()
        req(!is.null(basket_df), nrow(basket_df) > 0)

        export_dir <- file.path(tempdir(), paste0("signature_basket_", as.integer(Sys.time())))
        dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

        exported_files <- character(0)

        for (i in seq_len(nrow(basket_df))) {
          sig_id <- basket_df$signature_id[[i]]
          sig_name <- basket_df$signature_name[[i]]
          sig_download <- fetch_selected_signature(sig_id)

          safe_name <- gsub("[^A-Za-z0-9_-]", "_", sig_name)
          out_file <- file.path(export_dir, paste0("signature_", safe_name, ".rds"))
          saveRDS(sig_download, out_file)
          exported_files <- c(exported_files, out_file)
        }

        utils::zip(zipfile = file, files = exported_files, flags = "-j")
      }
    )

    # Exposed so the tab's state can be asserted on in testServer(); the app
    # itself does not read these.
    list(
      selected_signature = selected_sig,
      basket = basket_signatures
    )
  })
}
