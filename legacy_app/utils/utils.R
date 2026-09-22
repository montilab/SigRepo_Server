

# utils/DatatableFX.R

library(DT)

# Raw database column names shown to users as-is until issue #95. Tables built
# from a fixed query (the signature table) could hard-code their headers, but
# difexp tables carry whatever columns the uploader supplied, so anything
# unmapped falls back to title case rather than showing snake_case.
COLUMN_LABELS <- c(
  signature_id = "Signature ID",
  signature_name = "Signature",
  signature_hashkey = "Hash key",
  collection_id = "Collection ID",
  collection_name = "Collection",
  organism = "Organism",
  direction_type = "Direction",
  assay_type = "Assay",
  phenotype = "Phenotype",
  platform_name = "Platform",
  sample_type = "Sample type",
  covariates = "Covariates",
  description = "Description",
  keywords = "Keywords",
  others = "Other metadata",
  score_cutoff = "Score cutoff",
  logfc_cutoff = "logFC cutoff",
  p_value_cutoff = "p-value cutoff",
  adj_p_cutoff = "Adj. p cutoff",
  cutoff_description = "Cutoff description",
  PMID = "PMID",
  year = "Year",
  has_difexp = "Has difexp",
  num_of_difexp = "Difexp rows",
  num_up_regulated = "Up",
  num_down_regulated = "Down",
  user_name = "Owner",
  access_type = "Access",
  date_created = "Created",
  visibility = "Visibility",
  # Signature feature set and difexp columns.
  feature_name = "Feature",
  probe_id = "Probe ID",
  symbol = "Symbol",
  score = "Score",
  direction = "Direction",
  group_label = "Group",
  logfc = "logFC",
  p_value = "p-value",
  adj_p = "Adj. p",
  aveexpr = "Avg. expression",
  feature_id = "Feature ID",
  feature_database = "Feature database",
  nomenclature_type = "Nomenclature",
  match_status = "Match status",
  sig_feature_hashkey = "Feature hash key",
  access_sig_hashkey = "Access hash key"
)

#' Human-readable headers for raw database column names.
#'
#' @param columns Character vector of column names.
#'
#' @return A character vector the same length and order as `columns`.
#' @export
prettify_colnames <- function(columns) {
  if (length(columns) == 0) {
    return(character(0))
  }

  columns <- as.character(columns)
  labels <- unname(COLUMN_LABELS[columns])

  # Anything not in the curated map: split on underscores and title case, so
  # "some_new_column" reads as "Some New Column".
  unmapped <- is.na(labels)
  if (any(unmapped)) {
    labels[unmapped] <- vapply(columns[unmapped], function(column) {
      # A single camelCase word is already a name people know -- lfcSE and
      # baseMean come straight from DESeq2 -- so leave it be rather than
      # rewriting it as "LfcSE".
      if (!grepl("[_.]", column) && grepl("[a-z][A-Z]", column)) {
        return(column)
      }

      words <- strsplit(column, "[_.]+")[[1]]
      words <- words[nzchar(words)]
      if (length(words) == 0) {
        return(column)
      }

      words <- paste0(toupper(substring(words, 1, 1)), substring(words, 2))
      # "feature_id" reads as an identifier, not as the word "Id".
      words[tolower(words) == "id"] <- "ID"
      paste(words, collapse = " ")
    }, character(1), USE.NAMES = FALSE)
  }

  labels
}

#' DatatableFX: A Customizable DT Wrapper
#'
#' @param df A data frame to render in a DataTable.
#' @param hidden_columns Integer vector of 0-based column indices to hide.
#' @param scrollY Vertical scroll height (default: "500px").
#' @param paging Logical, whether to enable pagination (default: FALSE).
#' @param row_selection Selection mode: "none", "single", or "multiple".
#' @param column_labels Character vector of display headers, one per column of
#'   `df`. NULL keeps the raw database column names.
#' @param numeric_sort_columns Integer vector of 0-based indices of columns that
#'   are rendered as text -- so DT filters them with a search box rather than a
#'   range slider -- but should still sort numerically.
#'
#' @return A DT::datatable object.
#' @export
#' The script that turns a column's filter into a plain dropdown.
#'
#' DT builds every filter cell the same way: a visible `<input type="search">`
#' over a hidden `<select>` that selectize takes over when clicked. Even for a
#' factor that leaves a box you type into, so the dropdown has to replace the
#' input rather than be asked for. The options come from
#' `options$vocabularyFilters` rather than off the page because these tables
#' render server side, where the browser holds one page of rows at a time.
#'
#' Selecting writes DT's own factor search value -- a JSON array of the chosen
#' values, read identically by its client-side filter and its server-side
#' doColumnSearch() -- into DT's input and fires the event DT already listens
#' for. Nothing here filters anything; DT still does that.
#'
#' @return A DT::JS callback.
vocabulary_filter_js <- function() {
  # DT wraps this in function(table) { ... }, so it is a body, not a function.
  DT::JS(
    "  var spec = table.settings()[0].oInit.vocabularyFilters;",
    "  if (!spec) return table;",
    "  var cells = $(table.table().header()).find('tr:last td');",
    "  [].concat(spec).forEach(function(entry) {",
    # DataTables drops a hidden column from the DOM, so the filter row holds a
    # cell per VISIBLE column while the spec counts them all. Asking for the
    # visible position keeps each dropdown on its own column; without it every
    # column after a hidden one gets the previous column's options, silently.
    # The cell is then held as an element, so a later colvis toggle moves it
    # along with its column. A column hidden at render has no cell, and is
    # skipped rather than landing on its neighbour.
    "    var visible = table.column(entry.column).index('visible');",
    "    if (visible === null || visible === undefined) return;",
    "    var $td = $(cells[visible]);",
    "    if ($td.length === 0) return;",
    "    var $input = $td.children('div').first().children('input');",
    "    $input.parent().hide();",
    "    var $select = $('<select/>', {",
    "      'class': 'form-control input-sm',",
    "      css: {width: '100%'}",
    "    });",
    "    $select.append($('<option/>', {value: '', text: 'All'}));",
    "    [].concat(entry.options || []).forEach(function(option) {",
    "      $select.append($('<option/>', {value: option, text: option}));",
    "    });",
    "    $select.on('change', function() {",
    "      var value = $select.val();",
    "      $input.val(value === '' ? '' : JSON.stringify([value])).trigger('input');",
    "    });",
    "    $td.prepend($select);",
    "  });",
    "  return table;"
  )
}

DatatableFX <- function(df,
                        hidden_columns = c(0, 6, 7, 8, 11, 14, 15, 16, 19, 24, 25, 26),
                        scrollY = "500px",
                        row_selection = "single",
                        rownames = FALSE,
                        escape = TRUE,
                        column_labels = NULL,
                        numeric_sort_columns = integer(0),
                        vocabulary_filters = NULL) {

  # Check if df is valid
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(DT::datatable(
      data.frame(Message = "No data available."),
      class = "compact stripe hover nowrap",
      options = list(dom = 't'),  # minimal table
      rownames = FALSE
    ))
  }
  
  # Ensure hidden columns are within bounds
  max_index <- ncol(df) - 1  # 0-based indexing
  valid_hidden_columns <- hidden_columns[hidden_columns >= 0 & hidden_columns <= max_index]
  valid_numeric_sort <- numeric_sort_columns[
    numeric_sort_columns >= 0 & numeric_sort_columns <= max_index
  ]

  column_defs <- list(list(targets = valid_hidden_columns, visible = FALSE))

  # Identifier columns are handed to DT as text so its filter row offers a
  # search box instead of a range slider; telling DataTables to treat them as
  # numbers keeps "9" sorting before "275" (issue #95).
  if (length(valid_numeric_sort) > 0) {
    column_defs <- c(
      column_defs,
      list(list(targets = valid_numeric_sort, type = "num"))
    )
  }

  if (is.null(column_labels)) {
    column_labels <- base::colnames(df)
  }

  # A controlled vocabulary reads better as a dropdown than as a box you type
  # into. Only the tables that ask for it are affected: with no spec there is
  # no callback and no extra option, so every other table renders as before.
  # DT tests callback with missing(), so it has to be left out, not set to NULL.
  dropdown_options <- list()
  dropdown_callback <- list()
  if (length(vocabulary_filters) > 0) {
    dropdown_options <- list(vocabularyFilters = vocabulary_filters)
    dropdown_callback <- list(callback = vocabulary_filter_js())
  }

  # Render the datatable
  do.call(DT::datatable, c(dropdown_callback, list(
    data = df,
    extensions = "Buttons",
    filter = "top",
    colnames = column_labels,
    options = c(dropdown_options, list(
      pageLength = 50,
      lengthMenu = c(10,25, 50, 100, 500, -1),
      scrollY = scrollY,
      scrollX = TRUE,
      paging = TRUE,
      # Sorting was switched off for every table in the app (issue #95). `order`
      # stays empty so rows keep the order the server sent them: row selection
      # indexes the source data frame by position.
      ordering = TRUE,
      order = list(),
      fixedHeader = TRUE,
      dom = 'frtipB',
      # colvis lets users bring back columns hidden by default.
      buttons = c('copy', 'csv', 'excel', 'colvis'),
      columnDefs = column_defs
    )),
    class = "compact stripe hover nowrap",
    selection = row_selection,
    rownames = rownames,
    escape = escape
  )))
}

# === Reusable Delete Confirmation Modal ===
# delete_modal_server <- function(id, delete_id, delete_fn, update_trigger) {
#   moduleServer(id, function(input, output, session) {
#     ns <- session$ns
#     selected_id <- reactiveVal(NULL)
#     
#     observeEvent(delete_id(), {
#       req(delete_id())
#       selected_id(delete_id())
#       
#       showModal(modalDialog(
#         title = "Confirm Delete",
#         paste("Are you sure you want to delete ID", delete_id(), "?"),
#         footer = tagList(
#           modalButton("Cancel"),
#           actionButton(ns("confirm_delete"), "Delete", class = "btn-danger")
#         )
#       ))
#     })
#     
#     observeEvent(input$confirm_delete, {
#       req(selected_id())
#       tryCatch({
#         delete_fn(selected_id())
#         showNotification("Deleted successfully.", type = "message")
#         update_trigger(isolate(update_trigger()) + 1)
#       }, error = function(e) {
#         showNotification(paste("Error deleting:", e$message), type = "error")
#       })
#       
#       removeModal()
#     })
#   })
# }

# modals 



