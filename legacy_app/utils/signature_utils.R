# Helpers for the Signature tab (modules/signature_module.R) and its Manage
# Access modal (modals/manage_users_modal.R). Kept out of the module so they can
# be exercised without Shiny or a database -- see
# tests/testthat/test-shiny-signature-helpers.R.

# Identifiers, not quantities. searchSignature() returns them as numerics, and
# DT picks its filter widget from the column type, so they arrived as range
# sliders (issue #95). Rendering them as text gets a search box instead;
# SIGNATURE_NUMERIC_SORT_COLUMNS then tells DataTables to keep sorting them
# numerically so "9" still comes before "275".
SIGNATURE_TEXT_FILTER_COLUMNS <- c("signature_id", "PMID", "year")

# Shown on demand through the table's column-visibility button rather than by
# default: free text too long for a cell, and cutoffs that are rarely the reason
# someone is scanning the table.
SIGNATURE_HIDDEN_COLUMNS <- c(
  "description",
  "covariates",
  "cutoff_description",
  "others",
  "keywords",
  "signature_hashkey",
  "score_cutoff",
  "logfc_cutoff",
  "p_value_cutoff",
  "adj_p_cutoff"
)

# The access levels SigRepo::addUserToSignature() will accept. Kept in step with
# its match.arg() defaults by a test rather than by memory.
SIGNATURE_ACCESS_TYPES <- c("owner", "editor", "viewer")

# 1/0 columns that read as flags, not numbers. Both labels are always offered
# as filter options, even when the data happens to hold only one of them.
SIGNATURE_FLAG_LABELS <- list(
  visibility = c(yes = "Public", no = "Private"),
  has_difexp = c(yes = "Yes", no = "No")
)

# Columns drawn from a controlled vocabulary. DT picks its filter widget from
# the column type, so rendering these as factors turns their search boxes into
# dropdowns of the available options. Deliberately excluded: phenotype (256
# distinct values in the repository), keywords, description, signature_name and
# cutoff_description -- all free text, where typing beats a list. covariates is
# free text too, despite being short.
SIGNATURE_VOCABULARY_COLUMNS <- c(
  "organism",
  "direction_type",
  "assay_type",
  "platform_name",
  "sample_type",
  "user_name",
  # Signature feature set and difexp columns.
  "direction",
  "group_label",
  "assay",
  "nomenclature_type",
  "match_status",
  "feature_database"
)

# A dropdown of hundreds of options is worse than a search box, so a vocabulary
# that grows past this stays a text filter.
SIGNATURE_VOCABULARY_MAX_LEVELS <- 50L

#' Render a 1/0 database flag as its two labels.
#'
#' @return A character vector as long as `x`; NA stays NA.
label_binary_column <- function(x, yes, no) {
  out <- rep(NA_character_, length(x))
  known <- !is.na(x)
  out[known & x == 1] <- yes
  out[known & x == 0] <- no
  out
}

#' What the main signature table shows, as opposed to what the database returns.
#'
#' Row order and row count are deliberately untouched: row selection indexes
#' signature_db() by position, so reordering here would load the wrong
#' signature.
#'
#' @param df A data.frame from SigRepo::searchSignature().
#'
#' @return `df` with flags and identifier columns rewritten for display.
#' @export
signature_display_frame <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(df)
  }

  for (column in names(SIGNATURE_FLAG_LABELS)) {
    if (column %in% names(df)) {
      labels <- SIGNATURE_FLAG_LABELS[[column]]
      df[[column]] <- factor(
        label_binary_column(df[[column]], labels[["yes"]], labels[["no"]]),
        levels = c(labels[["yes"]], labels[["no"]])
      )
    }
  }

  for (column in SIGNATURE_VOCABULARY_COLUMNS) {
    if (column %in% names(df)) {
      df[[column]] <- as_vocabulary_column(df[[column]])
    }
  }

  for (column in SIGNATURE_TEXT_FILTER_COLUMNS) {
    if (column %in% names(df)) {
      df[[column]] <- as.character(df[[column]])
    }
  }

  df
}

#' Render a controlled-vocabulary column as a factor, so DT filters it with a
#' dropdown of its values rather than a search box.
#'
#' Falls back to text when the vocabulary is too large to pick from.
as_vocabulary_column <- function(x) {
  values <- as.character(x)
  levels <- sort(unique(values[!is.na(values) & nzchar(values)]))

  if (length(levels) > SIGNATURE_VOCABULARY_MAX_LEVELS) {
    return(values)
  }

  factor(values, levels = levels)
}

#' Which columns get a plain dropdown filter, and what it offers.
#'
#' A factor alone only buys DT's selectize widget, which is still a box you
#' type into. This says which columns to replace with a plain <select>, and
#' carries the options along: the Signature tab's tables render server side, so
#' the browser only ever holds one page of rows and cannot collect them itself.
#'
#' @param df A data.frame already through signature_display_frame().
#'
#' @return A list of `list(column = <zero-based position>, options = <character>)`,
#'   one entry per dropdown column, in column order. Empty when there are none.
#' @export
vocabulary_filter_spec <- function(df) {
  spec <- list()

  for (position in seq_along(df)) {
    column <- df[[position]]
    if (!is.factor(column) || length(levels(column)) == 0) next

    spec[[length(spec) + 1L]] <- list(
      # DT numbers columns from zero.
      column = as.integer(position - 1L),
      options = levels(column)
    )
  }

  spec
}

#' The Field/Value table shown for the selected signature.
#'
#' Values are read column by column: unlist() on a one-row frame would turn the
#' vocabulary factors into their integer codes, printing visibility as 1.
#'
#' @param sig A one-row data.frame from SigRepo::searchSignature().
#'
#' @return A data.frame of Field and Value.
#' @export
signature_metadata_frame <- function(sig) {
  shown <- signature_display_frame(sig)

  data.frame(
    Field = prettify_colnames(names(shown)),
    Value = vapply(shown, function(column) as.character(column[[1]]), character(1), USE.NAMES = FALSE),
    stringsAsFactors = FALSE
  )
}

#' Zero-based positions of the columns the signature table hides by default.
#'
#' @param df The data.frame being rendered.
#'
#' @return An integer vector for DT's `columnDefs` targets.
#' @export
signature_hidden_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(integer(0))
  }

  positions <- match(SIGNATURE_HIDDEN_COLUMNS, names(df))
  as.integer(positions[!is.na(positions)] - 1L)
}

#' Zero-based positions of columns rendered as text but sorted as numbers.
#'
#' @param df The data.frame being rendered.
#'
#' @return An integer vector for DT's `columnDefs` targets.
#' @export
signature_numeric_sort_columns <- function(df) {
  if (is.null(df) || !is.data.frame(df) || ncol(df) == 0) {
    return(integer(0))
  }

  positions <- match(SIGNATURE_TEXT_FILTER_COLUMNS, names(df))
  as.integer(positions[!is.na(positions)] - 1L)
}

#' Build the Signature tab's main table.
#'
#' Everything the table does differently from a bare DatatableFX lives here so
#' it can be asserted on without Shiny.
#'
#' @param df A data.frame from SigRepo::searchSignature().
#'
#' @return A DT::datatable object.
#' @export
signature_table_widget <- function(df) {
  shown <- signature_display_frame(df)

  if (is.null(shown) || !is.data.frame(shown) || nrow(shown) == 0) {
    return(DatatableFX(
      shown,
      hidden_columns = integer(0),
      scrollY = "500px",
      row_selection = "multiple",
      escape = FALSE
    ))
  }

  DatatableFX(
    shown,
    hidden_columns = signature_hidden_columns(shown),
    numeric_sort_columns = signature_numeric_sort_columns(shown),
    column_labels = prettify_colnames(names(shown)),
    vocabulary_filters = vocabulary_filter_spec(shown),
    scrollY = "500px",
    row_selection = "multiple",
    escape = FALSE
  )
}

#' Build one of the tables below the main one: the feature set, or difexp.
#'
#' Both carry per-signature columns that cannot be enumerated ahead of time, so
#' the headers come from the fallback in prettify_colnames() and the dropdowns
#' are decided by column type rather than by name.
#'
#' @param df A data.frame from SigRepo::getSignature() or getDifexp().
#'
#' @return A DT::datatable object.
#' @export
signature_detail_table_widget <- function(df) {
  shown <- signature_display_frame(df)

  DatatableFX(
    shown,
    hidden_columns = integer(0),
    column_labels = prettify_colnames(names(shown)),
    vocabulary_filters = vocabulary_filter_spec(shown),
    scrollY = "500px"
  )
}

#' Grant one or more users access to a signature.
#'
#' SigRepo::addUserToSignature() takes a single user and a single access_type
#' per call (match.arg(several.ok = FALSE)), so granting to several users means
#' looping and collecting the outcomes. One user failing -- most often because
#' they already have access -- must not stop the rest.
#'
#' @param conn_handler A SigRepo connection handler.
#' @param signature_id The signature being shared.
#' @param user_names Character vector of user names; duplicates are collapsed.
#' @param access_type One of "owner", "editor", "viewer".
#' @param add_fn The granting function, injected so tests need no database.
#'
#' @return A data.frame of user_name, success, message -- one row per user.
#' @export
grant_signature_access <- function(conn_handler,
                                   signature_id,
                                   user_names,
                                   access_type,
                                   add_fn = SigRepo::addUserToSignature) {
  user_names <- unique(trimws(as.character(user_names)))
  user_names <- user_names[!is.na(user_names) & nzchar(user_names)]

  if (length(user_names) == 0) {
    stop("Select at least one user to grant access to.")
  }

  results <- lapply(user_names, function(user_name) {
    tryCatch({
      add_fn(
        conn_handler = conn_handler,
        signature_id = signature_id,
        user_name = user_name,
        access_type = access_type
      )
      data.frame(user_name = user_name, success = TRUE, message = "", stringsAsFactors = FALSE)
    }, error = function(e) {
      data.frame(
        user_name = user_name,
        success = FALSE,
        message = conditionMessage(e),
        stringsAsFactors = FALSE
      )
    })
  })

  do.call(rbind, results)
}

#' Who currently has access to a signature.
#'
#' There is no exported client function for this, so it reads the
#' `signature_access` table through the exported lookup_table_sql(). Failure is
#' reported as NULL rather than raised: not being able to list existing access
#' should not stop someone granting new access.
#'
#' @param conn_handler A SigRepo connection handler.
#' @param signature_id The signature to look up.
#'
#' @return A data.frame of user_name and access_type, or NULL if unavailable.
#' @export
fetch_signature_access <- function(conn_handler, signature_id) {
  tryCatch({
    conn <- SigRepo::conn_init(conn_handler)
    on.exit(base::suppressWarnings(base::try(DBI::dbDisconnect(conn), silent = TRUE)), add = TRUE)

    access <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signature_access",
      return_var = c("user_name", "access_type"),
      filter_coln_var = "signature_id",
      filter_coln_val = base::list("signature_id" = signature_id),
      check_db_table = FALSE
    )

    if (is.null(access) || !is.data.frame(access)) {
      return(NULL)
    }

    access[order(access$access_type, access$user_name), , drop = FALSE]
  }, error = function(e) NULL)
}

#' Turn grant_signature_access()'s results into one notification.
#'
#' Partial failures used to vanish silently. They are surfaced by name so the
#' user can tell which grants landed.
#'
#' @param results The data.frame from grant_signature_access().
#'
#' @return A list of `type` (for showNotification) and `text`.
#' @export
grant_summary <- function(results) {
  granted <- results$user_name[results$success]
  failed <- results[!results$success, , drop = FALSE]

  if (nrow(failed) == 0) {
    return(list(
      type = "message",
      text = sprintf(
        "Added %d user(s) to the signature: %s.",
        length(granted),
        paste(granted, collapse = ", ")
      )
    ))
  }

  reasons <- paste(
    sprintf("%s (%s)", failed$user_name, trimws(failed$message)),
    collapse = "; "
  )

  if (length(granted) == 0) {
    return(list(type = "error", text = sprintf("Could not add %s.", reasons)))
  }

  list(
    type = "warning",
    text = sprintf(
      "Added %s. Could not add %s.",
      paste(granted, collapse = ", "),
      reasons
    )
  )
}
