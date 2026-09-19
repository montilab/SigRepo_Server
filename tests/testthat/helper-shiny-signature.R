# Shared by the Signature tab's module tests (test-shiny-signature-*.R).
#
# Attach packages in app_src/bootstrap.R's order so the module sees the same
# masking it does in the app -- jsonlite::validate() hides shiny::validate() --
# then source the module, its modal, and its helpers.
load_signature_app <- function() {
  for (pkg in c("shinyjs", "shiny", "DT", "rmarkdown", "httr", "jsonlite", "dplyr", "ggplot2", "promises", "future")) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
  }
  for (file in c("utils/utils.R", "utils/signature_utils.R",
                 "modals/manage_users_modal.R", "modules/signature_module.R")) {
    source(testthat::test_path("../../legacy_app", file), local = globalenv())
  }
}

# The rows searchSignature() returns, trimmed to the columns the tab reads.
# adj_p_cutoff is a genuine quantity and description is long free text, so
# between them they cover both filter kinds and the hidden-by-default set.
signature_db_rows <- data.frame(
  signature_id = c(275, 9),
  signature_name = c("alpha", "beta"),
  organism = "Mus musculus",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = c("old vs young", "treated vs control"),
  description = c("a long description", "another long description"),
  adj_p_cutoff = c(0.05, 0.01),
  has_difexp = c(1L, 0L),
  user_name = "devadmin",
  visibility = c(1L, 0L),
  date_created = "2025-10-06 00:44:53",
  stringsAsFactors = FALSE
)

# DT renders one <td> of filter markup per column, in column order.
signature_filter_cells <- function(rendered) {
  html <- jsonlite::fromJSON(as.character(rendered))$x$filterHTML
  cells <- strsplit(html, "<td", fixed = TRUE)[[1]]
  cells[-1]
}

# What searchUser() returns for the modal's dropdown.
signature_user_tbl <- data.frame(
  user_name = c("devadmin", "ann", "bo"),
  stringsAsFactors = FALSE
)

# Stands in for grant_signature_access(), recording its calls so a test can see
# whether the confirm button reached it at all -- which it never did before
# issue #96.
recording_grants <- function(fail_for = character(0)) {
  calls <- list()
  list(
    calls = function() calls,
    fn = function(conn_handler, signature_id, user_names, access_type, ...) {
      calls[[length(calls) + 1]] <<- list(
        signature_id = signature_id, user_names = user_names, access_type = access_type
      )
      data.frame(
        user_name = user_names,
        success = !(user_names %in% fail_for),
        message = ifelse(user_names %in% fail_for, "already has access", ""),
        stringsAsFactors = FALSE
      )
    }
  )
}

# testServer() quotes its expression, so splice the caller's in rather than
# passing a promise it would never see `session`/`output` from.
#
# It runs on a private later event loop, and the DB-backed test files leave
# pool::dbPool() maintenance tasks rescheduling themselves on the global loop,
# so in a full test_dir() run every output read hangs (issue #81).
run_signature_module <- function(expr, grant_fn = NULL) {
  later::with_temp_loop(eval(bquote(testServer(
    signature_module_server,
    args = list(
      signature_db = reactive(signature_db_rows),
      user_conn_handler = reactive("user-conn"),
      signature_trigger = reactiveVal(0),
      search_user_fn = function(...) signature_user_tbl,
      grant_fn = .(grant_fn)
    ),
    .(substitute(expr))
  )), envir = parent.frame()))
}
