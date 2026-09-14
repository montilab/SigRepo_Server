# Shared by the Shiny Compare tab's tests (test-shiny-compare-*.R): OmicSignature
# example data, simulated .rds uploads, and a testServer() runner for the
# module. Only defines functions; a test file calls load_compare_app() itself.

# Attach packages in app_src/bootstrap.R's order so the module sees the same
# masking it does in the app -- jsonlite::validate() hiding shiny::validate()
# broke every Annotate output once (c65f9b87) and this tab's heatmap too --
# then source the module and its helpers.
load_compare_app <- function() {
  for (pkg in c("shinyjs", "shiny", "DT", "rmarkdown", "httr", "jsonlite", "dplyr", "ggplot2", "promises", "future")) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
  }
  for (file in c("utils/utils.R", "utils/compare_utils.R", "modules/compare_module.R")) {
    source(testthat::test_path("../../legacy_app", file), local = globalenv())
  }
}

skip_without_two_list_client <- function() {
  testthat::skip_if_not_installed("OmicSignature")
  testthat::skip_if_not("omic_signatures2" %in% names(formals(SigRepo::compareSignatures)),
                        "installed SigRepo predates the two-list compareSignatures()")
}

compare_example_data <- function(name) {
  env <- new.env()
  utils::data(list = name, package = "OmicSignature", envir = env)
  env[[name]]
}

# What fileInput() hands the server for one uploaded file.
compare_upload_of <- function(obj, file_name = "signatures.rds") {
  path <- tempfile(fileext = ".rds")
  saveRDS(obj, path)
  data.frame(name = file_name, size = file.size(path), type = "", datapath = path, stringsAsFactors = FALSE)
}

compare_db_rows <- data.frame(
  signature_id = c(11, 12),
  signature_name = c("alpha", "beta"),
  organism = "Homo sapiens",
  direction_type = "bi-directional",
  assay_type = "transcriptomics",
  phenotype = c("a vs b", "c vs d"),
  has_difexp = c(1L, 0L),
  user_name = "devadmin",
  stringsAsFactors = FALSE
)

# testServer() quotes its expression, so splice the caller's in rather than
# passing a promise it would never see `session`/`output` from.
#
# It runs on a private later event loop. testServer() reads an output by running
# the current loop until it is empty, and the DB-backed test files leave
# pool::dbPool() maintenance tasks rescheduling themselves on the global loop,
# so in a full test_dir() run every output read hung (issue #81).
run_compare_module <- function(expr) {
  later::with_temp_loop(eval(bquote(testServer(
    compare_module_server,
    args = list(signature_db = reactive(compare_db_rows), user_conn_handler = reactive("user-conn")),
    .(substitute(expr))
  )), envir = parent.frame()))
}

# Type a label pairing into the tab's per-signature level inputs.
compare_set_pairing <- function(session, k, pairing) {
  ids <- session$returned$pairing_input_ids(k)
  inputs <- list()
  for (nm in names(pairing)) {
    inputs[[ids[[nm]][["level1"]]]] <- pairing[[nm]][1]
    inputs[[ids[[nm]][["level2"]]]] <- pairing[[nm]][2]
  }
  do.call(session$setInputs, inputs)
}
