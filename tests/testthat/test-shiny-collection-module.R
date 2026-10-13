# The Collections tab's module under testServer() with canned collection rows.
#
# collection_db() is what SigRepo::searchCollection() returns: one row per
# signature in a collection, which the tab groups to one row per collection.
# When the search finds nothing it is a data.frame() with no columns, which
# used to make the grouping error and the tab show "Error: [object Object]"
# (issue #159).
for (pkg in c("shinyjs", "shiny", "DT", "dplyr")) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}
for (file in c("utils/utils.R", "modals/upload_modal.R", "modules/collection_module.R")) {
  source(testthat::test_path("../../legacy_app", file), local = globalenv())
}

collection_db_rows <- data.frame(
  collection_id = c(1, 1, 2),
  collection_name = c("aging", "aging", "cancer"),
  description = c("age signatures", "age signatures", "tumour signatures"),
  user_name = "devadmin",
  date_created = "2025-10-06 00:44:53",
  visibility = c(1L, 1L, 0L),
  signature_id = c(11, 12, 13),
  signature_name = c("alpha", "beta", "gamma"),
  stringsAsFactors = FALSE
)

# testServer() quotes its expression, so splice the caller's in. It runs on a
# private later loop for the reason helper-shiny-signature.R gives.
run_collection_module <- function(expr, rows = collection_db_rows, add_fn = NULL) {
  collection_db <- reactive(rows)
  later::with_temp_loop(eval(bquote(testServer(
    collection_module_server,
    args = list(
      collection_db = .(collection_db),
      user_conn_handler = reactive("user-conn"),
      collection_trigger = reactiveVal(0),
      add_collection_fn = .(add_fn)
    ),
    .(substitute(expr))
  )), envir = parent.frame()))
}

test_that("with no collections the table renders a message rather than an error", {
  run_collection_module(rows = data.frame(), {
    expect_error(output$collection_tbl, NA)
    widget <- jsonlite::fromJSON(as.character(output$collection_tbl))
    # DatatableFX()'s "No data available." table is the one with the bare
    # table layout; a real table carries the filter, info and buttons.
    expect_equal(widget$x$options$dom, "t")
  })
})

test_that("collections are grouped to one row each with their signature count", {
  run_collection_module({
    grouped <- session$returned$grouped()
    expect_equal(nrow(grouped), 2)
    aging <- grouped[grouped$collection_name == "aging", ]
    expect_equal(aging$signature_count, 2L)
    expect_equal(aging$signature_preview, "alpha, beta")
  })
})

test_that("an uploaded collection is added with the visibility chosen in the dialog", {
  adds <- recording_adds()
  upload <- tempfile(fileext = ".rds")
  saveRDS(list(kind = "collection"), upload)

  run_collection_module(add_fn = adds$fn, {
    session$setInputs(
      upload_file = data.frame(name = "c.rds", size = 1, type = "", datapath = upload, stringsAsFactors = FALSE),
      visibility = "TRUE"
    )
    session$setInputs(upload_btn = 1)

    calls <- adds$calls()
    expect_length(calls, 1)
    expect_true(isTRUE(calls[[1]]$visibility))
    expect_equal(calls[[1]]$conn_handler, "user-conn")
    expect_equal(calls[[1]]$omic_collection, list(kind = "collection"))
  })
})
