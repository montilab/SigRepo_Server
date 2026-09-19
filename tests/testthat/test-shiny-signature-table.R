# The Signature tab's main table, at the module level (issue #95).
#
# The display helpers are unit tested in test-shiny-signature-helpers.R; what
# matters here is that turning on sorting and rewriting the table for display
# did not break the thing row selection depends on -- DT reports positions in
# the source data frame, and the tab indexes signature_db() with them.
load_signature_app()

test_that("the Create Signature button is not offered", {
  # Withdrawn for now: the create path is untested. The modal and its server
  # are deliberately left in place so the button can be put back.
  markup <- as.character(signature_module_ui("signatures"))

  expect_no_match(markup, "Create Signature", fixed = TRUE)
  expect_no_match(markup, "open_create_modal", fixed = TRUE)
})

test_that("Upload Signature is still offered", {
  # Guards against the removal above taking the neighbouring button with it.
  markup <- as.character(signature_module_ui("signatures"))

  expect_match(markup, "Upload Signature", fixed = TRUE)
})

test_that("selecting a row makes that row the active signature", {
  run_signature_module({
    session$setInputs(signature_tbl_rows_selected = 2, signature_tbl_row_last_clicked = 2)

    expect_identical(session$returned$selected_signature()$signature_name, "beta")
  })
})

test_that("the active signature follows the most recently clicked row", {
  # The tab supports selecting several rows for the basket while keeping one of
  # them as the detail view.
  run_signature_module({
    session$setInputs(signature_tbl_rows_selected = c(1, 2), signature_tbl_row_last_clicked = 1)

    expect_identical(session$returned$selected_signature()$signature_name, "alpha")
  })
})

test_that("several rows can be added to the basket at once", {
  run_signature_module({
    session$setInputs(signature_tbl_rows_selected = c(1, 2), signature_tbl_row_last_clicked = 2)
    session$setInputs(add_selected_to_basket_btn = 1)

    expect_identical(nrow(session$returned$basket()), 2L)
  })
})

test_that("the signature id filter is a box you can type an id into", {
  # It was a range slider, because DT picks the widget from the column type and
  # searchSignature() returns signature_id as a numeric.
  run_signature_module({
    cells <- signature_filter_cells(output$signature_tbl)
    id_cell <- cells[[match("signature_id", names(signature_db_rows))]]

    expect_match(id_cell, "type=\"search\"", fixed = TRUE)
    expect_no_match(id_cell, "data-min", fixed = TRUE)
  })
})

test_that("a genuine numeric cutoff still filters by range", {
  # Only identifiers were meant to change; a range is the right question for a
  # quantity.
  run_signature_module({
    cells <- signature_filter_cells(output$signature_tbl)
    cutoff_cell <- cells[[match("adj_p_cutoff", names(signature_db_rows))]]

    expect_match(cutoff_cell, "data-min", fixed = TRUE)
  })
})

test_that("the table is headed by readable labels, not database column names", {
  # Scoped to the header markup: DT also emits the real column names in
  # columnDefs, which is how server-side sorting and searching address them.
  run_signature_module({
    header <- jsonlite::fromJSON(as.character(output$signature_tbl))$x$container

    expect_match(header, "Signature ID", fixed = TRUE)
    expect_match(header, "Adj. p cutoff", fixed = TRUE)
    expect_no_match(header, "adj_p_cutoff", fixed = TRUE)
  })
})

test_that("the table keeps multi-row selection for the basket", {
  run_signature_module({
    selection <- jsonlite::fromJSON(as.character(output$signature_tbl))$x$selection

    expect_identical(selection$mode, "multiple")
  })
})

test_that("sorting is on and no default sort is imposed", {
  # Issue #95. `order` stays empty so rows arrive in the order the server sent
  # them, which is what row selection indexes into.
  run_signature_module({
    options <- jsonlite::fromJSON(as.character(output$signature_tbl))$x$options

    expect_true(options$ordering)
    expect_length(options$order, 0)
    expect_true("colvis" %in% options$buttons)
  })
})

test_that("long free-text columns are hidden by default", {
  run_signature_module({
    defs <- jsonlite::fromJSON(as.character(output$signature_tbl))$x$options$columnDefs
    hidden <- unlist(defs$targets[!is.na(defs$visible) & !defs$visible])

    expect_true((match("description", names(signature_db_rows)) - 1) %in% hidden)
  })
})

test_that("clearing the selection clears the active signature", {
  run_signature_module({
    session$setInputs(signature_tbl_rows_selected = 1, signature_tbl_row_last_clicked = 1)
    session$setInputs(signature_tbl_rows_selected = integer(0))

    expect_null(session$returned$selected_signature())
  })
})
