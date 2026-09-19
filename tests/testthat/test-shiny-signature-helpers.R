# The Signature tab's helpers, exercised without Shiny or a database.
#
# Two concerns live here. The display helpers decide what the main signature
# table shows -- issue #95 asked for sortable columns, readable headers, and an
# id filter you can type into instead of a slider. The access helpers back the
# Manage Access modal, whose grant path had never reached the database
# (issue #96), so they are injected with the granter rather than calling
# SigRepo::addUserToSignature() for real.
source(testthat::test_path("../../legacy_app/utils/utils.R"), local = FALSE)
source(testthat::test_path("../../legacy_app/utils/signature_utils.R"), local = FALSE)

# The columns and types searchSignature() actually returns, trimmed to what
# these helpers touch. visibility and has_difexp arrive as 1/0 integers and
# signature_id as a double, which is why DT gives them sliders today.
signature_rows <- function() {
  data.frame(
    signature_id = c(275, 9, 1041),
    signature_name = c("alpha", "beta", "gamma"),
    organism = "Mus musculus",
    adj_p_cutoff = c(0.05, 0.01, NA),
    PMID = c(12345678L, NA, 999L),
    year = c(2024L, 2019L, NA),
    has_difexp = c(1L, 0L, 1L),
    visibility = c(1L, 0L, NA),
    signature_hashkey = c("aa", "bb", "cc"),
    stringsAsFactors = FALSE
  )
}

# ---- prettify_colnames ------------------------------------------------------

test_that("known SigRepo columns get their curated labels", {
  expect_identical(
    prettify_colnames(c("signature_id", "direction_type", "adj_p_cutoff", "num_of_difexp", "user_name")),
    c("Signature ID", "Direction", "Adj. p cutoff", "Difexp rows", "Owner")
  )
})

test_that("an unmapped snake_case column falls back to title case", {
  # difexp tables carry per-signature columns we cannot enumerate ahead of time,
  # so the fallback is what keeps their headers readable.
  expect_identical(
    prettify_colnames(c("some_new_column", "t_stat")),
    c("Some New Column", "T Stat")
  )
})

test_that("established camelCase statistics keep their own spelling", {
  # difexp tables carry DESeq2/limma columns. Title-casing turned lfcSE into
  # "LfcSE" and baseMean into "BaseMean", which is wrong, not merely ugly.
  expect_identical(
    prettify_colnames(c("lfcSE", "baseMean", "AveExpr")),
    c("lfcSE", "baseMean", "AveExpr")
  )
})

test_that("id suffixes are capitalised as ID", {
  expect_identical(
    prettify_colnames(c("feature_id", "collection_id", "some_other_id")),
    c("Feature ID", "Collection ID", "Some Other ID")
  )
})

test_that("prettify_colnames preserves length and order", {
  input <- c("year", "not_a_real_column", "PMID")
  expect_length(prettify_colnames(input), length(input))
  expect_identical(prettify_colnames(input)[[3]], "PMID")
})

test_that("prettify_colnames returns an empty vector for no columns", {
  expect_identical(prettify_colnames(character(0)), character(0))
})

# ---- signature_display_frame ------------------------------------------------

test_that("visibility is shown as Public or Private rather than 1 or 0", {
  shown <- signature_display_frame(signature_rows())
  expect_identical(shown$visibility, c("Public", "Private", NA_character_))
})

test_that("has_difexp is shown as Yes or No rather than 1 or 0", {
  shown <- signature_display_frame(signature_rows())
  expect_identical(shown$has_difexp, c("Yes", "No", "Yes"))
})

test_that("identifier columns become character so DT filters them with a text box", {
  # DT picks the filter widget from the column's R type: numeric gives the range
  # slider issue #95 complained about, character gives a search box.
  shown <- signature_display_frame(signature_rows())
  for (column in SIGNATURE_TEXT_FILTER_COLUMNS) {
    expect_type(shown[[column]], "character")
  }
})

test_that("genuine measurements stay numeric so their filter stays a range", {
  shown <- signature_display_frame(signature_rows())
  expect_type(shown$adj_p_cutoff, "double")
})

test_that("the display frame preserves row count and row order", {
  # Row selection indexes signature_db() by position, so any reordering or
  # filtering here would silently load the wrong signature.
  rows <- signature_rows()
  shown <- signature_display_frame(rows)
  expect_identical(nrow(shown), nrow(rows))
  expect_identical(shown$signature_name, rows$signature_name)
})

test_that("the display frame leaves a table without those columns alone", {
  plain <- data.frame(feature_name = c("A2M", "TP53"), score = c(1.5, -2), stringsAsFactors = FALSE)
  expect_identical(signature_display_frame(plain), plain)
})

test_that("an empty table survives the display transform", {
  expect_identical(nrow(signature_display_frame(signature_rows()[0, ])), 0L)
})

# ---- signature_hidden_columns -----------------------------------------------

test_that("noisy columns are hidden by their zero-based position", {
  # DT's columnDefs targets are zero-based; signature_hashkey sits at R index 9.
  expect_true(8 %in% signature_hidden_columns(signature_rows()))
})

test_that("hiding ignores columns the table does not have", {
  plain <- data.frame(feature_name = "A2M", stringsAsFactors = FALSE)
  expect_identical(signature_hidden_columns(plain), integer(0))
})

test_that("the signature name and owner are never hidden", {
  hidden <- signature_hidden_columns(signature_rows())
  expect_false((match("signature_name", names(signature_rows())) - 1) %in% hidden)
})

# ---- grant_signature_access -------------------------------------------------

# Stands in for SigRepo::addUserToSignature(), recording its calls. The real one
# takes a single user and a single access_type per call
# (match.arg(several.ok = FALSE)), so the helper has to loop.
recording_granter <- function(fail_for = character(0)) {
  calls <- list()
  list(
    calls = function() calls,
    fn = function(conn_handler, signature_id, user_name, access_type, ...) {
      calls[[length(calls) + 1]] <<- list(
        signature_id = signature_id, user_name = user_name, access_type = access_type
      )
      if (user_name %in% fail_for) {
        stop(sprintf("user '%s' already has access", user_name))
      }
      invisible(TRUE)
    }
  )
}

test_that("each selected user is granted separately with the chosen access type", {
  granter <- recording_granter()
  grant_signature_access(
    conn_handler = "conn", signature_id = 275,
    user_names = c("ann", "bo"), access_type = "viewer", add_fn = granter$fn
  )

  expect_length(granter$calls(), 2)
  expect_identical(vapply(granter$calls(), `[[`, character(1), "user_name"), c("ann", "bo"))
  expect_identical(granter$calls()[[1]]$access_type, "viewer")
  expect_identical(granter$calls()[[1]]$signature_id, 275)
})

test_that("a successful grant is reported per user", {
  granter <- recording_granter()
  result <- grant_signature_access(
    conn_handler = "conn", signature_id = 275,
    user_names = c("ann", "bo"), access_type = "editor", add_fn = granter$fn
  )

  expect_identical(result$user_name, c("ann", "bo"))
  expect_true(all(result$success))
})

test_that("one user failing does not stop the others and is reported by name", {
  # The modal used to swallow this entirely: no notification, no database write.
  granter <- recording_granter(fail_for = "bo")
  result <- grant_signature_access(
    conn_handler = "conn", signature_id = 275,
    user_names = c("ann", "bo", "cy"), access_type = "viewer", add_fn = granter$fn
  )

  expect_length(granter$calls(), 3)
  expect_identical(result$success, c(TRUE, FALSE, TRUE))
  expect_match(result$message[[2]], "already has access")
})

test_that("granting to nobody is an error rather than a silent no-op", {
  granter <- recording_granter()
  expect_error(
    grant_signature_access(
      conn_handler = "conn", signature_id = 275,
      user_names = character(0), access_type = "viewer", add_fn = granter$fn
    ),
    "user"
  )
  expect_length(granter$calls(), 0)
})

test_that("duplicate selections are granted once", {
  granter <- recording_granter()
  grant_signature_access(
    conn_handler = "conn", signature_id = 275,
    user_names = c("ann", "ann"), access_type = "viewer", add_fn = granter$fn
  )
  expect_length(granter$calls(), 1)
})

# ---- grant_summary ----------------------------------------------------------

test_that("a clean run summarises as a message", {
  summary <- grant_summary(data.frame(
    user_name = c("ann", "bo"), success = c(TRUE, TRUE),
    message = c("", ""), stringsAsFactors = FALSE
  ))
  expect_identical(summary$type, "message")
  expect_match(summary$text, "ann")
})

test_that("a partial failure summarises as a warning naming the failed users", {
  summary <- grant_summary(data.frame(
    user_name = c("ann", "bo"), success = c(TRUE, FALSE),
    message = c("", "already has access"), stringsAsFactors = FALSE
  ))
  expect_identical(summary$type, "warning")
  expect_match(summary$text, "bo")
  expect_no_match(summary$text, "^Added 2")
})

test_that("a total failure summarises as an error", {
  summary <- grant_summary(data.frame(
    user_name = "ann", success = FALSE,
    message = "nope", stringsAsFactors = FALSE
  ))
  expect_identical(summary$type, "error")
})

# ---- DatatableFX ------------------------------------------------------------

# DT appends its own entries to columnDefs, so look for ours rather than
# assuming a position.
has_column_def <- function(widget, target, key, value) {
  any(vapply(widget$x$options$columnDefs, function(def) {
    isTRUE(target %in% def$targets) && identical(def[[key]], value)
  }, logical(1)))
}

test_that("tables can be sorted by clicking a column", {
  # Issue #95: ordering was switched off for every table in the app.
  expect_true(DatatableFX(signature_rows(), hidden_columns = integer(0))$x$options$ordering)
})

test_that("no sort order is imposed before the user picks one", {
  # A regression guard, not a driver: DT already defaults `order` to list().
  # Rows must arrive in the order the server sent them, because row selection
  # indexes signature_db() by position -- so this fails if anyone later sets an
  # explicit default sort.
  expect_identical(
    DatatableFX(signature_rows(), hidden_columns = integer(0))$x$options$order,
    list()
  )
})

test_that("a column visibility control is offered", {
  # Hiding noisy columns by default is only reasonable if they can be brought
  # back without code changes.
  expect_true("colvis" %in% DatatableFX(signature_rows(), hidden_columns = integer(0))$x$options$buttons)
})

test_that("supplied column labels replace the raw database names", {
  widget <- DatatableFX(
    signature_rows(),
    hidden_columns = integer(0),
    column_labels = prettify_colnames(names(signature_rows()))
  )
  header <- as.character(widget$x$container)
  expect_match(header, "Signature ID", fixed = TRUE)
  expect_no_match(header, "signature_hashkey", fixed = TRUE)
})

test_that("raw column names are kept when no labels are supplied", {
  # The other tabs share this helper and should change only by gaining sort.
  header <- as.character(DatatableFX(signature_rows(), hidden_columns = integer(0))$x$container)
  expect_match(header, "signature_hashkey", fixed = TRUE)
})

test_that("text-rendered identifier columns are still sorted numerically", {
  # signature_id renders as text so DT gives it a search box; without this it
  # would sort lexically and put "1041" before "275".
  widget <- DatatableFX(
    signature_display_frame(signature_rows()),
    hidden_columns = integer(0),
    numeric_sort_columns = 0L
  )
  expect_true(has_column_def(widget, 0L, "type", "num"))
})

test_that("an empty table still renders instead of erroring", {
  expect_s3_class(DatatableFX(data.frame(), hidden_columns = integer(0)), "datatables")
})

# ---- signature_table_widget -------------------------------------------------

test_that("the main table shows readable headers instead of database names", {
  header <- as.character(signature_table_widget(signature_rows())$x$container)
  expect_match(header, "Signature ID", fixed = TRUE)
  expect_no_match(header, "adj_p_cutoff", fixed = TRUE)
})

test_that("the main table shows visibility as a word", {
  expect_true(any(unlist(signature_table_widget(signature_rows())$x$data) == "Public"))
})

test_that("the main table hides the noisy columns by default", {
  widget <- signature_table_widget(signature_rows())
  expect_true(has_column_def(widget, 8L, "visible", FALSE))
})

# Multi-row selection is not assertable here -- DT resolves `selection` at
# render time and leaves nothing on the widget object -- so it is covered as
# behaviour in test-shiny-signature-table.R instead.

test_that("the main table sorts the id column numerically despite its text filter", {
  expect_true(has_column_def(signature_table_widget(signature_rows()), 0L, "type", "num"))
})

test_that("the main table copes with no signatures at all", {
  expect_s3_class(signature_table_widget(data.frame()), "datatables")
})
