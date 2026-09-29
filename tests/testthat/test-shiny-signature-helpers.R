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
    prettify_colnames(c("signature_id", "type", "adj_p_cutoff", "num_of_difexp", "user_name")),
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
  # A factor, so DT offers a dropdown; what it displays is the label.
  shown <- signature_display_frame(signature_rows())
  expect_identical(as.character(shown$visibility), c("Public", "Private", NA_character_))
})

test_that("has_difexp is shown as Yes or No rather than 1 or 0", {
  shown <- signature_display_frame(signature_rows())
  expect_identical(as.character(shown$has_difexp), c("Yes", "No", "Yes"))
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

# ---- controlled vocabulary --------------------------------------------------

# DT picks the filter widget from the column type, so a factor is what turns a
# search box into a dropdown of its levels.

test_that("has_difexp is a dropdown of Yes and No", {
  shown <- signature_display_frame(signature_rows())

  expect_s3_class(shown$has_difexp, "factor")
  expect_identical(levels(shown$has_difexp), c("Yes", "No"))
})

test_that("visibility is a dropdown of Public and Private", {
  shown <- signature_display_frame(signature_rows())

  expect_s3_class(shown$visibility, "factor")
  expect_identical(levels(shown$visibility), c("Public", "Private"))
})

test_that("both flag options are offered even when the data holds only one", {
  # Every signature here is public; "Private" must still be selectable, or the
  # filter cannot express the question.
  one_sided <- data.frame(visibility = c(1L, 1L), has_difexp = c(1L, 1L))
  shown <- signature_display_frame(one_sided)

  expect_identical(levels(shown$visibility), c("Public", "Private"))
  expect_identical(levels(shown$has_difexp), c("Yes", "No"))
})

test_that("controlled vocabulary columns become dropdowns", {
  df <- data.frame(
    organism = c("Homo sapiens", "Mus musculus"),
    type = c("bi-directional", "categorical"),
    assay_type = c("transcriptomics", "proteomics"),
    platform = c("transcriptomics by array", "proteomics by mass spectrometry"),
    sample_type = c("liver", "HSC-3 cell"),
    user_name = c("montilab", "root"),
    stringsAsFactors = FALSE
  )
  shown <- signature_display_frame(df)

  for (column in names(df)) {
    expect_s3_class(shown[[column]], "factor")
  }
})

test_that("a dropdown's options are sorted", {
  df <- data.frame(organism = c("Mus musculus", "Homo sapiens"), stringsAsFactors = FALSE)

  expect_identical(levels(signature_display_frame(df)$organism), c("Homo sapiens", "Mus musculus"))
})

test_that("free text columns keep their search box", {
  # phenotype has 256 distinct values in the repository and description is prose;
  # neither is a vocabulary.
  df <- data.frame(
    phenotype = c("old vs. young", "TAZ KD"),
    description = c("a", "b"),
    signature_name = c("alpha", "beta"),
    stringsAsFactors = FALSE
  )
  shown <- signature_display_frame(df)

  expect_type(shown$phenotype, "character")
  expect_type(shown$description, "character")
  expect_type(shown$signature_name, "character")
})

test_that("a vocabulary column that grows too large falls back to a search box", {
  # A dropdown of hundreds of options is worse than typing. The guard keeps a
  # vocabulary that unexpectedly explodes from producing an unusable filter.
  df <- data.frame(
    sample_type = paste0("tissue_", seq_len(SIGNATURE_VOCABULARY_MAX_LEVELS + 1)),
    stringsAsFactors = FALSE
  )

  expect_type(signature_display_frame(df)$sample_type, "character")
})

test_that("a vocabulary column exactly at the limit is still a dropdown", {
  df <- data.frame(
    sample_type = paste0("tissue_", seq_len(SIGNATURE_VOCABULARY_MAX_LEVELS)),
    stringsAsFactors = FALSE
  )

  expect_s3_class(signature_display_frame(df)$sample_type, "factor")
})

# ---- vocabulary_filter_spec -------------------------------------------------

# A factor alone only gets DT's selectize box, which is still a search box you
# type into. The spec says which columns get a plain <select> instead, and
# carries the options with it: these tables render server side, so the browser
# never sees more than one page of rows and cannot collect the options itself.

test_that("a dropdown column is listed with its position and options", {
  df <- data.frame(
    signature_name = c("alpha", "beta"),
    organism = c("Mus musculus", "Homo sapiens"),
    stringsAsFactors = FALSE
  )

  spec <- vocabulary_filter_spec(signature_display_frame(df))

  expect_length(spec, 1L)
  expect_identical(spec[[1]]$column, 1L)
  expect_identical(spec[[1]]$options, c("Homo sapiens", "Mus musculus"))
})

test_that("columns are numbered from zero, the way DT numbers them", {
  df <- data.frame(organism = "Mus musculus", stringsAsFactors = FALSE)

  expect_identical(vocabulary_filter_spec(signature_display_frame(df))[[1]]$column, 0L)
})

test_that("search box columns are left out of the spec", {
  # Free text and genuine quantities keep the widgets they have: a search box
  # for phenotype, a range slider for adj_p_cutoff.
  df <- data.frame(
    phenotype = c("old vs young", "TAZ KD"),
    adj_p_cutoff = c(0.05, 0.01),
    stringsAsFactors = FALSE
  )

  expect_identical(vocabulary_filter_spec(signature_display_frame(df)), list())
})

test_that("a vocabulary too large for a dropdown is left out too", {
  # signature_display_frame() already fell back to text for it; the spec must
  # not put the dropdown back.
  df <- data.frame(
    sample_type = paste0("tissue_", seq_len(SIGNATURE_VOCABULARY_MAX_LEVELS + 1)),
    stringsAsFactors = FALSE
  )

  expect_identical(vocabulary_filter_spec(signature_display_frame(df)), list())
})

test_that("a dropdown offers every level, not just the values present", {
  # The levels, not the values present, are what the dropdown must offer, or
  # the filter cannot ask for the rows that are missing today.
  all_public <- data.frame(visibility = c(1L, 1L))

  spec <- vocabulary_filter_spec(signature_display_frame(all_public))

  expect_identical(spec[[1]]$options, c("Public", "Private"))
})

test_that("a column with no options at all is left out", {
  # A <select> holding only "All" asks nothing. The flags are exempt: their two
  # options are fixed, so they survive an empty table.
  df <- data.frame(organism = character(0), stringsAsFactors = FALSE)

  expect_identical(vocabulary_filter_spec(signature_display_frame(df)), list())
})

# ---- signature_metadata_frame -----------------------------------------------

test_that("the metadata table shows flag words, not factor codes", {
  # unlist() on a one-row frame turns a factor into its integer code, so making
  # the vocabulary columns factors would have printed visibility as 1.
  meta <- signature_metadata_frame(signature_rows()[1, , drop = FALSE])

  expect_identical(meta$Value[[match("Visibility", meta$Field)]], "Public")
  expect_identical(meta$Value[[match("Organism", meta$Field)]], "Mus musculus")
})

test_that("the metadata table labels its fields readably", {
  meta <- signature_metadata_frame(signature_rows()[1, , drop = FALSE])

  expect_true("Signature ID" %in% meta$Field)
  expect_false("signature_id" %in% meta$Field)
})

test_that("the metadata table reports a missing value as NA, not a code", {
  meta <- signature_metadata_frame(signature_rows()[3, , drop = FALSE])

  expect_true(is.na(meta$Value[[match("Visibility", meta$Field)]]))
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
  cells <- unlist(lapply(signature_table_widget(signature_rows())$x$data, as.character))
  expect_true("Public" %in% cells)
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

# ---- dropdown filters on the rendered table ---------------------------------

# The options travel with the widget rather than being read off the page: these
# tables render server side, so the browser holds one page of rows at a time.

dropdown_options <- function(widget, column) {
  spec <- widget$x$options$vocabularyFilters
  entry <- Filter(function(e) identical(e$column, column), spec)

  if (length(entry) == 0) NULL else entry[[1]]$options
}

test_that("the main table's vocabulary columns carry their options", {
  widget <- signature_table_widget(signature_rows())

  expect_identical(dropdown_options(widget, 2L), "Mus musculus")
  expect_identical(dropdown_options(widget, 7L), c("Public", "Private"))
})

test_that("a dropdown after a hidden column keeps counting every column", {
  # adj_p_cutoff is hidden and sits before visibility. The positions here count
  # all columns; the browser translates each to its visible position, because
  # DataTables drops a hidden column from the DOM. Pre-subtracting them here
  # instead would put every later dropdown on its neighbour's column -- which
  # is what happened, silently, until the translation was added.
  shown <- signature_display_frame(signature_rows())

  expect_true("adj_p_cutoff" %in% SIGNATURE_HIDDEN_COLUMNS)
  expect_identical(match("visibility", names(shown)) - 1L, 7L)
  expect_identical(dropdown_options(signature_table_widget(signature_rows()), 7L),
                   c("Public", "Private"))
})

test_that("the main table's free text and quantity columns carry none", {
  widget <- signature_table_widget(signature_rows())

  expect_null(dropdown_options(widget, 1L))
  expect_null(dropdown_options(widget, 3L))
})

test_that("the main table ships the script that draws the dropdowns", {
  expect_s3_class(signature_table_widget(signature_rows())$x$callback, "JS_EVAL")
})

test_that("a table with no dropdowns is left exactly as it was", {
  # Compare, Collection, Reference and Annotate all come through here; none of
  # them should gain a callback or an options key they did not have before.
  plain <- DatatableFX(data.frame(a = c("x", "y"), stringsAsFactors = FALSE))

  expect_null(plain$x$callback)
  expect_null(plain$x$options$vocabularyFilters)
})

# ---- signature_detail_table_widget ------------------------------------------

# The feature set and difexp tables below the main one. Their columns vary per
# signature, so what they get is decided by type, not by a list of names.

difexp_rows <- function() {
  data.frame(
    feature_name = c("Cd19", "Actb", "Gapdh"),
    direction = c("up", "dn", "up"),
    logFC = c(1.2, -0.4, 0.8),
    stringsAsFactors = FALSE
  )
}

test_that("a difexp vocabulary column gets a dropdown", {
  expect_identical(
    dropdown_options(signature_detail_table_widget(difexp_rows()), 1L),
    c("dn", "up")
  )
})

test_that("a difexp feature column keeps its search box", {
  # feature_name is an identifier, not a vocabulary: there are as many values
  # as there are rows.
  expect_null(dropdown_options(signature_detail_table_widget(difexp_rows()), 0L))
})

test_that("a difexp quantity keeps its range slider", {
  expect_null(dropdown_options(signature_detail_table_widget(difexp_rows()), 2L))
})

test_that("the detail tables show readable headers", {
  header <- as.character(signature_detail_table_widget(difexp_rows())$x$container)

  expect_match(header, "Feature", fixed = TRUE)
  expect_no_match(header, "feature_name", fixed = TRUE)
})

test_that("a detail table leaves a camelCase statistic alone", {
  # logFC and adj.P.Val are how the difexp tables name their columns; the
  # title-case fallback must not turn them into Logfc and Adj.p.val.
  header <- as.character(signature_detail_table_widget(difexp_rows())$x$container)

  expect_match(header, "logFC", fixed = TRUE)
})

test_that("a detail table with no rows still renders", {
  expect_s3_class(signature_detail_table_widget(difexp_rows()[0, ]), "datatables")
})
