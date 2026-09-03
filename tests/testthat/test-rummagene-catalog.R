# The Rummagene catalog: build-time gate, storage, and query.
# See specs/2026-08-31-rummagene-catalog-design.md.
source(testthat::test_path("../../api/lib/rummagene.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)

test_that("the rummagene_catalog schema file declares every column the build job writes", {
  # Guards against the schema drifting from the writer, which is exactly how
  # metabolite_reference and signatures.assay_type went wrong before.
  sql <- base::paste(base::readLines(
    testthat::test_path("../../mysql/schema/rummagene_catalog.sql")
  ), collapse = "\n")

  for (col in c("term", "pmcid", "pmid", "title", "year", "doi", "description",
                "organism", "assay_type", "mesh_evidence", "n_genes",
                "gene_symbols", "feature_names", "gmt_version", "built_at",
                "term_hashkey")) {
    # Match backtick-quoted column name followed by whitespace and a type keyword.
    # This prevents false positives from comments that mention the column name.
    pattern <- base::sprintf("`%s`\\s+[A-Z]", col)
    expect_match(sql, pattern,
                 info = base::sprintf("column %s missing from schema", col))
  }
  # term must outgrow signature_name's 255, and uniqueness must be on the
  # hashkey -- a 512-char unique index exceeds InnoDB's key length under utf8.
  expect_match(sql, "`term` VARCHAR(512)", fixed = TRUE)
  expect_match(sql, "UNIQUE (`term_hashkey`)", fixed = TRUE)
})

test_that("database_admin.R creates the rummagene_catalog table", {
  admin <- base::paste(base::readLines(
    testthat::test_path("../../api/lib/database_admin.R")
  ), collapse = "\n")
  expect_match(admin, "rummagene_catalog.sql", fixed = TRUE)
})

test_that("rummagene_parse_gmt_line splits term, description and genes", {
  line <- "PMC7202592-Table_1.xlsx-liver\tDEGs FDR<0.05\tTP53\tMYC\tEGFR"
  out <- rummagene_parse_gmt_line(line)

  expect_equal(out$term, "PMC7202592-Table_1.xlsx-liver")
  expect_equal(out$description, "DEGs FDR<0.05")
  expect_equal(out$genes, c("TP53", "MYC", "EGFR"))
  expect_equal(out$pmcid, "PMC7202592")
})

test_that("rummagene_parse_gmt_line tolerates an empty description", {
  out <- rummagene_parse_gmt_line("PMC1-t.xlsx-x\t\tTP53\tMYC")
  expect_equal(out$description, "")
  expect_equal(out$genes, c("TP53", "MYC"))
})

test_that("rummagene_parse_gmt_line drops blank and duplicate gene fields", {
  # Real GMT lines carry trailing tabs and repeat symbols.
  out <- rummagene_parse_gmt_line("PMC1-t.xlsx-x\tdesc\tTP53\t\tMYC\tTP53\t")
  expect_equal(out$genes, c("TP53", "MYC"))
})

test_that("rummagene_parse_gmt_line returns NULL for a line with no genes", {
  expect_null(rummagene_parse_gmt_line("PMC1-t.xlsx-x\tdesc"))
  expect_null(rummagene_parse_gmt_line("PMC1-t.xlsx-x\tdesc\t\t"))
})

test_that("rummagene_parse_gmt_line returns NULL when the term carries no PMC id", {
  # Without a PMC id there is no way to reach MeSH, so the set can never
  # qualify. Dropping it at parse time avoids carrying it through the pipeline.
  expect_null(rummagene_parse_gmt_line("some-other-source-table\tdesc\tTP53\tMYC"))
})

test_that("rummagene_parse_gmt_line returns NULL for a blank line", {
  expect_null(rummagene_parse_gmt_line(""))
  expect_null(rummagene_parse_gmt_line("   "))
})

test_that("rummagene_map_symbols returns lowercased Ensembl ids for human symbols", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  out <- rummagene_map_symbols(c("TP53", "MYC"), "Homo sapiens")
  # feature_name in transcriptomics_features is stored lowercased
  # (updateTranscriptomicsFeatureSet does trimws(tolower(ensembl_gene_id))),
  # so the mapping must match that form or nothing will ever resolve.
  expect_equal(out[["TP53"]], "ensg00000141510")
  expect_equal(out[["MYC"]], "ensg00000136997")
})

test_that("rummagene_map_symbols returns NA for a symbol with no Ensembl id", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  # A retired alias. Measured 2026-08-31: LGTN, TRA, SOGA1 and CCDC153 are the
  # symbols that fail across the sampled corpus.
  out <- rummagene_map_symbols(c("TP53", "LGTN"), "Homo sapiens")
  expect_false(base::is.na(out[["TP53"]]))
  expect_true(base::is.na(out[["LGTN"]]))
})

test_that("rummagene_map_symbols returns all NA, not an error, when every symbol is invalid", {
  # AnnotationDbi::mapIds() does not degrade to all-NA on its own here: it
  # routes through .testForValidKeys(), which THROWS ("None of the keys
  # entered are valid keys for 'SYMBOL'. ...") whenever NOT ONE key is valid
  # -- verified directly against org.Hs.eg.db 2026-08-31. That is distinct
  # from the partial-invalid case in the test above, where mapIds returns NA
  # only for the bad key and never throws. Every fixture elsewhere in this
  # file mixes in at least one valid symbol, which is exactly why nothing
  # caught this: this is the one whose genes are ALL invalid.
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  out <- NULL
  expect_no_error(out <- rummagene_map_symbols(c("NOTAREALSYMBOL123", "ALSONOTREAL456"), "Homo sapiens"))
  expect_true(base::is.na(out[["NOTAREALSYMBOL123"]]))
  expect_true(base::is.na(out[["ALSONOTREAL456"]]))
})

test_that("rummagene_map_symbols re-throws an unrelated mapIds error instead of treating it as all-unmapped", {
  # Proves the tryCatch inside rummagene_map_symbols() is scoped to the exact
  # "None of the keys entered are valid keys" message and does not become a
  # blanket swallow of every mapIds() failure -- a genuinely broken
  # org.Hs.eg.db install, a corrupt database, or a coding mistake upstream
  # must still surface rather than being silently reported as "unmapped".
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  testthat::local_mocked_bindings(
    mapIds = function(...) base::stop("simulated corrupt annotation database"),
    .package = "AnnotationDbi"
  )
  expect_error(rummagene_map_symbols("TP53", "Homo sapiens"), "simulated corrupt annotation database")
})

test_that("rummagene_map_symbols refuses an organism outside scope", {
  expect_error(rummagene_map_symbols("TP53", "Mus musculus"), "only Homo sapiens")
})

test_that("rummagene_gate accepts a set whose every symbol resolves", {
  # org.Hs.eg.db is a Bioconductor ANNOTATION package. It is in neither the
  # published image nor CI (both install only testthat/pkgload on top of the
  # image), so anything reaching rummagene_map_symbols() has to skip without
  # it. These tests passed for weeks only because the package happened to be
  # installed by hand into a long-lived container; recreating that container
  # turned them into errors, which is how the missing guard surfaced.
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  conn <- test_conn()
  new_hashkeys <- base::character(0)
  on.exit({
    unseed_features(conn, new_hashkeys)
    DBI::dbDisconnect(conn)
  }, add = TRUE)
  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510", "ensg00000136997"))

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("TP53", "MYC"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_true(out$ok)
  expect_setequal(out$feature_names, c("ensg00000141510", "ensg00000136997"))
})

test_that("rummagene_gate rejects a set with one unmappable symbol", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  # The whole point of the 100%-mappable rule: a single dead alias disqualifies
  # the set rather than being silently dropped, so a stored signature always
  # matches the published gene list exactly.
  conn <- test_conn()
  new_hashkeys <- base::character(0)
  on.exit({
    unseed_features(conn, new_hashkeys)
    DBI::dbDisconnect(conn)
  }, add = TRUE)
  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510"))

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("TP53", "LGTN"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_false(out$ok)
  expect_equal(out$reason, "unmapped_symbol")
})

test_that("rummagene_gate rejects a set whose every symbol is invalid, rather than erroring", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  # C1: AnnotationDbi::mapIds() THROWS instead of returning NAs when NOT ONE
  # symbol in the set is a valid SYMBOL key (see rummagene_map_symbols()'s own
  # comment) -- which made the "unmapped_symbol" branch below UNREACHABLE in
  # exactly the case it exists for. Every other gate fixture in this file
  # mixes in at least one valid symbol, which is why nothing else here would
  # have caught this. Real Rummagene sets scraped from a miRNA or probe-id
  # column can be entirely unmappable, at real volume, so this must be a
  # normal rejection -- not an uncaught error that aborts the whole build.
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("NOTAREALSYMBOL123", "ALSONOTREAL456"), pmcid = "PMC1")

  out <- NULL
  expect_no_error(out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L))
  expect_false(out$ok)
  expect_equal(out$reason, "unmapped_symbol")
})

test_that("rummagene_gate rejects a set whose Ensembl id is absent from the reference table", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  # Distinct from unmapped: the symbol maps fine, but that Ensembl id is not in
  # THIS database. Checking the live table rather than org.Hs.eg.db is what
  # makes Ensembl version drift move a set out of the catalog instead of
  # producing a row that fails on pull.
  conn <- test_conn()
  new_hashkeys <- base::character(0)
  on.exit({
    unseed_features(conn, new_hashkeys)
    DBI::dbDisconnect(conn)
  }, add = TRUE)
  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510"))
  # MYC maps to ensg00000136997, which a populated reference table DOES hold.
  # Remove it for the duration so "absent" is a fact this test establishes
  # rather than an accident of an empty fixture -- see suppress_feature().
  restore_myc <- suppress_feature(conn, "ensg00000136997", 2L)
  # after = FALSE PREPENDS. on.exit(add = TRUE) appends, which would put this
  # restore AFTER the handler that disconnects `conn` -- it would then run
  # against a dead connection and INSERT IGNORE would swallow the failure,
  # silently leaving the reference table one gene short. That happened.
  on.exit(restore_myc(), add = TRUE, after = FALSE)

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("TP53", "MYC"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_false(out$ok)
  expect_equal(out$reason, "feature_absent")
})

test_that("rummagene_gate deduplicates two symbols that map to the same Ensembl id", {
  # multiVals = "first" in rummagene_map_symbols() means two DIFFERENT
  # symbols can legitimately land on the SAME Ensembl id. Verified directly
  # against org.Hs.eg.db (2026-08-31): SMIM8 and LINC01590 both map to
  # ENSG00000111850. feature_names must report that id once, not twice --
  # the verbatim published symbols are preserved elsewhere as gene_symbols,
  # so nothing here drops a gene; it only collapses a mapping artifact in
  # the DERIVED Ensembl list.
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  conn <- test_conn()
  new_hashkeys <- base::character(0)
  on.exit({
    unseed_features(conn, new_hashkeys)
    DBI::dbDisconnect(conn)
  }, add = TRUE)
  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000111850"))

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("SMIM8", "LINC01590"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_true(out$ok)
  expect_equal(out$feature_names, "ensg00000111850")
})

catalog_row_fixture <- function(term = "PMC1-t.xlsx-x", genes = c("TP53", "MYC")) {
  base::list(
    term = term, pmcid = "PMC1", pmid = "111", title = "A paper", year = 2020L,
    doi = "10.1/x", description = "d", organism = "Homo sapiens",
    assay_type = "transcriptomics", mesh_evidence = "Humans, Transcriptome",
    gene_symbols = genes, feature_names = c("ensg00000141510", "ensg00000136997")
  )
}

test_that("rummagene_catalog_upsert writes a row that reads back intact", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  n <- rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")
  expect_equal(n, 1)

  got <- DBI::dbGetQuery(conn, "SELECT * FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
  expect_equal(base::nrow(got), 1)
  expect_equal(got$term[1], "PMC1-t.xlsx-x")
  expect_equal(got$n_genes[1], 2)
  expect_equal(got$organism[1], "Homo sapiens")
  # Genes round-trip as a delimited list in both namespaces.
  expect_equal(base::strsplit(got$gene_symbols[1], ",", fixed = TRUE)[[1]], c("TP53", "MYC"))
  expect_equal(base::strsplit(got$feature_names[1], ",", fixed = TRUE)[[1]],
               c("ensg00000141510", "ensg00000136997"))
})

test_that("rummagene_catalog_upsert is idempotent", {
  # A second build over an unchanged GMT must be a no-op, not a duplicate-key
  # error and not a second row.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")
  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")

  got <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
  expect_equal(got$n[1], 1)
})

test_that("rummagene_catalog_upsert refreshes a term whose genes changed", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version IN ('test-v1','test-v2')")
            DBI::dbDisconnect(conn) }, add = TRUE)

  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")
  changed <- catalog_row_fixture(genes = c("TP53", "MYC", "EGFR"))
  rummagene_catalog_upsert(conn, base::list(changed), gmt_version = "test-v2")

  got <- DBI::dbGetQuery(conn, "SELECT n_genes, gmt_version FROM rummagene_catalog WHERE pmcid = 'PMC1'")
  expect_equal(base::nrow(got), 1)
  expect_equal(got$n_genes[1], 3)
  expect_equal(got$gmt_version[1], "test-v2")
})

test_that("rummagene_catalog_prune deletes rows from earlier builds only", {
  # A set withdrawn from Rummagene stops being offered. Any signature already
  # pulled from it is untouched -- it lives in `signatures` with its provenance,
  # and there is deliberately no FK between the two tables.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version IN ('test-v1','test-v2')")
            DBI::dbDisconnect(conn) }, add = TRUE)

  # rummagene_catalog_prune() deletes every row that does NOT carry the target
  # gmt_version -- by design, so a real build's withdrawals take effect (see
  # the guard inside rummagene_catalog_prune() itself for the failed-build
  # case). test_conn() is the real local dev database, per helper-rummagene.R,
  # which tests must not leave a mark on. If the build job has ever populated
  # this table for real, running this unconditionally would delete every one
  # of those rows. Skip loudly instead of risking a developer's actual catalog.
  foreign <- DBI::dbGetQuery(conn,
    "SELECT COUNT(*) n FROM rummagene_catalog WHERE gmt_version NOT IN ('test-v1', 'test-v2')")$n[1]
  testthat::skip_if(
    base::as.numeric(foreign) > 0,
    "rummagene_catalog holds non-test rows; skipping to avoid pruning a real catalog"
  )

  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture(term = "PMC1-old")), gmt_version = "test-v1")
  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture(term = "PMC1-new")), gmt_version = "test-v2")

  deleted <- rummagene_catalog_prune(conn, gmt_version = "test-v2")
  expect_gte(deleted, 1)

  remaining <- DBI::dbGetQuery(conn, "SELECT term FROM rummagene_catalog WHERE gmt_version = 'test-v2'")
  expect_equal(remaining$term, "PMC1-new")
})

test_that("rummagene_catalog_upsert stores NA metadata as SQL NULL, not the string \"NA\"", {
  # Governing rule for this whole plan: nothing is invented. A row that
  # arrives without a pmid/title/year/doi must be stored as true SQL NULL --
  # never a fabricated value, and never R's NA stringified into the literal
  # two-character text "NA". sql_value() is what's supposed to guarantee
  # this; this is the regression test that holds it to that.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-na'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  row <- catalog_row_fixture(term = "PMC1-na-check")
  row$pmid <- NA_character_
  row$title <- NA_character_
  row$year <- NA_integer_
  row$doi <- NA_character_

  rummagene_catalog_upsert(conn, base::list(row), gmt_version = "test-na")

  got <- DBI::dbGetQuery(conn,
    "SELECT pmid, title, year, doi FROM rummagene_catalog WHERE gmt_version = 'test-na'")
  expect_equal(base::nrow(got), 1)
  expect_true(base::is.na(got$pmid[1]))
  expect_true(base::is.na(got$title[1]))
  expect_true(base::is.na(got$year[1]))
  expect_true(base::is.na(got$doi[1]))
  # The specific regression this guards against: NA stringifying into the
  # two-character text "NA" instead of becoming SQL NULL.
  expect_false(base::identical(got$pmid[1], "NA"))
  expect_false(base::identical(got$title[1], "NA"))
  expect_false(base::identical(got$doi[1], "NA"))

  # The R-level checks above are necessary but NOT sufficient: this suite
  # connects via RMySQL::MySQL(), the old deprecated driver, which is known
  # to run fetched text through type.convert-style coercion -- a genuinely
  # broken write that stored the literal string "NA" would come back from
  # dbGetQuery() already folded into R's NA, making is.na() report TRUE and
  # identical(x, "NA") report FALSE for that bug too. Ask MySQL itself,
  # where the driver's fetch-side coercion cannot interfere: a stored "NA"
  # string is NOT SQL NULL, so IS NULL still correctly reports false for it
  # regardless of what the driver does to the value on the way out.
  null_check <- DBI::dbGetQuery(conn,
    "SELECT pmid IS NULL AS pmid_null, title IS NULL AS title_null,
            year IS NULL AS year_null, doi IS NULL AS doi_null
     FROM rummagene_catalog WHERE gmt_version = 'test-na'")
  expect_true(base::as.logical(null_check$pmid_null[1]))
  expect_true(base::as.logical(null_check$title_null[1]))
  expect_true(base::as.logical(null_check$year_null[1]))
  expect_true(base::as.logical(null_check$doi_null[1]))
})

test_that("rummagene_catalog_upsert skips a term over the column's 512-character limit instead of erroring", {
  # I1(a): checked in R rather than left to MySQL, because under a
  # non-strict sql_mode MySQL would not error at all -- it would silently
  # TRUNCATE `term` while term_hashkey (hashed from the FULL term) keeps
  # hashing the untruncated text, so get_rummagene_catalog_entry() could
  # never find the row again by that hash. This dev database runs
  # STRICT_TRANS_TABLES (verified 2026-08-31), so an unguarded INSERT here
  # would throw "Data too long for column 'term'" and, pre-fix, would have
  # aborted the whole build; this asserts the R-side guard catches it first,
  # for either sql_mode.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-unstorable-len'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  long_term <- base::paste(base::rep("x", 600), collapse = "")
  row <- catalog_row_fixture(term = long_term)

  report_count <- 0L
  report_message <- NULL
  n <- NULL
  expect_no_error(n <- rummagene_catalog_upsert(
    conn, base::list(row), gmt_version = "test-unstorable-len",
    on_unstorable = function(row, message) {
      report_count <<- report_count + 1L
      report_message <<- message
    }
  ))

  expect_equal(n, 0)
  expect_equal(report_count, 1L)
  expect_match(report_message, "512", fixed = TRUE)

  got <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM rummagene_catalog WHERE gmt_version = 'test-unstorable-len'")
  expect_equal(got$n[1], 0)
})

test_that("rummagene_catalog_upsert skips a term with a character its utf8 (utf8mb3) column cannot encode, instead of aborting", {
  # I1(b): the table is CHARSET=utf8, which MySQL treats as utf8mb3 -- 3
  # bytes per character, so it cannot store a 4-byte character. Unlike the
  # length check above, there is no cheap R-side precheck for this one, so
  # the guard has to be the tryCatch around the write itself. U+1F600 is a
  # 4-byte character in UTF-8, the kind a scraped Excel sheet name can carry.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-unstorable-charset'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  emoji_term <- base::paste0("PMC1-t.xlsx-", base::intToUtf8(0x1F600))
  row <- catalog_row_fixture(term = emoji_term)

  report_count <- 0L
  report_message <- NULL
  n <- NULL
  expect_no_error(n <- rummagene_catalog_upsert(
    conn, base::list(row), gmt_version = "test-unstorable-charset",
    on_unstorable = function(row, message) {
      report_count <<- report_count + 1L
      report_message <<- message
    }
  ))

  expect_equal(n, 0)
  expect_equal(report_count, 1L)
  expect_match(report_message, "Incorrect string value", fixed = TRUE)

  got <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM rummagene_catalog WHERE gmt_version = 'test-unstorable-charset'")
  expect_equal(got$n[1], 0)
})

test_that("rummagene_catalog_upsert warns instead of silently dropping an unstorable row when no on_unstorable callback is given", {
  # A caller that does not wire the callback must not be able to mistake a
  # lower `written` count for every row having succeeded.
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  long_term <- base::paste(base::rep("x", 600), collapse = "")
  row <- catalog_row_fixture(term = long_term)

  n <- NULL
  expect_warning(
    n <- rummagene_catalog_upsert(conn, base::list(row), gmt_version = "test-unstorable-warn-only"),
    "unstorable"
  )
  expect_equal(n, 0)
})

test_that("rummagene_catalog_upsert re-throws a storage error that is not a known unstorable-row shape", {
  # Proves the tryCatch inside rummagene_catalog_upsert() is scoped to the two
  # known "this row's data does not fit the column" shapes and does not
  # become a blanket swallow of every INSERT failure -- a real bug or a real
  # outage must still abort loudly. NA organism reaches MySQL as a genuine
  # SQL NULL (DBI::dbQuoteLiteral() renders NA_character_ as the literal
  # NULL, verified 2026-08-31), which violates `organism`'s NOT NULL
  # constraint with a message this function's pattern does not match.
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  row <- catalog_row_fixture(term = "PMC1-null-organism-should-not-be-stored")
  row$organism <- NA_character_

  expect_error(
    rummagene_catalog_upsert(conn, base::list(row), gmt_version = "test-should-not-exist"),
    "cannot be null"
  )
})

seed_catalog <- function(conn) {
  rows <- base::list(
    utils::modifyList(catalog_row_fixture(term = "PMC1-liver-up"),  base::list(title = "Liver study", year = 2019L)),
    utils::modifyList(catalog_row_fixture(term = "PMC2-tumor-down"), base::list(title = "Tumor study", year = 2023L, pmcid = "PMC2")),
    utils::modifyList(catalog_row_fixture(term = "PMC3-liver-down"), base::list(title = "Another liver", year = 2021L, pmcid = "PMC3"))
  )
  rummagene_catalog_upsert(conn, rows, gmt_version = "test-search")
}

test_that("search_rummagene_catalog returns a page plus the total matching count", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, limit = 2, offset = 0)
  expect_equal(base::nrow(out$rows), 2)
  expect_gte(out$count, 3)
})

test_that("search_rummagene_catalog matches free text against term and title", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, q = "liver", limit = 50)
  expect_true(base::all(base::grepl("liver", base::tolower(
    base::paste(out$rows$term, out$rows$title)))))
  expect_gte(base::nrow(out$rows), 2)
})

test_that("search_rummagene_catalog filters by year range", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, year_min = 2021, limit = 50)
  expect_true(base::all(out$rows$year >= 2021))
})

test_that("search_rummagene_catalog sorts server-side on a whitelisted column", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, sort_by = "year", sort_dir = "desc", limit = 50)
  expect_equal(out$rows$year, base::sort(out$rows$year, decreasing = TRUE))
})

test_that("search_rummagene_catalog ignores an unknown sort column instead of interpolating it", {
  # sort_by lands in ORDER BY, where quoting cannot protect it -- the same
  # reasoning as .signature_sort_columns in api/lib/signature.R.
  #
  # This payload is deliberately a SINGLE SQL statement (a trailing comment,
  # not a stacked query): neither the test connection nor the production pool
  # sets CLIENT_MULTI_STATEMENTS, so a ";"-based payload would be rejected by
  # MySQL itself regardless of whether the whitelist below exists, and the
  # test would pass for the wrong reason. "year DESC -- " is, on its own,
  # valid SQL wherever the ORDER BY expression lands -- if the whitelist were
  # ever bypassed and sort_by interpolated raw, the trailing comment would
  # swallow the "ASC" this call explicitly requests, and the rows would come
  # back sorted DESCENDING instead. Asserting ascending order below is what
  # actually proves the whitelist -- not MySQL's inability to run a second
  # statement -- is what makes this safe.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- NULL
  expect_no_error(
    out <- search_rummagene_catalog(conn, sort_by = "year DESC -- ", sort_dir = "asc", limit = 50)
  )
  expect_equal(out$rows$year, base::sort(out$rows$year, decreasing = FALSE))
  expect_equal(base::nrow(DBI::dbGetQuery(conn, "SELECT 1 FROM rummagene_catalog LIMIT 1")), 1)
})

test_that("search_rummagene_catalog omits the large gene columns", {
  # The list endpoint must never ship gene_symbols/feature_names -- 135k rows of
  # 40 genes each is why they are fetched only on a detail view.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, limit = 5)
  expect_false("gene_symbols" %in% base::colnames(out$rows))
  expect_false("feature_names" %in% base::colnames(out$rows))
})

test_that("get_rummagene_catalog_entry round-trips gene_symbols and feature_names as vectors", {
  # Task 8's detail route and Task 9's pull path both build on this return
  # value -- Task 9 in particular turns feature_names into a REAL, persisted
  # signatures row, so the round trip must land on character vectors, not
  # the comma-joined storage string.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-entry'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  row <- catalog_row_fixture(term = "PMC1-entry-check", genes = c("TP53", "MYC"))
  rummagene_catalog_upsert(conn, base::list(row), gmt_version = "test-entry")

  out <- get_rummagene_catalog_entry(conn, "PMC1-entry-check")

  expect_equal(out$term, "PMC1-entry-check")
  expect_type(out$gene_symbols, "character")
  expect_equal(out$gene_symbols, c("TP53", "MYC"))
  expect_type(out$feature_names, "character")
  expect_equal(out$feature_names, c("ensg00000141510", "ensg00000136997"))
})

test_that("get_rummagene_catalog_entry returns NULL for a term not in the catalog", {
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  expect_null(get_rummagene_catalog_entry(conn, "no-such-term-at-all"))
})

test_that("get_rummagene_catalog_entry degrades to an empty vector when the gene lists are empty", {
  # strsplit("", ",", fixed = TRUE)[[1]] yields character(0), not a length-1
  # vector holding "" -- this is the regression test that holds the reader
  # to that.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-entry-empty'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  row <- catalog_row_fixture(term = "PMC1-entry-empty")
  row$gene_symbols <- base::character(0)
  row$feature_names <- base::character(0)
  rummagene_catalog_upsert(conn, base::list(row), gmt_version = "test-entry-empty")

  out <- get_rummagene_catalog_entry(conn, "PMC1-entry-empty")
  expect_equal(out$gene_symbols, base::character(0))
  expect_equal(out$feature_names, base::character(0))
})

test_that("search_rummagene_catalog treats a missing numeric bound (NULL, NA, or empty string) as not supplied", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  # The route layer's json_scalar() yields "" for a parameter the caller
  # never supplied -- that, an explicit NA, and the default NULL must all
  # stay silently absent rather than becoming a filter that matches nothing.
  out <- search_rummagene_catalog(
    conn, year_min = "", year_max = NA, n_genes_min = NA_character_, limit = 50
  )
  expect_gte(base::nrow(out$rows), 3)
})

test_that("search_rummagene_catalog errors on an unparseable numeric bound instead of silently dropping it", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  # A genuinely supplied but garbage bound must fail loudly. Silently
  # dropping it would be indistinguishable, from the caller's side, from
  # "filtered correctly and nothing matched" -- the same failure shape the
  # governing "nothing is invented" rule exists to prevent.
  # Pinned to the literal wording the route layer's 400-vs-500 branch keys
  # off of (rummagene_catalog_route in api.R matches "must be a number" via
  # grepl(..., fixed = TRUE)) -- an unpinned expect_error() would keep passing
  # even if this message were reworded, silently breaking that branch without
  # any test noticing.
  expect_error(search_rummagene_catalog(conn, year_min = "abc", limit = 50), "year_min must be a number")
  expect_error(search_rummagene_catalog(conn, n_genes_max = "many", limit = 50), "n_genes_max must be a number")
})

test_that("search_rummagene_catalog falls back to the default limit instead of crashing on a non-numeric one", {
  # as.integer("abc") is NA; unguarded, sprintf's %d would render the literal
  # text "LIMIT NA" and MySQL would reject the whole query.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- NULL
  expect_no_error(out <- search_rummagene_catalog(conn, limit = "abc"))
  expect_gte(base::nrow(out$rows), 3)
})

test_that("search_rummagene_catalog treats a negative offset as zero instead of erroring", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- NULL
  expect_no_error(out <- search_rummagene_catalog(conn, offset = -5, limit = 50))
  expect_gte(base::nrow(out$rows), 3)
})

test_that("search_rummagene_catalog caps the limit instead of returning the whole table", {
  # 3 seeded rows (seed_catalog()'s usual fixture) can never exercise a cap
  # of 100 -- nrow(out$rows) <= 100 would be trivially true whether the cap
  # is 100, 5, or removed entirely. Seed comfortably past the cap, under
  # this test's own gmt_version, so the assertions below actually exercise
  # the code path they name.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-cap'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  n_seed <- 105L
  rows <- base::lapply(base::seq_len(n_seed), function(i) {
    catalog_row_fixture(term = base::sprintf("PMC-cap-%03d", i))
  })
  rummagene_catalog_upsert(conn, rows, gmt_version = "test-cap")

  out <- search_rummagene_catalog(conn, limit = 500)
  # Exact, not an upper bound -- the whole point is that 500 does not win.
  expect_equal(base::nrow(out$rows), 100)
  # The cap must trim the PAGE, not the reported total: a caller needs to
  # know there are more rows than it received, or the pager breaks. This is
  # >= n_seed (105), strictly more than the 100-row page, which is what
  # actually distinguishes "count reports the true total" from a bug that
  # caps count down to the page size too.
  expect_gte(out$count, n_seed)
})

test_that("api.R declares the rummagene catalog route with every documented parameter", {
  api <- base::paste(base::readLines(testthat::test_path("../../api/api.R")), collapse = "\n")
  # Trailing "\n" matters: without it this substring is also a prefix of
  # "@get /rummagene/catalog/entry", so the assertion would keep passing
  # even if this route's own annotation line were deleted, as long as the
  # entry route still existed. See the entry-route test below for the same.
  expect_match(api, "@get /rummagene/catalog\n", fixed = TRUE)
  for (p in c("api_key", "q", "organism", "assay_type", "year_min", "year_max",
              "n_genes_min", "n_genes_max", "limit", "offset", "sort_by", "sort_dir")) {
    expect_match(api, base::sprintf("#* @param %s", p), fixed = TRUE,
                 info = base::sprintf("route parameter %s not documented", p))
  }
})

test_that("api.R declares a catalog entry route that can serve one entry's genes", {
  api <- base::paste(base::readLines(testthat::test_path("../../api/api.R")), collapse = "\n")
  expect_match(api, "@get /rummagene/catalog/entry", fixed = TRUE)
})

test_that("rummagene_catalog_omic_signature builds a signature keyed by Ensembl id", {
  # feature_name must be the Ensembl ids, because that is what
  # create_signature.R's resolve_feature_ids() hashes and looks up. Handing it
  # symbols is exactly the failure that makes 0% of Rummagene sets uploadable.
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  entry <- base::list(
    term = "PMC1-t.xlsx-x", pmcid = "PMC1", title = "A paper", year = 2020L,
    description = "d", organism = "Homo sapiens", assay_type = "transcriptomics",
    mesh_evidence = "Humans, Transcriptome",
    gene_symbols  = c("TP53", "MYC"),
    feature_names = c("ensg00000141510", "ensg00000136997")
  )
  os <- base::suppressWarnings(rummagene_catalog_omic_signature(entry))

  expect_s3_class(os, "OmicSignature")
  expect_setequal(os$signature$feature_name, c("ensg00000141510", "ensg00000136997"))
  expect_equal(os$metadata$phenotype, "unknown")
  expect_equal(os$metadata$direction_type, "uni-directional")
  expect_equal(os$metadata$organism, "Homo sapiens")
  expect_false("group_label" %in% base::colnames(os$signature))
})

test_that("rummagene_catalog_omic_signature records provenance including the untruncated term", {
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  long_term <- base::paste0("PMC1-", base::paste(base::rep("x", 400), collapse = ""))
  entry <- base::list(
    term = long_term, pmcid = "PMC1", title = "t", year = 2020L, description = "d",
    organism = "Homo sapiens", assay_type = "transcriptomics",
    mesh_evidence = "Humans, Transcriptome",
    gene_symbols = c("TP53", "MYC"), feature_names = c("ensg00000141510", "ensg00000136997")
  )
  os <- base::suppressWarnings(rummagene_catalog_omic_signature(entry))

  expect_lte(base::nchar(os$metadata$signature_name), 255)
  expect_match(os$metadata$others, long_term, fixed = TRUE)
  # "key: value; key: value" -- the form SigRepo:::parseRetrievedOthers()
  # reads back. Assert the pairs, not a bare substring.
  expect_match(os$metadata$others, "source: rummagene", fixed = TRUE)
  expect_match(os$metadata$others, "mesh_evidence: ", fixed = TRUE)
  # direction_type is INFERRED, not attested -- Rummagene has no direction
  # field and the column is NOT NULL with no "unknown" member. The
  # provenance has to say so or a reader cannot tell it apart from
  # organism/assay_type, which MeSH genuinely attests.
  expect_match(os$metadata$others, "direction_type: not stated by source", fixed = TRUE)
})

test_that("api.R declares the rummagene pull route", {
  api <- base::paste(base::readLines(testthat::test_path("../../api/api.R")), collapse = "\n")
  expect_match(api, "@post /rummagene/pull", fixed = TRUE)
})

# ---------------------------------------------------------------------------
# POST /rummagene/pull -- exercising the route itself, not just the library
# functions it wraps.
#
# rummagene_pull_route lives in api.R, which Plumber parses directly for its
# `#*` annotations (see the comment above api.R's own lib-loading loop) --
# nothing else in this suite sources api.R or calls a route function; routes
# are otherwise verified only by matching their annotation text, as the
# "declares the rummagene pull route" test above does. But the branches this
# route exists for -- the empty-term 400, the unknown-term 404, the
# reason -> status switch, and the message-less forbidden fallback -- live
# INSIDE the route itself: text-matching cannot exercise them, and the
# underlying library functions don't reproduce them either (e.g.
# build_signature_from_upload() has no concept of an HTTP status code at
# all). Sourcing api.R into its OWN, throwaway environment (never globalenv)
# gets a callable reference to the real route without redefining any of
# api.R's ~100 other routes anywhere another test file could see them --
# api.R only DEFINES functions at its top level (actually running the server
# is the separate run_sigrepo_api.R) -- and the result is cached so it only
# happens once for this whole file's run.
.rummagene_pull_route_cache <- base::new.env()

rummagene_pull_route_for_test <- function() {
  if (base::is.null(.rummagene_pull_route_cache$route)) {
    server_dir <- base::Sys.getenv("SIGREPO_SERVER_DIR")
    testthat::skip_if_not(base::nzchar(server_dir), "SIGREPO_SERVER_DIR not set")
    env <- base::new.env(parent = base::globalenv())
    base::sys.source(base::file.path(server_dir, "api", "api.R"), envir = env)
    .rummagene_pull_route_cache$route <- env$rummagene_pull_route
  }
  .rummagene_pull_route_cache$route
}

# json_response()/json_error() only ever do res$serializer <- ...; res$status
# <- ... -- a plain environment (mutated in place, unlike a list) is enough
# to read the status back after the call, without needing a real
# PlumberResponse object.
mock_plumber_res <- function() base::new.env()

# A throwaway users() row with a self-fabricated, never-looked-up api_key --
# never a real user's credential. signatures.user_name carries an actual
# FOREIGN KEY to users(user_name) (mysql/schema/signatures.sql), so a
# successful (or forbidden) pull cannot be exercised without a real row
# there. The ci_admin/ci_viewer fixture users test-create-signature.R and
# test-collection.R rely on only exist in CI's seeded database
# (tests/testthat/fixtures/seed.sql) -- confirmed absent from this local
# stack -- so a throwaway insert, deleted by name both before and after, is
# the only credential-free way to get one here. Mirrors test-collection.R's
# own throwaway-user pattern.
seed_pull_test_user <- function(conn, user_name, user_role) {
  DBI::dbExecute(conn, base::sprintf("DELETE FROM users WHERE user_name = %s", DBI::dbQuoteLiteral(conn, user_name)))
  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO users (user_name, user_password_hashkey, user_email, user_role, api_key, user_hashkey, active)
     VALUES (%s, 'x', %s, %s, %s, %s, 1)",
    DBI::dbQuoteLiteral(conn, user_name),
    DBI::dbQuoteLiteral(conn, base::paste0(user_name, "@example.com")),
    DBI::dbQuoteLiteral(conn, user_role),
    DBI::dbQuoteLiteral(conn, base::paste0(user_name, "_key")),
    DBI::dbQuoteLiteral(conn, base::paste0(user_name, "_hk"))
  ))
}

unseed_pull_test_user <- function(conn, user_name) {
  DBI::dbExecute(conn, base::sprintf("DELETE FROM users WHERE user_name = %s", DBI::dbQuoteLiteral(conn, user_name)))
}

# Deletes exactly the rows a successful pull wrote, in the same order
# create_signature.R's own rollback() uses on its failure path -- never a
# bare DELETE.
delete_pull_test_signature <- function(conn, signature_hashkey) {
  row <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT signature_id FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, signature_hashkey)
  ))
  if (base::nrow(row) == 0) {
    return(base::invisible(NULL))
  }
  sid <- row$signature_id[1]
  DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", sid))
  DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_access WHERE signature_id = %d", sid))
  DBI::dbExecute(conn, base::sprintf("DELETE FROM signatures WHERE signature_id = %d", sid))
}

test_that("POST /rummagene/pull falls back to the JSON body when plumber's own arg-binding leaves api_key/term empty", {
  # Regression test for the reliability gap: with only `@parser json`,
  # plumber populates req$body (and from it, matched-name args) only when
  # Content-Type resolves -- after parser_picker() strips any
  # ";charset=..." suffix -- to exactly application/json or text/json;
  # anything else (a client defaulting to
  # application/x-www-form-urlencoded or text/plain) falls through to the
  # (here, NULL) form alias, req$body stays empty, and api_key/term would
  # silently stay at their "" defaults without the request_json_body(req)
  # fallback every other @parser json @post route in api.R already has.
  # Simulated by calling the route with api_key/term already at their
  # un-populated "" defaults (as plumber would leave them in that scenario)
  # and a `req` carrying only the raw, unparsed postBody -- exactly what a
  # non-JSON Content-Type would leave behind.
  conn <- test_conn()
  user_name <- "rg_pull_test_body"
  seed_pull_test_user(conn, user_name, "editor")
  on.exit({
    unseed_pull_test_user(conn, user_name)
    DBI::dbDisconnect(conn)
  }, add = TRUE)

  route <- rummagene_pull_route_for_test()
  res <- mock_plumber_res()
  req <- base::list(postBody = base::as.character(jsonlite::toJSON(
    base::list(api_key = base::paste0(user_name, "_key"), term = "term-supplied-only-in-the-body"),
    auto_unbox = TRUE
  )))
  out <- route(req = req, res = res, api_key = "", term = "")

  # Getting the 404 "no such catalog entry" (rather than a 400 "provide a
  # term", which is what an unread, still-empty term would produce, or an
  # auth failure, which is what an unread, still-empty api_key would
  # produce) is what proves BOTH fields were actually read out of the body.
  expect_equal(res$status, 404)
  expect_match(out$MESSAGES, "No Rummagene catalog entry", fixed = TRUE)
})

test_that("POST /rummagene/pull returns 400 for an empty term", {
  conn <- test_conn()
  user_name <- "rg_pull_test_editor"
  seed_pull_test_user(conn, user_name, "editor")
  on.exit({
    unseed_pull_test_user(conn, user_name)
    DBI::dbDisconnect(conn)
  }, add = TRUE)

  route <- rummagene_pull_route_for_test()
  res <- mock_plumber_res()
  out <- route(req = NULL, res = res, api_key = base::paste0(user_name, "_key"), term = "")

  expect_equal(res$status, 400)
  expect_match(out$MESSAGES, "Provide the", fixed = TRUE)
})

test_that("POST /rummagene/pull returns 404 for a term not in the catalog", {
  conn <- test_conn()
  user_name <- "rg_pull_test_editor"
  seed_pull_test_user(conn, user_name, "editor")
  on.exit({
    unseed_pull_test_user(conn, user_name)
    DBI::dbDisconnect(conn)
  }, add = TRUE)

  route <- rummagene_pull_route_for_test()
  res <- mock_plumber_res()
  out <- route(req = NULL, res = res, api_key = base::paste0(user_name, "_key"), term = "no-such-term-anywhere-in-the-catalog")

  expect_equal(res$status, 404)
  expect_match(out$MESSAGES, "No Rummagene catalog entry", fixed = TRUE)
})

test_that("POST /rummagene/pull returns 403 with the no-message fallback for a non-editor caller", {
  # build_signature_from_upload() returns list(ok = FALSE, reason =
  # "forbidden") with NO message field (create_signature.R:362) -- this is
  # exactly the case the route's `result$message %||% "..."` fallback exists
  # for. Getting the fallback text (rather than an error, or a literal "NULL")
  # is what proves that line is doing its job.
  conn <- test_conn()
  user_name <- "rg_pull_test_viewer"
  seed_pull_test_user(conn, user_name, "viewer")

  new_hashkeys <- base::character(0)
  on.exit({
    unseed_pull_test_user(conn, user_name)
    DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-pull-forbidden'")
    unseed_features(conn, new_hashkeys)
    DBI::dbDisconnect(conn)
  }, add = TRUE)

  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510", "ensg00000136997"))
  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture(term = "PMC1-pull-forbidden")), gmt_version = "test-pull-forbidden")

  route <- rummagene_pull_route_for_test()
  res <- mock_plumber_res()
  out <- route(req = NULL, res = res, api_key = base::paste0(user_name, "_key"), term = "PMC1-pull-forbidden")

  expect_equal(res$status, 403)
  expect_equal(out$MESSAGES, "This signature could not be created.")
})

test_that("POST /rummagene/pull creates a signature whose feature set matches the catalog entry's feature_names exactly", {
  conn <- test_conn()
  user_name <- "rg_pull_test_editor2"
  seed_pull_test_user(conn, user_name, "editor")

  new_hashkeys <- base::character(0)
  signature_hashkey <- NULL
  on.exit({
    if (!base::is.null(signature_hashkey)) {
      delete_pull_test_signature(conn, signature_hashkey)
    }
    unseed_pull_test_user(conn, user_name)
    DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-pull-success'")
    unseed_features(conn, new_hashkeys)
    DBI::dbDisconnect(conn)
  }, add = TRUE)

  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510", "ensg00000136997"))
  term <- "PMC1-pull-success"
  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture(term = term)), gmt_version = "test-pull-success")
  entry <- get_rummagene_catalog_entry(conn, term)

  route <- rummagene_pull_route_for_test()
  res <- mock_plumber_res()
  out <- route(req = NULL, res = res, api_key = base::paste0(user_name, "_key"), term = term)

  expect_equal(res$status, 200)
  expect_true(base::is.character(out$signature_hashkey) && base::nzchar(out$signature_hashkey))
  signature_hashkey <- out$signature_hashkey

  written <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT tf.feature_name FROM signature_feature_set sfs
       JOIN signatures s ON s.signature_id = sfs.signature_id
       JOIN transcriptomics_features tf ON tf.feature_id = sfs.feature_id
     WHERE s.signature_hashkey = %s",
    DBI::dbQuoteLiteral(conn, signature_hashkey)
  ))$feature_name

  expect_equal(base::length(written), base::length(entry$feature_names))
  expect_setequal(written, entry$feature_names)
})
