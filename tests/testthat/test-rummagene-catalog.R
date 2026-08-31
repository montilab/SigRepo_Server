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

test_that("rummagene_map_symbols refuses an organism outside scope", {
  expect_error(rummagene_map_symbols("TP53", "Mus musculus"), "only Homo sapiens")
})

test_that("rummagene_gate accepts a set whose every symbol resolves", {
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

test_that("rummagene_gate rejects a set whose Ensembl id is absent from the reference table", {
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
})
