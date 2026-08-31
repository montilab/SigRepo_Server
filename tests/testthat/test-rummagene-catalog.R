# The Rummagene catalog: build-time gate, storage, and query.
# See specs/2026-08-31-rummagene-catalog-design.md.
source(testthat::test_path("../../api/lib/rummagene.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)

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
