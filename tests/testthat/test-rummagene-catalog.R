# The Rummagene catalog: build-time gate, storage, and query.
# See specs/2026-08-31-rummagene-catalog-design.md.
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog.R"), local = FALSE)

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
