source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/feature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

# Reference feature search, backing the Browse page.
#
# The page this replaced rendered five hardcoded genes from web/src/data/mock.ts
# and, worse, misrepresented the schema: it showed a "chromosome" column for
# transcriptomics features (that table has none) and put the gene SYMBOL in
# feature_name with the Ensembl id in a "gene_id" column, which is backwards.
# So the tests that matter here are the ones asserting that what comes back
# corresponds to columns the database actually has.

feature_conn <- function() {
  testthat::skip_if_not(base::nzchar(base::Sys.getenv("DB_NAME")), "no database configured")
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = base::Sys.getenv("DB_HOST"), port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), password = base::Sys.getenv("DB_PASSWORD"),
    dbname = base::Sys.getenv("DB_NAME")
  )
}

test_that("returned columns exist on the table they came from", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # The whole point. Every column the search claims to return is checked against
  # information_schema, so this fails the moment the API promises a field the
  # database does not hold -- which is exactly what the mock page did.
  for (assay in feature_assay_types()) {
    src <- FEATURE_SOURCES[[assay]]
    live <- DBI::dbGetQuery(conn, base::sprintf("
      SELECT COLUMN_NAME FROM information_schema.COLUMNS
      WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = %s",
      DBI::dbQuoteLiteral(conn, src$table)))$COLUMN_NAME
    testthat::expect_true(base::length(live) > 0, info = src$table)
    for (col in src$columns) {
      testthat::expect_true(col %in% live, info = base::paste(src$table, col))
    }
  }
})

test_that("each assay type returns its own column set", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  tx <- search_features(conn, "transcriptomics", limit = 1)
  testthat::expect_true("gene_symbol" %in% tx$columns)
  # transcriptomics has no chromosome. The mock showed one anyway.
  testthat::expect_false("chromosome" %in% tx$columns)

  snp <- search_features(conn, "snps", limit = 1)
  testthat::expect_true(base::all(base::c("chromosome", "position", "annotation") %in% snp$columns))
  testthat::expect_false("gene_symbol" %in% snp$columns)
})

test_that("an unsupported assay type is a caller error naming the valid ones", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  err <- base::tryCatch(search_features(conn, "metabolomics"), error = function(e) base::conditionMessage(e))
  testthat::expect_match(err, "Unsupported assay_type", fixed = TRUE)
  # Metabolomics is genuinely unsupported here rather than forgotten -- its
  # features need an identifier namespace chosen explicitly -- so the message
  # must list what IS available instead of leaving the caller guessing.
  testthat::expect_match(err, "transcriptomics", fixed = TRUE)
})

test_that("search matches a substring of either the identifier or the symbol", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  testthat::skip_if(search_features(conn, "transcriptomics", limit = 1)$total == 0,
                    "no transcriptomics features seeded")

  # A person browsing knows a symbol; the table is keyed by Ensembl id. Matching
  # only feature_name -- which is what the MCP copy of this query does -- would
  # find nothing for the identifier people actually remember.
  by_symbol <- search_features(conn, "transcriptomics", q = "TP53", limit = 50)
  testthat::skip_if(by_symbol$total == 0, "no TP53-like symbols in this database")
  testthat::expect_true(base::any(base::grepl("TP53", by_symbol$rows$gene_symbol, fixed = TRUE)))

  by_id <- search_features(conn, "transcriptomics", q = "ensg00000000003", limit = 5)
  testthat::expect_true(by_id$total >= 1)
})

test_that("total reflects the filter, and paging walks distinct rows", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  all_rows <- search_features(conn, "transcriptomics", limit = 1)
  testthat::skip_if(all_rows$total < 3, "not enough features to page")

  filtered <- search_features(conn, "transcriptomics", q = "TP53", limit = 100)
  # A total that ignored the filter would offer pages that come back empty.
  testthat::expect_lt(filtered$total, all_rows$total)

  p0 <- search_features(conn, "transcriptomics", limit = 2, offset = 0)
  p1 <- search_features(conn, "transcriptomics", limit = 2, offset = 2)
  testthat::expect_equal(base::length(base::intersect(p0$rows$feature_name, p1$rows$feature_name)), 0)
  # Ordering is stable, so a row cannot appear on two pages or none.
  testthat::expect_false(base::is.unsorted(base::c(p0$rows$feature_name, p1$rows$feature_name)))
})

test_that("limit is capped and nonsense paging arguments fall back", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  testthat::skip_if(search_features(conn, "transcriptomics", limit = 1)$total < 101,
                    "fewer than 101 features seeded")
  # Uncapped, a caller could ask for all 102k rows in one response.
  testthat::expect_lte(base::nrow(search_features(conn, "transcriptomics", limit = 5000)$rows), 100)
  testthat::expect_gt(base::nrow(search_features(conn, "transcriptomics", limit = -1)$rows), 0)
  testthat::expect_gt(base::nrow(search_features(conn, "transcriptomics", offset = -5)$rows), 0)
})

test_that("only current feature versions are returned", {
  conn <- feature_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  res <- search_features(conn, "transcriptomics", limit = 100)
  testthat::skip_if(base::nrow(res$rows) == 0, "no features seeded")
  # The feature tables are versioned; showing superseded rows would double-count
  # every re-annotated gene in the browse view.
  live <- DBI::dbGetQuery(conn, "
    SELECT COUNT(*) AS n FROM transcriptomics_features WHERE is_current = 1")$n[1]
  testthat::expect_equal(base::as.integer(res$total), base::as.integer(live))
})

test_that("the mock data module is gone, not merely unreferenced", {
  # It was fake AND wrong about the schema, so leaving it importable invites its
  # return.
  testthat::expect_false(base::file.exists(testthat::test_path("../../web/src/data/mock.ts")))
  browse <- base::paste(base::readLines(
    testthat::test_path("../../web/src/pages/BrowsePage.tsx"), warn = FALSE), collapse = "\n")
  testthat::expect_false(base::grepl("data/mock", browse, fixed = TRUE))
  testthat::expect_match(browse, "searchFeatures", fixed = TRUE)
})
