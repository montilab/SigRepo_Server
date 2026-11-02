source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/orthologs.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

# Ortholog lookup over gene_orthologs, backing GET /features/orthologs.
#
# The fixture rows in tests/testthat/fixtures/seed.sql carry the real ids of
# three mouse genes (Adipoq, a clean one-to-one pair; Orm1, which has two human
# orthologs; Trp53), so the same assertions hold against CI's seeded database
# and against a development stack that ran /update_orthologs. Read-only.

ortholog_conn <- function() {
  testthat::skip_if_not(base::nzchar(base::Sys.getenv("DB_NAME")), "no database configured")
  conn <- DBI::dbConnect(
    RMySQL::MySQL(),
    host = base::Sys.getenv("DB_HOST"), port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), password = base::Sys.getenv("DB_PASSWORD"),
    dbname = base::Sys.getenv("DB_NAME")
  )
  n <- base::suppressWarnings(DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM gene_orthologs WHERE is_current = 1"))$n
  if (n == 0) {
    base::suppressWarnings(DBI::dbDisconnect(conn))
    testthat::skip("gene_orthologs is empty on this database")
  }
  conn
}

test_that("a mouse UniProt accession reaches the human Ensembl id through the reference tables", {
  conn <- ortholog_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  out <- lookup_orthologs(conn, c("Q60994", "ENSMUSG00000022878", "Adipoq", "11450"), from = "Mus musculus")

  expect_equal(out$rows$query_type, c("uniprot", "ensembl", "symbol", "entrez"))
  expect_equal(out$rows$query_symbol, rep("Adipoq", 4))
  expect_equal(out$rows$symbol, rep("ADIPOQ", 4))
  expect_equal(out$rows$ensembl, rep("ENSG00000181092", 4))
  expect_equal(out$rows$entrez, rep("9370", 4))
  expect_equal(out$rows$organism, rep("Homo sapiens", 4))
  expect_false(base::any(out$rows$ambiguous))
  expect_equal(out$total, 4)
  expect_equal(
    out$columns,
    base::c("query", "query_type", "query_symbol", "organism", "symbol", "ensembl", "entrez", "support_n", "ambiguous")
  )
})

test_that("ambiguous and unmapped inputs are reported, not dropped", {
  conn <- ortholog_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  out <- lookup_orthologs(conn, c("Q60590", "Nothing123"), from = "Mus musculus")

  expect_equal(out$rows$query, c("Q60590", "Q60590", "Nothing123"))
  expect_setequal(out$rows$symbol[1:2], c("ORM1", "ORM2"))
  expect_equal(out$rows$ambiguous, c(TRUE, TRUE, FALSE))
  expect_true(base::is.na(out$rows$symbol[3]))
  expect_equal(out$total, 3)
})

test_that("human to mouse works from an Ensembl id and a symbol", {
  conn <- ortholog_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  out <- lookup_orthologs(conn, c("ENSG00000181092", "TP53"), from = "Homo sapiens", to = "Mus musculus")

  expect_equal(out$rows$symbol, c("Adipoq", "Trp53"))
  expect_equal(out$rows$ensembl, c("ENSMUSG00000022878", "ENSMUSG00000059552"))
  expect_equal(out$rows$organism, rep("Mus musculus", 2))
})

test_that("ids are trimmed, de-duplicated and matched regardless of case", {
  conn <- ortholog_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  out <- lookup_orthologs(conn, c(" adipoq", "Adipoq ", "ADIPOQ"), from = "Mus musculus")

  expect_equal(out$rows$query, c("adipoq", "Adipoq", "ADIPOQ"))
  expect_equal(out$rows$symbol, rep("ADIPOQ", 3))
})

test_that("caller mistakes are plain errors", {
  conn <- ortholog_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  expect_error(lookup_orthologs(conn, "x", from = "Mus musculus", to = "Mus musculus"), "different organisms")
  expect_error(lookup_orthologs(conn, "x", from = "Mus musculus", to = "CI Test Organism"), "Homo sapiens")
  expect_error(lookup_orthologs(conn, "x", from = "Not an organism"), "Unknown organism")
  expect_error(lookup_orthologs(conn, base::paste0("x", 1:2001), from = "Mus musculus"), "At most 2000")
  # Duplicates are collapsed before the limit applies.
  expect_equal(lookup_orthologs(conn, base::rep("Adipoq", 2001), from = "Mus musculus")$total, 1)
  expect_error(lookup_orthologs(conn, base::character(), from = "Mus musculus"), "No ids")
})

test_that("an accession-shaped human symbol unknown to the proteomics table is read as a symbol", {
  conn <- ortholog_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # H2AC11 is a real human symbol that fits the UniProt pattern. On CI's
  # seed it maps to nothing (one empty row); on a loaded stack the histone
  # cluster gives it many mouse orthologs. Either way it is read as a symbol.
  out <- lookup_orthologs(conn, "H2AC11", from = "Homo sapiens", to = "Mus musculus")

  expect_gte(base::nrow(out$rows), 1)
  expect_true(base::all(out$rows$query_type == "symbol"))
  expect_true(base::all(out$rows$query_symbol == "H2AC11"))
})

test_that("the id classifier matches the client's rules", {
  expect_equal(
    classify_feature_id_type(c("ENSMUSG00000059552", "P02340", "22059", "Trp53", "", NA, "Q60590-2", "ensg00000141510")),
    c("ensembl", "uniprot", "entrez", "symbol", NA, NA, "uniprot", "ensembl")
  )
})
