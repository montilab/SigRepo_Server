# Guards the difexp gene-symbol fallback that over-representation enrichment
# depends on. Both halves of this were real production failures:
#   * only "gene_symbol" was recognised, but of 297 difexp tables on the
#     production repository 118 use "symbol" and just 39 use "gene_symbol";
#   * the hypergeometric branch had no difexp fallback at all, so 83 of 286
#     signatures -- 80 of them mouse, whose reference-table gene_symbol is
#     NULL -- failed with "could not be mapped to a gene symbol" even though
#     their difexp carried symbols.
source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)

test_that("difexp_symbol_column accepts the names depositors actually use", {
  # The alias that matters most in practice: `symbol` outnumbers `gene_symbol`
  # 3:1 in the real corpus.
  expect_equal(difexp_symbol_column(data.frame(symbol = c("Abat", "Sybu"))), "symbol")
  expect_equal(difexp_symbol_column(data.frame(gene_symbol = c("TP53", "MDM2"))), "gene_symbol")
  expect_equal(difexp_symbol_column(data.frame(geneSymbol = "TP53")), "geneSymbol")
  expect_equal(difexp_symbol_column(data.frame(mgi_symbol = "Abat")), "mgi_symbol")
  expect_equal(difexp_symbol_column(data.frame(hgnc_symbol = "TP53")), "hgnc_symbol")
})

test_that("difexp_symbol_column prefers gene_symbol when several are present", {
  # Deterministic ordering matters: a table carrying both must resolve the same
  # way every run, or two signatures with identical data could disagree.
  tbl <- data.frame(symbol = "FROM_SYMBOL", gene_symbol = "FROM_GENE_SYMBOL")
  expect_equal(difexp_symbol_column(tbl), "gene_symbol")
})

test_that("a present-but-empty column is not treated as usable", {
  # A column of NAs or blanks is the same as no column: falling for it would
  # produce an empty query instead of moving on to the next candidate.
  expect_null(difexp_symbol_column(data.frame(symbol = c(NA_character_, NA_character_))))
  expect_null(difexp_symbol_column(data.frame(symbol = c("", "   "))))
  expect_equal(
    difexp_symbol_column(data.frame(gene_symbol = c(NA_character_, ""), symbol = c("Abat", "Sybu"))),
    "symbol"
  )
})

test_that("difexp_symbol_column returns NULL when nothing carries symbols", {
  expect_null(difexp_symbol_column(data.frame(probe_id = "p1", score = 1)))
  expect_null(difexp_symbol_column(data.frame()))
})

test_that("the accepted names match rummagene.R's list exactly", {
  # These two features answer the same question -- "does this signature have
  # usable gene symbols?" -- and disagreeing produced exactly the reported bug:
  # Rummagene worked on a signature where the annotate page said the features
  # could not be mapped.
  rummagene_src <- readLines(testthat::test_path("../../api/lib/rummagene.R"), warn = FALSE)
  line <- grep("sym_candidates <- c\\(", rummagene_src, value = TRUE)
  expect_length(line, 1)

  names_in_rummagene <- regmatches(line, gregexpr('"[^"]+"', line))[[1]]
  names_in_rummagene <- gsub('"', "", names_in_rummagene)
  expect_setequal(names_in_rummagene, DIFEXP_SYMBOL_COLUMNS)
})
