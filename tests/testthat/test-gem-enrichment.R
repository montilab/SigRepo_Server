source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/msigdb_cache.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("../../api/lib/omic_signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/gem_enrichment.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("is_gem_test recognizes exactly the two GEM methods", {
  expect_true(is_gem_test("gem_hypergeo"))
  expect_true(is_gem_test("gem_weighted"))
  expect_false(is_gem_test("hypergeometric"))
  expect_false(is_gem_test("kstest"))
  expect_false(is_gem_test("gsea"))
})

test_that(".gem_method maps the UI's method names onto runHyperGEM's", {
  expect_equal(.gem_method("gem_weighted"), "weighted")
  expect_equal(.gem_method("gem_hypergeo"), "unweighted")
})

# signature2gene() matches species with match.arg() against capitalized names,
# which is case-sensitive -- so passing runHyperGEM()'s own "human" default
# through would error before any work happened.
test_that(".gem_species normalizes MSigDB binomials and lowercase names to the model's capitalization", {
  expect_equal(.gem_species("Homo sapiens"), "Human")
  expect_equal(.gem_species("human"), "Human")
  expect_equal(.gem_species("HUMAN"), "Human")
  expect_equal(.gem_species("Mus musculus"), "Mouse")
  expect_equal(.gem_species("mouse"), "Mouse")
  expect_equal(.gem_species("Rattus norvegicus"), "Rat")
  expect_equal(.gem_species("Danio rerio"), "Zebrafish")
  expect_equal(.gem_species("Caenorhabditis elegans"), "Worm")
  # Anything unrecognized falls back to the only model most repositories use,
  # rather than erroring inside match.arg() with an opaque message.
  expect_equal(.gem_species("Gallus gallus"), "Human")
  expect_equal(.gem_species(NULL), "Human")
})

test_that("gem_result_table unwraps a plain data.frame and filters on fdr", {
  df <- data.frame(
    label = c("A", "B", "C"),
    pval = c(1e-6, 1e-3, 0.4),
    fdr = c(1e-5, 0.02, 0.6),
    stringsAsFactors = FALSE
  )
  out <- gem_result_table(df, fdr = 0.05)
  expect_equal(length(out), 2)
  # Sorted by fdr ascending, so the strongest hit leads.
  expect_equal(out[[1]]$label, "A")
  expect_equal(out[[2]]$label, "B")
})

test_that("gem_result_table unwraps hypeR-style per-signature lists and labels each row", {
  nested <- list(data = list(
    "sig_up" = list(data = data.frame(label = "A", pval = 1e-6, fdr = 1e-5, stringsAsFactors = FALSE)),
    "sig_dn" = list(data = data.frame(label = "B", pval = 1e-4, fdr = 1e-3, stringsAsFactors = FALSE))
  ))
  out <- gem_result_table(nested, fdr = 0.05)
  expect_equal(length(out), 2)
  labels <- vapply(out, function(r) r$signature_label, character(1))
  expect_setequal(labels, c("sig_up", "sig_dn"))
})

test_that("gem_result_table returns an empty list rather than erroring on empty input", {
  expect_equal(gem_result_table(NULL), list())
  expect_equal(gem_result_table(list()), list())
  expect_equal(gem_result_table(data.frame()), list())
})

test_that("gem_count_metabolites counts distinct metabolites across group/direction splits", {
  sigs <- list(
    up = data.frame(refmet_name = c("a", "b"), stringsAsFactors = FALSE),
    dn = data.frame(refmet_name = c("b", "c"), stringsAsFactors = FALSE)
  )
  expect_equal(gem_count_metabolites(sigs), 3)
  expect_equal(gem_count_metabolites(list()), 0L)
  expect_equal(gem_count_metabolites(NULL), 0L)
})

test_that("gem_count_genes counts distinct genes across the model's gene tables", {
  obj <- list(gene_tables = list(
    up = data.frame(symbol = c("TP53", "MYC"), stringsAsFactors = FALSE),
    dn = data.frame(symbol = c("MYC", "EGFR"), stringsAsFactors = FALSE)
  ))
  expect_equal(gem_count_genes(obj), 3)
  expect_equal(gem_count_genes(list()), 0L)
})

test_that("run_gem_enrichment refuses non-metabolomics signatures instead of returning a meaningless result", {
  skip_if_no_test_db()
  testthat::skip_if_not(requireNamespace("hypeR.GEM", quietly = TRUE), "hypeR.GEM not installed")

  # The seeded CI signature is transcriptomics (tests/testthat/fixtures/seed.sql).
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  present <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'")$n[1]
  testthat::skip_if_not(present > 0, "CI fixture signature not present in this database")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  out <- run_gem_enrichment(
    auth = auth, signature_hashkey = "ci_test_signature_hashkey_0000", test = "gem_hypergeo",
    difexp_dir = tempdir(), msigdb_cache_dir = tempdir()
  )
  expect_false(out$ok)
  expect_equal(out$reason, "unsupported_assay_type")
  expect_match(out$message, "metabolomics", fixed = TRUE)
})

test_that("run_gem_enrichment reports a clear reason when the signature does not exist", {
  skip_if_no_test_db()
  testthat::skip_if_not(requireNamespace("hypeR.GEM", quietly = TRUE), "hypeR.GEM not installed")

  out <- run_gem_enrichment(
    auth = list(user_name = "ci_admin", user_role = "admin"), signature_hashkey = "definitely_not_a_real_hashkey",
    test = "gem_hypergeo", difexp_dir = tempdir(), msigdb_cache_dir = tempdir()
  )
  expect_false(out$ok)
  expect_equal(out$reason, "not_found")
})
