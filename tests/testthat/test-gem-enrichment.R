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

# The checkout's own gene-set cache (see test-annotate.R), reused here so a
# real run_gem_enrichment() call can get past resolve_msigdb_genesets()
# without a network fetch.
real_msigdb_cache_dir <- testthat::test_path("../../data/msigdb_genesets")

# Temporarily overrides a global binding, returning a function that restores
# whatever was there before (or removes it, if it did not exist). Called with
# on.exit(restore(), add = TRUE) directly inside the test that needs it, so
# the restore runs in that test's own frame rather than this helper's.
stash_global <- function(name) {
  existed <- base::exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) base::get(name, envir = globalenv()) else NULL
  function() {
    if (existed) {
      base::assign(name, old, envir = globalenv())
    } else if (base::exists(name, envir = globalenv(), inherits = FALSE)) {
      base::rm(list = name, envir = globalenv())
    }
  }
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

# On production, EVERY metabolomics signature has has_difexp = 1, and
# production's installed SigRepo client is the 2-arg createOmicSignature()
# (no difexp/fetch_difexp arguments) -- so before this fix, run_gem_enrichment
# refused all of them with reason = "signature_build_failed". This test
# forces that exact client shape (see .omic_signature_supports_difexp above)
# regardless of what is actually installed here, so it fails under the old
# code no matter which client this checkout happens to have loaded.
test_that("run_gem_enrichment does not load a difexp, and does not refuse a has_difexp = 1 signature under a 2-arg client", {
  skip_if_no_test_db()
  testthat::skip_if_not(requireNamespace("hypeR.GEM", quietly = TRUE), "hypeR.GEM not installed")
  testthat::skip_if_not(dir.exists(real_msigdb_cache_dir), "repo's msigdb cache is not present in this checkout")

  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  hashkey <- "ci_gem_difexp_test_hashkey_0001"
  cleanup <- function() {
    # No signature_feature_set row is ever inserted for this fixture (the
    # stubbed build_omic_signature() below short-circuits before that table
    # would be queried) -- this is just defensive symmetry, and a no-op
    # subquery against a different table needs no self-reference workaround.
    suppressWarnings(DBI::dbExecute(conn, base::sprintf(
      "DELETE FROM signature_feature_set WHERE signature_id IN (SELECT signature_id FROM signatures WHERE signature_hashkey = '%s')",
      hashkey
    )))
    suppressWarnings(DBI::dbExecute(conn, base::sprintf("DELETE FROM signatures WHERE signature_hashkey = '%s'", hashkey)))
  }
  cleanup()
  on.exit(cleanup(), add = TRUE)

  DBI::dbExecute(conn, base::sprintf("
    INSERT INTO signatures
      (signature_name, organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id,
       user_name, visibility, has_difexp, signature_hashkey)
    SELECT
      'CI GEM Difexp Test', (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'),
      'uni-directional', 'metabolomics',
      (SELECT phenotype_id FROM phenotypes WHERE phenotype = 'CI Test Phenotype'),
      (SELECT platform_id FROM platforms WHERE platform_name = 'CI Test Platform'),
      (SELECT sample_type_id FROM sample_types WHERE sample_type = 'CI Test Sample Type'),
      'ci_viewer', 1, 1, '%s'", hashkey
  ))

  # Force the 2-arg (\"production\") client shape for this call, regardless of
  # what this test environment actually has installed.
  restore_supports <- stash_global(".omic_signature_supports_difexp")
  on.exit(restore_supports(), add = TRUE)
  assign(".omic_signature_supports_difexp", function() FALSE, envir = globalenv())

  # Proves no difexp is ever read from disk for GEM: the old code loaded one
  # whenever has_difexp = 1, which this fixture is, and this stub would fail
  # the test outright if that still happened.
  restore_load_difexp <- stash_global("load_difexp_rds")
  on.exit(restore_load_difexp(), add = TRUE)
  assign("load_difexp_rds", function(...) base::stop("load_difexp_rds() should never be called for GEM"), envir = globalenv())

  # Stubs build_omic_signature itself so this test is about
  # run_gem_enrichment()'s own wiring -- does it still ask for a difexp? does
  # it tell build_omic_signature to skip the guard? -- rather than about
  # createOmicSignature()'s behaviour under either client shape, which
  # test-omic-signature.R already covers directly.
  seen_args <- NULL
  restore_build <- stash_global("build_omic_signature")
  on.exit(restore_build(), add = TRUE)
  assign("build_omic_signature", function(db_row, difexp = NULL, require_difexp = TRUE) {
    seen_args <<- base::list(difexp = difexp, require_difexp = require_difexp)
    base::list(signature = data.frame(probe_id = character(0), refmet_name = character(0), stringsAsFactors = FALSE))
  }, envir = globalenv())

  auth <- list(user_name = "ci_admin", user_role = "admin")
  out <- run_gem_enrichment(
    auth = auth, signature_hashkey = hashkey, test = "gem_hypergeo",
    difexp_dir = tempdir(), msigdb_cache_dir = real_msigdb_cache_dir
  )

  expect_false(base::is.null(seen_args))
  expect_null(seen_args$difexp)
  expect_false(seen_args$require_difexp)

  # The stubbed signature has no metabolites, so this is exactly as far as a
  # has_difexp = 1 metabolomics signature should get here: past the difexp
  # guard (which used to fail with "signature_build_failed"), stopping
  # instead at the next real check down the pipeline.
  expect_false(out$ok)
  expect_equal(out$reason, "no_reference_values")
})
