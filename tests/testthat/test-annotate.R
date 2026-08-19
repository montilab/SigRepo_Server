source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/msigdb_cache.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

skip_if_offline <- function() {
  testthat::skip_if_not_installed("curl")
  testthat::skip_if_not(curl::has_internet(), "no network access (required to fetch MSigDB gene sets)")
}

local_tempdir <- function() {
  dir <- tempfile("sigrepo-annotate-test-")
  dir.create(dir)
  dir
}

# The checkout's own gene-set cache (data/msigdb_genesets). It is not committed
# -- it is built on demand by /init_db_genesets -- so the tests below skip when
# it is absent. When it is there, using it directly means run_enrichment can be
# tested without hitting the network, same as the API does.
real_msigdb_cache_dir <- testthat::test_path("../../data/msigdb_genesets")

test_that("enrichment_reference_table maps assay_type to the right features table", {
  expect_equal(enrichment_reference_table("transcriptomics"), "transcriptomics_features")
  expect_equal(enrichment_reference_table("proteomics"), "proteomics_features")
  expect_null(enrichment_reference_table("metabolomics"))
  expect_null(enrichment_reference_table("snps"))
})

test_that("resolve_single_enrichment_query resolves hypergeometric queries to real gene symbols", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }

  # The seeded signature's two features (feature_id 1/2, see seed.sql) have
  # no matching transcriptomics_features rows -- add them here so the
  # gene_symbol join has something real to resolve.
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- resolve_single_enrichment_query(auth, "ci_test_signature_hashkey_0000", "hypergeometric", difexp_dir = tempdir())

  expect_true(result$ok)
  expect_setequal(result$query, c("TP53", "BRCA1"))
  expect_equal(result$signature_name, "CI Test Signature")
})

test_that("resolve_single_enrichment_query reports no_gene_symbols when features don't map to any", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- resolve_single_enrichment_query(auth, "ci_test_signature_hashkey_0000", "hypergeometric", difexp_dir = tempdir())

  expect_false(result$ok)
  expect_equal(result$reason, "no_gene_symbols")
})

test_that("resolve_single_enrichment_query reports not_found for an unknown signature", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- resolve_single_enrichment_query(auth, "does-not-exist-hashkey", "hypergeometric", difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "not_found")
})

test_that("resolve_single_enrichment_query requires difexp for kstest, and resolves it correctly when present", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")

  # has_difexp = 0 for the seeded signature -> no_difexp, regardless of files on disk.
  no_difexp_result <- resolve_single_enrichment_query(auth, "ci_test_signature_hashkey_0000", "kstest", difexp_dir = tempdir())
  expect_false(no_difexp_result$ok)
  expect_equal(no_difexp_result$reason, "no_difexp")

  exec_sql("UPDATE signatures SET has_difexp = 1 WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'")
  on.exit(exec_sql("UPDATE signatures SET has_difexp = 0 WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'"), add = TRUE)

  difexp_dir <- local_tempdir()
  difexp_tbl <- data.frame(
    probe_id = c("probe_1", "probe_2", "probe_unmapped"),
    score = c(2.5, -1.2, 9.9),
    stringsAsFactors = FALSE
  )
  saveRDS(difexp_tbl, file.path(difexp_dir, "ci_test_signature_hashkey_0000.RDS"))

  kstest_result <- resolve_single_enrichment_query(auth, "ci_test_signature_hashkey_0000", "kstest", difexp_dir = difexp_dir)
  expect_true(kstest_result$ok)
  expect_setequal(names(kstest_result$query), c("TP53", "BRCA1"))
  expect_equal(unname(kstest_result$query["TP53"]), 2.5)
  expect_equal(unname(kstest_result$query["BRCA1"]), -1.2)
})

test_that("resolve_single_enrichment_query's kstest uses difexp's own gene_symbol/feature_name columns, not just the curated feature set", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2, 3, 4)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2, 3, 4)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 3, 'ENSG_TEST_3', organism_id, 'MYC', 1, 'annotate_test_feature_hashkey_03' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 4, 'ENSG_TEST_4', organism_id, 'EGFR', 1, 'annotate_test_feature_hashkey_04' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  exec_sql("UPDATE signatures SET has_difexp = 1 WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'")
  on.exit(exec_sql("UPDATE signatures SET has_difexp = 0 WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'"), add = TRUE)

  # A realistic difexp table: far more rows than the signature's curated
  # 2-feature set (probe_1/probe_2 only), with its own gene_symbol column --
  # every row here must be usable, not just the two that also happen to be
  # curated features.
  difexp_dir <- local_tempdir()
  difexp_with_symbol <- data.frame(
    probe_id = c("probe_1", "probe_2", "probe_3", "probe_4"),
    gene_symbol = c("TP53", "BRCA1", "MYC", "EGFR"),
    score = c(2.5, -1.2, 4.4, -3.3),
    stringsAsFactors = FALSE
  )
  saveRDS(difexp_with_symbol, file.path(difexp_dir, "ci_test_signature_hashkey_0000.RDS"))

  result_via_symbol <- resolve_single_enrichment_query(auth, "ci_test_signature_hashkey_0000", "kstest", difexp_dir = difexp_dir)
  expect_true(result_via_symbol$ok)
  expect_setequal(names(result_via_symbol$query), c("TP53", "BRCA1", "MYC", "EGFR"))
  expect_equal(unname(result_via_symbol$query["MYC"]), 4.4)

  # Same shape, but no gene_symbol column -- resolved via feature_name
  # against the reference table instead.
  difexp_via_feature_name <- data.frame(
    probe_id = c("probe_1", "probe_2", "probe_3", "probe_4"),
    feature_name = c("ENSG_TEST_1", "ENSG_TEST_2", "ENSG_TEST_3", "ENSG_TEST_4"),
    score = c(2.5, -1.2, 4.4, -3.3),
    stringsAsFactors = FALSE
  )
  saveRDS(difexp_via_feature_name, file.path(difexp_dir, "ci_test_signature_hashkey_0000.RDS"))

  result_via_feature_name <- resolve_single_enrichment_query(auth, "ci_test_signature_hashkey_0000", "kstest", difexp_dir = difexp_dir)
  expect_true(result_via_feature_name$ok)
  expect_setequal(names(result_via_feature_name$query), c("TP53", "BRCA1", "MYC", "EGFR"))
  expect_equal(unname(result_via_feature_name$query["EGFR"]), -3.3)
})

test_that("msigdb_species_options lists real species including Homo sapiens", {
  species <- msigdb_species_options()
  expect_true(is.character(species))
  expect_true("Homo sapiens" %in% species)
})

test_that("msigdb_collection_metadata lists the real Collection/Subcollection matrix", {
  meta <- msigdb_collection_metadata()
  expect_equal(nrow(meta), 25)
  expect_true(all(c("collection", "collection_label", "subcollection") %in% colnames(meta)))
  expect_true("H" %in% meta$collection)
  expect_equal(meta$collection_label[meta$collection == "H"][1], "Hallmark (H)")
})

test_that("msigdb_slugify normalizes names into cache-file-safe slugs", {
  expect_equal(msigdb_slugify("Homo sapiens"), "Homo_sapiens")
  expect_equal(msigdb_slugify("CP:KEGG_LEGACY"), "CP_KEGG_LEGACY")
})

test_that("load_cached_msigdb_genesets loads the real Hallmark cache and returns NULL for a miss", {
  testthat::skip_if_not(dir.exists(real_msigdb_cache_dir), "repo's msigdb cache is not present in this checkout")

  gs <- load_cached_msigdb_genesets(real_msigdb_cache_dir, "Homo sapiens", "H")
  expect_type(gs, "list")
  expect_true(length(gs) > 0)
  expect_true("HALLMARK_ADIPOGENESIS" %in% names(gs))

  expect_null(load_cached_msigdb_genesets(real_msigdb_cache_dir, "Homo sapiens", "NOT_A_REAL_COLLECTION"))
})

test_that("resolve_msigdb_genesets resolves from cache and reports not_cached when disabled and missing", {
  testthat::skip_if_not(dir.exists(real_msigdb_cache_dir), "repo's msigdb cache is not present in this checkout")
  withr_env <- Sys.getenv("MSIGDB_ALLOW_RUNTIME_FETCH", unset = NA)
  Sys.setenv(MSIGDB_ALLOW_RUNTIME_FETCH = "false")
  on.exit({
    if (is.na(withr_env)) Sys.unsetenv("MSIGDB_ALLOW_RUNTIME_FETCH") else Sys.setenv(MSIGDB_ALLOW_RUNTIME_FETCH = withr_env)
  }, add = TRUE)

  cached <- resolve_msigdb_genesets(real_msigdb_cache_dir, "Homo sapiens", "H")
  expect_true(cached$ok)
  expect_equal(cached$source, "cache")
  expect_true(length(cached$genesets) > 0)

  missing <- resolve_msigdb_genesets(real_msigdb_cache_dir, "Homo sapiens", "NOT_A_REAL_COLLECTION")
  expect_false(missing$ok)
  expect_equal(missing$reason, "not_cached")
})

test_that("run_enrichment computes real hypergeometric enrichment from the on-disk MSigDB cache", {
  skip_if_no_test_db()
  testthat::skip_if_not(dir.exists(real_msigdb_cache_dir), "repo's msigdb cache is not present in this checkout")

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")
  result <- run_enrichment(
    auth, "ci_test_signature_hashkey_0000", test = "hypergeometric",
    species = "Homo sapiens", collection = "H", fdr = 1,
    difexp_dir = tempdir(), msigdb_cache_dir = real_msigdb_cache_dir
  )

  expect_true(result$ok)
  expect_equal(length(result$resolved), 1)
  expect_equal(result$resolved[[1]]$n_query, 2)
  expect_equal(result$resolved[[1]]$signature_name, "CI Test Signature")
  expect_equal(length(result$skipped), 0)
  expect_equal(result$geneset_source, "cache")
  expect_true(is.data.frame(result$results))
  expect_true(all(c("label", "pval", "fdr", "overlap", "hits", "signature_label") %in% colnames(result$results)))
  expect_true(all(result$results$signature_label == "CI Test Signature"))
  expect_true(nrow(result$results) > 0)
  expect_true(is.character(result$dotplot_png))
  expect_true(startsWith(result$dotplot_png, "data:image/png;base64,"))
})

test_that("run_enrichment runs multiple signatures at once and skips ones that can't resolve", {
  skip_if_no_test_db()
  testthat::skip_if_not(dir.exists(real_msigdb_cache_dir), "repo's msigdb cache is not present in this checkout")

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  # A second signature that happens to share the first's signature_name (two
  # different users can both name a signature "CI Test Signature") -- the
  # realistic case resolve_enrichment_queries()'s label-disambiguation
  # exists for, since signature_hashkey is unique but signature_name isn't.
  exec_sql("
    INSERT INTO signatures
      (signature_name, organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id, user_name, visibility, signature_hashkey)
    SELECT signature_name, organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id, 'ci_admin', 1, 'ci_test_signature_hashkey_dup1'
    FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'
  ")
  on.exit({
    exec_sql("DELETE FROM signature_feature_set WHERE signature_id = (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_dup1')")
    exec_sql("DELETE FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_dup1'")
  }, add = TRUE)
  exec_sql("
    INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
    SELECT (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_dup1'),
           feature_id, probe_id, score, group_label, assay_type, MD5(CONCAT(sig_feature_hashkey, '_dup'))
    FROM signature_feature_set
    WHERE signature_id = (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000')
  ")

  auth <- list(user_name = "ci_admin", user_role = "admin")

  # Two distinct signatures sharing one signature_name (exercises the
  # duplicate-label disambiguation), plus one unresolvable hashkey
  # (exercises skip-on-partial-failure).
  result <- run_enrichment(
    auth, c("ci_test_signature_hashkey_0000", "ci_test_signature_hashkey_dup1", "does-not-exist-hashkey"),
    test = "hypergeometric", species = "Homo sapiens", collection = "H", fdr = 1,
    difexp_dir = tempdir(), msigdb_cache_dir = real_msigdb_cache_dir
  )

  expect_true(result$ok)
  expect_equal(length(result$resolved), 2)
  expect_equal(result$resolved[[1]]$label, "CI Test Signature")
  expect_equal(result$resolved[[2]]$label, "CI Test Signature (2)")
  expect_equal(length(result$skipped), 1)
  expect_equal(result$skipped[[1]]$signature_hashkey, "does-not-exist-hashkey")
  expect_equal(result$skipped[[1]]$reason, "not_found")
  expect_setequal(unique(result$results$signature_label), c("CI Test Signature", "CI Test Signature (2)"))
  expect_true(is.character(result$dotplot_png))
  expect_true(startsWith(result$dotplot_png, "data:image/png;base64,"))
})

test_that("run_enrichment fails only when every requested signature is unresolvable", {
  auth <- list(user_name = "ci_admin", user_role = "admin")

  empty_result <- run_enrichment(
    auth, character(), test = "hypergeometric",
    difexp_dir = tempdir(), msigdb_cache_dir = real_msigdb_cache_dir
  )
  expect_false(empty_result$ok)
  expect_equal(empty_result$reason, "no_signatures")

  skip_if_no_test_db()
  all_fail_result <- run_enrichment(
    auth, c("does-not-exist-1", "does-not-exist-2"), test = "hypergeometric",
    difexp_dir = tempdir(), msigdb_cache_dir = real_msigdb_cache_dir
  )
  expect_false(all_fail_result$ok)
  expect_equal(all_fail_result$reason, "not_found")
  expect_equal(length(all_fail_result$skipped), 2)
})

test_that("run_enrichment falls back to a live MSigDB fetch when nothing is cached and it's allowed", {
  skip_if_no_test_db()
  skip_if_offline()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  on.exit(exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)"), add = TRUE)
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, 'annotate_test_feature_hashkey_01' FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, 'annotate_test_feature_hashkey_02' FROM organisms WHERE organism = 'CI Test Organism'
  ")

  withr_env <- Sys.getenv("MSIGDB_ALLOW_RUNTIME_FETCH", unset = NA)
  Sys.setenv(MSIGDB_ALLOW_RUNTIME_FETCH = "true")
  on.exit({
    if (is.na(withr_env)) Sys.unsetenv("MSIGDB_ALLOW_RUNTIME_FETCH") else Sys.setenv(MSIGDB_ALLOW_RUNTIME_FETCH = withr_env)
  }, add = TRUE)

  auth <- list(user_name = "ci_admin", user_role = "admin")
  # An empty, definitely-uncached directory forces the live-fetch path.
  result <- run_enrichment(
    auth, "ci_test_signature_hashkey_0000", test = "hypergeometric",
    species = "Homo sapiens", collection = "H", fdr = 1,
    difexp_dir = tempdir(), msigdb_cache_dir = local_tempdir()
  )

  expect_true(result$ok)
  expect_equal(result$geneset_source, "live")
  expect_true(nrow(result$results) > 0)
})
