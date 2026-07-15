source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("feature_ids_from_context extracts probe_id or feature_id and dedupes", {
  context <- list(features = list(
    list(probe_id = "p1", feature_id = NA),
    list(probe_id = "p1", feature_id = NA),
    list(probe_id = NA, feature_id = 42)
  ))
  expect_equal(sort(feature_ids_from_context(context)), sort(c("p1", "42")))
  expect_equal(feature_ids_from_context(list(features = list())), character())
})

test_that("signature_similarity_summary computes Jaccard similarity between pairs", {
  contexts <- list(
    sig_a = list(features = list(list(probe_id = "1"), list(probe_id = "2"))),
    sig_b = list(features = list(list(probe_id = "2"), list(probe_id = "3")))
  )
  similarity <- signature_similarity_summary(contexts)
  expect_equal(nrow(similarity), 1)
  expect_equal(similarity$shared_features, 1)
  expect_equal(similarity$jaccard_similarity, 1 / 3)

  expect_equal(nrow(signature_similarity_summary(list(only_one = list()))), 0)
})

test_that("draft_signature_groups merges signatures above the similarity threshold", {
  similarity_tbl <- data.frame(
    signature_hashkey_1 = c("a", "b"),
    signature_hashkey_2 = c("b", "c"),
    jaccard_similarity = c(0.5, 0.05),
    stringsAsFactors = FALSE
  )

  groups <- draft_signature_groups(similarity_tbl, threshold = 0.10)
  expect_length(groups, 2)
  expect_true(any(vapply(groups, function(g) setequal(g, c("a", "b")), logical(1))))
  expect_true(any(vapply(groups, function(g) identical(g, "c"), logical(1))))

  expect_equal(draft_signature_groups(data.frame()), list())
})

test_that("fetch_signature_context joins reference tables and returns features for a seeded signature", {
  skip_if_no_test_db()

  context <- fetch_signature_context(
    "ci_test_signature_hashkey_0000",
    include_features = TRUE,
    max_features = 50,
    auth = list(user_role = "admin", user_name = "ci_viewer")
  )

  expect_equal(context$signature$organism, "CI Test Organism")
  expect_equal(context$signature$phenotype, "CI Test Phenotype")
  expect_equal(context$feature_count, 2)
  expect_length(context$features, 2)
})

test_that("fetch_signature_context returns NULL for a signature that does not exist", {
  skip_if_no_test_db()
  expect_null(fetch_signature_context("does-not-exist-hashkey"))
})

test_that("search_signatures filters by organism/keyword and hides invisible rows from non-admins", {
  skip_if_no_test_db()
  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  by_organism <- search_signatures(conn, organism = "CI Test Organism", is_admin = TRUE)
  expect_true("CI Test Signature" %in% by_organism$signature_name)

  by_keyword <- search_signatures(conn, keyword = "CI Test Signature", is_admin = TRUE)
  expect_true(nrow(by_keyword) >= 1)
  expect_true(all(grepl("CI Test", by_keyword$signature_name, fixed = TRUE)))

  none <- search_signatures(conn, organism = "No Such Organism", is_admin = TRUE)
  expect_equal(nrow(none), 0)

  # The seeded signature is visibility = 1 (public) per fixtures/seed.sql, so
  # it should also surface for a non-admin search -- this only proves the
  # is_admin flag doesn't wrongly hide visible rows, not that hidden rows are
  # filtered (seed.sql has no hidden signature to assert against).
  as_viewer <- search_signatures(conn, organism = "CI Test Organism", is_admin = FALSE)
  expect_true("CI Test Signature" %in% as_viewer$signature_name)
})

test_that("delete_signature enforces caller role/ownership and removes child rows", {
  skip_if_no_test_db()

  # Short-lived connection per statement -- delete_signature() opens/closes
  # its own connection internally, and RMySQL corrupts handles if another
  # connection is held open across those calls (see test-login-vocabulary.R).
  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  query_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    DBI::dbGetQuery(conn, stmt)
  }
  make_test_signature <- function(hashkey, owner) {
    exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey = '%s'", hashkey))
    exec_sql(sprintf("
      INSERT INTO signatures (signature_name, organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id, user_name, visibility, signature_hashkey)
      SELECT 'Delete Test Signature', organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id, '%s', 1, '%s'
      FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'
    ", owner, hashkey))
    query_sql(sprintf("SELECT signature_id FROM signatures WHERE signature_hashkey = '%s'", hashkey))$signature_id[1]
  }

  hashkey_1 <- "slice3_delete_test_hashkey_001"
  signature_id_1 <- make_test_signature(hashkey_1, "ci_viewer")
  on.exit(exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey = '%s'", hashkey_1)), add = TRUE)

  exec_sql(sprintf(
    "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, score, assay_type, sig_feature_hashkey)
     VALUES (%d, 1, 'probe_test_1', 1.0, 'transcriptomics', 'slice3_feature_hashkey_001')",
    signature_id_1
  ))

  viewer_auth <- list(user_name = "ci_viewer", user_role = "viewer")
  non_owner_editor_auth <- list(user_name = "ci_admin", user_role = "editor")
  owner_editor_auth <- list(user_name = "ci_viewer", user_role = "editor")

  # Viewer role can never delete, even as the owner.
  expect_equal(delete_signature(viewer_auth, hashkey_1)$reason, "forbidden")

  # Editor role, but neither the owner nor granted access -- forbidden.
  expect_equal(delete_signature(non_owner_editor_auth, hashkey_1)$reason, "forbidden")

  # Unknown signature.
  expect_equal(delete_signature(owner_editor_auth, "does-not-exist-hashkey")$reason, "not_found")

  # Owner with editor role can delete their own signature; child rows go too.
  result <- delete_signature(owner_editor_auth, hashkey_1)
  expect_true(result$ok)
  expect_equal(nrow(query_sql(sprintf("SELECT * FROM signatures WHERE signature_hashkey = '%s'", hashkey_1))), 0)
  expect_equal(nrow(query_sql(sprintf("SELECT * FROM signature_feature_set WHERE signature_id = %d", signature_id_1))), 0)

  # Admin can delete a signature they don't own, without an access grant.
  hashkey_2 <- "slice3_delete_test_hashkey_002"
  make_test_signature(hashkey_2, "ci_viewer")
  on.exit(exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey = '%s'", hashkey_2)), add = TRUE)

  admin_auth <- list(user_name = "ci_admin", user_role = "admin")
  expect_true(delete_signature(admin_auth, hashkey_2)$ok)
})
