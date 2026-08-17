source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("collection_hash is a stable, case-insensitive md5", {
  testthat::skip_if_not_installed("digest")
  expect_equal(collection_hash("Foo", "bar"), collection_hash("foo", "BAR"))
  expect_equal(nchar(collection_hash("x", "y")), 32)
})

test_that("collection CRUD + membership works end-to-end, authorized against the api_key caller", {
  skip_if_no_test_db()

  # Short-lived connection per statement -- the functions under test open and
  # close their own connections, and RMySQL corrupts handles if another
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

  # ci_viewer/ci_admin already exist in the seed fixture; add an editor too.
  editor_name <- "collection_test_editor"
  exec_sql(sprintf("DELETE FROM users WHERE user_name = '%s'", editor_name))
  on.exit(exec_sql(sprintf("DELETE FROM users WHERE user_name = '%s'", editor_name)), add = TRUE)
  exec_sql(sprintf(
    "INSERT INTO users (user_name, user_password_hashkey, user_email, user_role, api_key, user_hashkey, active)
     VALUES ('%s','x','collection_test_editor@example.com','editor','collection_test_editor_key','collection_test_editor_hk',1)",
    editor_name
  ))

  viewer_auth <- list(user_name = "ci_viewer", user_role = "viewer")
  editor_auth <- list(user_name = editor_name, user_role = "editor")
  admin_auth <- list(user_name = "ci_admin", user_role = "admin")

  cleanup_hashkeys <- character()
  on.exit({
    for (hk in cleanup_hashkeys) {
      row <- query_sql(sprintf("SELECT collection_id FROM collection WHERE collection_hashkey = '%s'", hk))
      if (nrow(row) > 0) {
        cid <- row$collection_id[1]
        exec_sql(sprintf("DELETE FROM signature_collection_access WHERE collection_id = %d", cid))
        exec_sql(sprintf("DELETE FROM collection_access WHERE collection_id = %d", cid))
        exec_sql(sprintf("DELETE FROM collection WHERE collection_id = %d", cid))
      }
    }
  }, add = TRUE)

  # Viewer role cannot create collections.
  expect_equal(create_collection(viewer_auth, "Viewer Collection", "nope")$reason, "forbidden")

  # Editor can create a private collection.
  created <- create_collection(editor_auth, "Collection Test Private", "A private test collection", visibility = FALSE)
  expect_true(created$ok)
  cleanup_hashkeys <- c(cleanup_hashkeys, created$collection_hashkey)

  # Duplicate name for the same user is rejected.
  dup <- create_collection(editor_auth, "Collection Test Private", "again")
  expect_equal(dup$reason, "duplicate")

  # Empty name is rejected.
  expect_equal(create_collection(editor_auth, "  ", "desc")$reason, "invalid")

  # A public collection from admin, for the search-visibility assertions below.
  created_public <- create_collection(admin_auth, "Collection Test Public", "Public test collection", visibility = TRUE)
  expect_true(created_public$ok)
  cleanup_hashkeys <- c(cleanup_hashkeys, created_public$collection_hashkey)

  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # search_collections: the owner sees their own private collection.
  as_owner <- search_collections(conn, editor_auth, keyword = "Collection Test")
  expect_true("Collection Test Private" %in% as_owner$collection_name)
  expect_true("Collection Test Public" %in% as_owner$collection_name)

  # A different non-admin viewer sees only the public one.
  as_other <- search_collections(conn, viewer_auth, keyword = "Collection Test")
  expect_false("Collection Test Private" %in% as_other$collection_name)
  expect_true("Collection Test Public" %in% as_other$collection_name)

  # get_collection_detail: forbidden for a non-owner without access; ok for the owner.
  detail_forbidden <- get_collection_detail(viewer_auth, created$collection_hashkey)
  expect_equal(detail_forbidden$reason, "forbidden")

  detail_ok <- get_collection_detail(editor_auth, created$collection_hashkey)
  expect_true(detail_ok$ok)
  expect_equal(detail_ok$collection$collection_name, "Collection Test Private")
  expect_length(detail_ok$signatures, 0)

  expect_equal(get_collection_detail(editor_auth, "does-not-exist-hashkey")$reason, "not_found")

  # add_signature_to_collection: viewer lacks editor role -> forbidden.
  sig_hashkey <- "ci_test_signature_hashkey_0000"
  expect_equal(add_signature_to_collection(viewer_auth, created$collection_hashkey, sig_hashkey)$reason, "forbidden")

  # Editor owns the collection but not the seeded signature (ci_viewer does) -> forbidden.
  expect_equal(add_signature_to_collection(editor_auth, created$collection_hashkey, sig_hashkey)$reason, "forbidden")

  # Admin has access to everything.
  added <- add_signature_to_collection(admin_auth, created$collection_hashkey, sig_hashkey)
  expect_true(added$ok)
  expect_false(added$already_member)

  # Adding again is idempotent.
  added_again <- add_signature_to_collection(admin_auth, created$collection_hashkey, sig_hashkey)
  expect_true(added_again$ok)
  expect_true(added_again$already_member)

  detail_with_member <- get_collection_detail(admin_auth, created$collection_hashkey)
  expect_length(detail_with_member$signatures, 1)
  expect_equal(detail_with_member$signatures[[1]]$signature_hashkey, sig_hashkey)

  # remove_signature_from_collection: viewer forbidden, admin succeeds.
  expect_equal(remove_signature_from_collection(viewer_auth, created$collection_hashkey, sig_hashkey)$reason, "forbidden")
  removed <- remove_signature_from_collection(admin_auth, created$collection_hashkey, sig_hashkey)
  expect_true(removed$ok)

  detail_after_remove <- get_collection_detail(admin_auth, created$collection_hashkey)
  expect_length(detail_after_remove$signatures, 0)

  # delete_collection_by_hashkey: viewer forbidden, owner (editor) succeeds, then not_found.
  expect_equal(delete_collection_by_hashkey(viewer_auth, created$collection_hashkey)$reason, "forbidden")
  deleted <- delete_collection_by_hashkey(editor_auth, created$collection_hashkey)
  expect_true(deleted$ok)
  expect_equal(delete_collection_by_hashkey(editor_auth, created$collection_hashkey)$reason, "not_found")
  cleanup_hashkeys <- setdiff(cleanup_hashkeys, created$collection_hashkey)
})
