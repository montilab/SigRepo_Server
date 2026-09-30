# Guards the link between mysql/migrations/ and the schema_migrations table:
# a database built by generate_db_schema() must count as fully migrated, and
# the API's boot check must refuse one that is not.
source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/schema_migrations.R"), local = FALSE)
source(testthat::test_path("../../api/lib/database_admin.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

server_path <- function() {
  Sys.getenv("SIGREPO_SERVER_DIR", unset = testthat::test_path("../.."))
}

test_that("the shipped list is the forward migrations, in order, without rollbacks", {
  shipped <- shipped_schema_migrations(server_path())

  expect_gt(length(shipped), 0)
  expect_true("2026-09-25-rename-type-platform.sql" %in% shipped)
  expect_false(any(grepl("rollback", shipped)))
  expect_equal(shipped, sort(shipped))
})

test_that("a database built by generate_db_schema has nothing pending", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  generate_db_schema(server_path())

  conn <- db_connect_local()
  expect_equal(pending_schema_migrations(conn, server_path()), character(0))
  recorded <- DBI::dbGetQuery(conn, "SELECT name FROM schema_migrations")$name
  expect_setequal(recorded, shipped_schema_migrations(server_path()))
  expect_true(assert_schema_migrated(db_connect_local, server_path()))
})

test_that("recording twice neither fails nor duplicates", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  generate_db_schema(server_path())
  conn <- db_connect_local()
  record_schema_migrations(conn, server_path())

  n <- DBI::dbGetQuery(conn, "SELECT COUNT(*) AS n FROM schema_migrations")$n
  expect_equal(as.integer(n), length(shipped_schema_migrations(server_path())))
})

test_that("a missing record is pending, and the boot check names it", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  generate_db_schema(server_path())
  conn <- db_connect_local()
  DBI::dbGetQuery(conn, "DELETE FROM schema_migrations WHERE name = '2026-09-25-rename-type-platform.sql'")

  expect_equal(pending_schema_migrations(conn, server_path()), "2026-09-25-rename-type-platform.sql")
  expect_error(
    assert_schema_migrated(db_connect_local, server_path()),
    "2026-09-25-rename-type-platform.sql"
  )
  expect_error(assert_schema_migrated(db_connect_local, server_path()), "update_sigrepo.sh")
})

test_that("a database with tables but no schema_migrations table is entirely pending", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  generate_db_schema(server_path())
  conn <- db_connect_local()
  DBI::dbGetQuery(conn, "DROP TABLE schema_migrations")

  expect_setequal(pending_schema_migrations(conn, server_path()), shipped_schema_migrations(server_path()))
})

test_that("an empty database is not behind: /init_db has to be able to run", {
  skip_if_no_test_db()
  on.exit(reseed_test_db())

  reset_db_tables(conn_handler = NULL)

  expect_equal(pending_schema_migrations(db_connect_local(), server_path()), character(0))
  expect_true(assert_schema_migrated(db_connect_local, server_path()))
})

test_that("a database that cannot be reached is tolerated, with a message", {
  unreachable <- function() stop("Can't connect to MySQL server on 'sigrepo-mysql:3306' (111)")

  expect_message(
    result <- assert_schema_migrated(unreachable, server_path()),
    "Could not check schema migrations"
  )
  expect_false(result)
})
