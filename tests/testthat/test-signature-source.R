# Sourced explicitly, as every other test file in this directory does. Under
# test_dir() each file runs in its own environment and the order in which files
# run is not something this file should depend on -- relying on an earlier test
# having sourced these made this file pass alone and fail in the suite, calling
# whichever definition happened to be in scope.
source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

# signatures.signature_source -- where a signature came from.
#
# The tests that matter most here are the DRIFT tests. This repo has no
# migration mechanism: generate_db_schema() DROPs and recreates every table, so
# it can only be run against a disposable database, and any database with real
# data in it is changed by hand through mysql/alter/*.sql. That means a column
# can exist in three places that are free to disagree --
#
#   mysql/schema/signatures.sql   what a FRESH database gets
#   mysql/alter/*.sql             what an EXISTING database gets
#   the running database          what the code actually talks to
#
# -- and nothing but these tests notices when they diverge. A column added to
# the schema file alone works perfectly in development and fails in production;
# that is exactly the failure this file exists to catch.

repo_file <- function(...) testthat::test_path("..", "..", ...)

test_that("the schema file declares signature_source", {
  sql <- base::readLines(repo_file("mysql", "schema", "signatures.sql"), warn = FALSE)
  decl <- base::grep("signature_source", sql, value = TRUE)
  testthat::expect_true(base::length(decl) > 0)
  # NOT NULL DEFAULT 'curated' is what makes every pre-existing row and every
  # caller that does not mention the column correct without further action. If
  # the default is ever dropped, upload starts writing NULL and the UI's
  # provenance claim quietly becomes "unknown" for new signatures.
  decl_line <- decl[base::grepl("VARCHAR", decl, ignore.case = TRUE)]
  testthat::expect_length(decl_line, 1)
  testthat::expect_match(decl_line, "NOT NULL", fixed = TRUE)
  testthat::expect_match(decl_line, "DEFAULT 'curated'", fixed = TRUE)
})

test_that("an alter file exists to add signature_source to an existing database", {
  alters <- base::list.files(repo_file("mysql", "alter"), pattern = "\\.sql$", full.names = TRUE)
  testthat::expect_true(base::length(alters) > 0)
  bodies <- base::vapply(alters, function(f) base::paste(base::readLines(f, warn = FALSE), collapse = "\n"), "")
  adding <- bodies[base::grepl("ADD COLUMN `signature_source`", bodies, fixed = TRUE)]
  testthat::expect_length(adding, 1)

  # Guarded on information_schema, so applying it twice is not an error. An
  # operator who is unsure whether it has already run must be able to just run
  # it, rather than having to inspect the database to find out.
  testthat::expect_match(adding, "information_schema", fixed = TRUE)

  # BOTH provenance formats. The first Rummagene pulls wrote "source=rummagene"
  # and the corrected version writes "source: rummagene"; a backfill matching
  # only one silently labels the other rows "curated", which is precisely the
  # claim this column exists to make trustworthy.
  testthat::expect_match(adding, "source: rummagene", fixed = TRUE)
  testthat::expect_match(adding, "source=rummagene", fixed = TRUE)
})

test_that("the running database has signature_source, matching the schema file", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  col <- DBI::dbGetQuery(conn, "
    SELECT COLUMN_NAME, IS_NULLABLE, COLUMN_DEFAULT, DATA_TYPE
    FROM information_schema.COLUMNS
    WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME = 'signatures'
      AND COLUMN_NAME = 'signature_source'")
  # Failing here means the alter has not been applied to THIS database -- the
  # code below will be writing to a column that does not exist.
  testthat::expect_equal(base::nrow(col), 1)
  testthat::expect_equal(col$IS_NULLABLE[1], "NO")
  testthat::expect_equal(col$COLUMN_DEFAULT[1], "curated")
})

test_that("every existing row has a source, and pulled rows are labelled rummagene", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # No row may be NULL or empty: a blank source would render as an empty column
  # in the UI, which reads as "no data" rather than "curated".
  blank <- DBI::dbGetQuery(conn, "
    SELECT COUNT(*) AS n FROM signatures
    WHERE signature_source IS NULL OR signature_source = ''")$n[1]
  testthat::expect_equal(base::as.integer(blank), 0L)

  # The backfill is a claim about existing rows, so check it against the
  # evidence rather than against a remembered number: every row whose
  # provenance string says rummagene must be labelled rummagene, and no row
  # without one may be.
  mismatched <- DBI::dbGetQuery(conn, "
    SELECT COUNT(*) AS n FROM signatures
    WHERE (others LIKE '%source: rummagene%' OR others LIKE '%source=rummagene%')
      AND signature_source <> 'rummagene'")$n[1]
  testthat::expect_equal(base::as.integer(mismatched), 0L)

  overreach <- DBI::dbGetQuery(conn, "
    SELECT COUNT(*) AS n FROM signatures
    WHERE signature_source = 'rummagene'
      AND others NOT LIKE '%source: rummagene%'
      AND others NOT LIKE '%source=rummagene%'")$n[1]
  testthat::expect_equal(base::as.integer(overreach), 0L)
})

test_that("search_signatures returns signature_source on every row", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  result <- search_signatures(conn, limit = 100, is_admin = TRUE)
  testthat::skip_if(base::nrow(result$rows) == 0, "no signatures to inspect")
  testthat::expect_true("signature_source" %in% base::names(result$rows))
  testthat::expect_true(base::all(base::nzchar(result$rows$signature_source)))
})

test_that("search_signatures filters to one source", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  all_rows <- search_signatures(conn, limit = 100, is_admin = TRUE)
  testthat::skip_if(base::nrow(all_rows$rows) == 0, "no signatures to inspect")

  for (src in base::unique(all_rows$rows$signature_source)) {
    filtered <- search_signatures(conn, limit = 100, is_admin = TRUE, signature_source = src)
    testthat::expect_true(base::all(filtered$rows$signature_source == src))
    # `total` is used for pagination, so it has to reflect the FILTER, not the
    # unfiltered table -- otherwise the UI offers pages that come back empty.
    testthat::expect_equal(
      base::as.integer(filtered$total),
      base::sum(all_rows$rows$signature_source == src)
    )
  }
})

test_that("an unknown source matches nothing rather than everything", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # A filter that silently does nothing when it does not recognise its argument
  # is worse than one that returns nothing: the caller sees a full list and
  # believes it was filtered.
  result <- search_signatures(conn, limit = 100, is_admin = TRUE,
                              signature_source = "no-such-source")
  testthat::expect_equal(base::nrow(result$rows), 0L)
  testthat::expect_equal(base::as.integer(result$total), 0L)
})

test_that("an empty source filter is treated as no filter", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  # The route passes "" when the caller omits the parameter, so "" must mean
  # "all sources" -- matching how organism/phenotype/assay_type already behave.
  unfiltered <- search_signatures(conn, limit = 100, is_admin = TRUE)
  for (empty in base::list("", "   ", NULL)) {
    result <- search_signatures(conn, limit = 100, is_admin = TRUE, signature_source = empty)
    testthat::expect_equal(base::as.integer(result$total), base::as.integer(unfiltered$total))
  }
})

test_that("signature_source is an accepted sort column", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  asc <- search_signatures(conn, limit = 100, is_admin = TRUE,
                           sort_by = "signature_source", sort_dir = "asc")
  testthat::skip_if(base::nrow(asc$rows) < 2, "need at least two rows to check ordering")
  testthat::expect_false(base::is.unsorted(asc$rows$signature_source))

  desc <- search_signatures(conn, limit = 100, is_admin = TRUE,
                            sort_by = "signature_source", sort_dir = "desc")
  testthat::expect_false(base::is.unsorted(base::rev(desc$rows$signature_source)))
})

test_that("the upload path defaults to curated and does not read the payload", {
  # signature_source is a claim about provenance, so it must come from the code
  # path that knows it -- not from whoever composed the upload. If it were read
  # out of `uploaded`, a person could label their own submission "rummagene"
  # and the UI would vouch for it.
  fn <- base::formals(build_signature_from_upload)
  testthat::expect_true("signature_source" %in% base::names(fn))
  testthat::expect_equal(base::eval(fn$signature_source), "curated")

  body_src <- base::paste(base::deparse(base::body(build_signature_from_upload)), collapse = "\n")
  testthat::expect_false(base::grepl("uploaded$signature_source", body_src, fixed = TRUE))
  testthat::expect_false(base::grepl("metadata$signature_source", body_src, fixed = TRUE))
})

test_that("the pull route declares rummagene as the source", {
  api_src <- base::paste(base::readLines(repo_file("api", "api.R"), warn = FALSE), collapse = "\n")
  # (?s) so "." crosses newlines -- without it this matches nothing and the
  # test passes vacuously on a route that never sets the source.
  pull <- base::regmatches(api_src, base::regexpr(
    "(?s)rummagene_pull_route <- function.*?\\n\\}", api_src, perl = TRUE))
  testthat::expect_length(pull, 1)
  testthat::expect_match(pull, 'signature_source = "rummagene"', fixed = TRUE)
})

# ---------------------------------------------------------------------------
# The MCP server's copy of the query.
#
# mcp/lib/queries.R defines its OWN search_signatures, and unlike the API's it
# names columns explicitly instead of selecting s.* -- an MCP result goes into
# a model's context, where every extra column costs tokens. The consequence is
# that a new column does NOT reach it for free, so the two implementations can
# silently disagree about what a signature record contains.
#
# Loaded into its own environment rather than source()d at the top of this
# file, because sourcing it would REPLACE the API's search_signatures for
# every test above -- which is exactly the accident that made this file pass
# alone and fail in the suite. Naming the environment also makes each test say
# plainly which of the two implementations it is exercising.
# ---------------------------------------------------------------------------
mcp_env <- local({
  e <- base::new.env(parent = base::globalenv())
  base::sys.source(testthat::test_path("../../mcp/lib/queries.R"), envir = e)
  e
})
mcp_search <- base::get("search_signatures", envir = mcp_env)

test_that("the MCP query is a separate implementation from the API's", {
  # If these ever become the same function, the isolation above is pointless
  # and the tests below are duplicates -- so assert the premise.
  testthat::expect_false(base::identical(mcp_search, search_signatures))
  testthat::expect_false("sort_by" %in% base::names(base::formals(mcp_search)))
})

test_that("the MCP search_signatures returns signature_source", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  results <- mcp_search(conn, limit = 100, is_admin = TRUE)
  testthat::skip_if(base::nrow(results) == 0, "no signatures to inspect")
  testthat::expect_true("signature_source" %in% base::names(results))
  # A blank field in a model's context reads as "unknown" rather than
  # "curated", so it must never come back empty.
  testthat::expect_true(base::all(base::nzchar(results$signature_source)))
})

test_that("the MCP search_signatures filters by source", {
  conn <- test_conn()
  base::on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  all_rows <- mcp_search(conn, limit = 100, is_admin = TRUE)
  testthat::skip_if(base::nrow(all_rows) == 0, "no signatures to inspect")

  for (src in base::unique(all_rows$signature_source)) {
    filtered <- mcp_search(conn, limit = 100, is_admin = TRUE, signature_source = src)
    testthat::expect_true(base::all(filtered$signature_source == src))
    testthat::expect_equal(base::nrow(filtered), base::sum(all_rows$signature_source == src))
  }

  testthat::expect_equal(
    base::nrow(mcp_search(conn, limit = 100, is_admin = TRUE, signature_source = "no-such-source")), 0L)
  # "" is what an omitted argument looks like coming off the wire.
  testthat::expect_equal(
    base::nrow(mcp_search(conn, limit = 100, is_admin = TRUE, signature_source = "")),
    base::nrow(all_rows))
})

test_that("the MCP tool schema exposes signature_source and explains it", {
  tools_src <- base::paste(base::readLines(repo_file("mcp", "lib", "tools.R"), warn = FALSE), collapse = "\n")

  # Declared, or the model cannot use the filter at all.
  testthat::expect_match(tools_src, "signature_source = ellmer::type_string", fixed = TRUE)

  # And the description has to say what a non-curated source MEANS. A model
  # that cannot tell a curated deposit from a gene list scraped out of a
  # supplementary table will weigh the two equally -- which is the actual risk.
  testthat::expect_match(tools_src, "signature_source: 'curated'", fixed = TRUE)
  testthat::expect_match(tools_src, "supplementary table", fixed = TRUE)
})
