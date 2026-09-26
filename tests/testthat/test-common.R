source(testthat::test_path("../../api/lib/common.R"), local = FALSE)

# json_response()/json_error() look up `serializers` in their defining
# environment (globalenv, since common.R was source()'d with local = FALSE),
# so the mock needs to live there too rather than inside a test_that() block.
assign("serializers", list(json = "json-serializer-marker"), envir = globalenv())

mock_res <- function() {
  e <- new.env()
  e$serializer <- NULL
  e$status <- NULL
  e
}

test_that("sourcing common.R again closes the pool it would otherwise orphan", {
  testthat::skip_if_not_installed("pool")
  # Most test files source common.R into the global environment, and each
  # source replaces .db_pool. An orphaned pool keeps a recurring task on later's
  # event loop, which blocks every shiny::testServer() output read after it.
  old <- pool::poolCreate(factory = function() new.env(), minSize = 1, maxSize = 2)
  # assign() into the environment rather than `.db_pool$pool <-`, which would
  # also bind a local .db_pool here that still points at the replaced one.
  base::assign("pool", old, envir = base::get(".db_pool", envir = globalenv()))

  source(testthat::test_path("../../api/lib/common.R"), local = FALSE)

  expect_false(old$valid)
  expect_null(base::get(".db_pool", envir = globalenv())$pool)
})

test_that("normalize_flag interprets truthy/falsy strings and defaults", {
  expect_equal(normalize_flag("true"), 1L)
  expect_equal(normalize_flag("YES"), 1L)
  expect_equal(normalize_flag("0"), 0L)
  expect_equal(normalize_flag("no"), 0L)
  expect_equal(normalize_flag(NULL, default = TRUE), 1L)
  expect_equal(normalize_flag(NA, default = FALSE), 0L)
})

test_that("json_scalar trims and defaults", {
  expect_equal(json_scalar("  hello  "), "hello")
  expect_equal(json_scalar(NULL), "")
  expect_equal(json_scalar(character(0), default = "fallback"), "fallback")
  expect_equal(json_scalar(NA), "")
})

test_that("json_vector flattens, trims, and drops empties", {
  expect_equal(json_vector(list(" a ", "", "b", NA)), c("a", "b"))
  expect_equal(json_vector(NULL), character())
})

test_that("compact_table caps rows and converts to list-of-lists", {
  tbl <- data.frame(x = 1:5, y = letters[1:5])
  result <- compact_table(tbl, max_rows = 2)
  expect_length(result, 2)
  expect_equal(result[[1]]$x, 1)
  expect_equal(compact_table(NULL), list())
  expect_equal(compact_table(data.frame()), list())
})

test_that("json_error sets status/serializer and embeds the message", {
  res <- mock_res()
  out <- json_error(res, 404, "boom")
  expect_equal(res$status, 404)
  expect_equal(res$serializer, "json-serializer-marker")
  expect_match(as.character(out), "boom")
})

test_that("request_json_body parses valid JSON and tolerates missing/invalid bodies", {
  expect_equal(request_json_body(NULL), list())
  expect_equal(request_json_body(list(postBody = "")), list())
  expect_equal(request_json_body(list(postBody = "not json")), list())
  parsed <- request_json_body(list(postBody = '{"a": 1}'))
  expect_equal(parsed$a, 1)
})

test_that("route_error_message keeps the failing call and message instead of flattening the condition (#130)", {
  err <- tryCatch(stop("Table 'sigrepo.users' doesn't exist"), error = function(e) e)
  err$call <- quote(DBI::dbSendQuery(conn, statement))

  expect_equal(route_error_message(err), "ERROR: Table 'sigrepo.users' doesn't exist (in DBI::dbSendQuery(conn, statement))")
})

test_that("route_error_message reports a condition without a call as just its message (#130)", {
  err <- simpleError("no call here")

  expect_equal(route_error_message(err), "ERROR: no call here")
})
