# Request-duration logging.
#
# The API serialises requests (single R process), and a heavy endpoint can hold
# the queue long enough for nginx to give up at 300s. This logging exists to
# answer "is that actually happening in production, and where" with evidence
# rather than argument -- so the tests that matter are the ones protecting the
# properties that make the log trustworthy:
#
#   * api_keys must never reach it (31 endpoints take api_key as a QUERY param)
#   * a slow request must be findable (grep SLOW)
#   * an unknown duration must not masquerade as either fast or slow
#
# request_log_line() and .slow_request_ms() live in api/api.R rather than
# api/lib/ because plumber parses route annotations only from that file and the
# hook that calls them has to be defined there too. api.R is not sourceable as
# a whole (it is full of unannotated route functions that plumb() assembles), so
# these tests extract the two definitions rather than source the file.

api_source <- function() {
  base::paste(base::readLines(testthat::test_path("../../api/api.R"), warn = FALSE), collapse = "\n")
}

# Pull one top-level `name <- function(...) { ... }` definition out of api.R and
# evaluate it, so the tests exercise the real shipped code rather than a copy
# that can drift from it.
extract_fn <- function(name, envir) {
  src <- api_source()
  start <- base::regexpr(base::sprintf("(?m)^%s <- function", name), src, perl = TRUE)
  testthat::expect_true(start[[1]] > 0)
  rest <- base::substring(src, start[[1]])
  # Walk braces to find the end of the definition.
  chars <- base::strsplit(rest, "")[[1]]
  depth <- 0L
  started <- FALSE
  end <- NA_integer_
  for (i in base::seq_along(chars)) {
    if (chars[i] == "{") { depth <- depth + 1L; started <- TRUE }
    else if (chars[i] == "}") {
      depth <- depth - 1L
      if (started && depth == 0L) { end <- i; break }
    }
  }
  testthat::expect_false(base::is.na(end))
  base::eval(base::parse(text = base::substring(rest, 1, end)), envir = envir)
  base::get(name, envir = envir)
}

log_env <- base::new.env(parent = base::globalenv())
slow_threshold <- extract_fn(".slow_request_ms", log_env)
log_line <- extract_fn("request_log_line", log_env)

fixed_now <- base::as.POSIXct("2026-09-02 12:00:00", tz = "UTC")

test_that("the completion line carries method, path, status and duration", {
  line <- log_line("GET", "/signatures/search", 200L, 7.25,
                   now = fixed_now, threshold_ms = 1000)
  testthat::expect_match(line, "done GET /signatures/search", fixed = TRUE)
  testthat::expect_match(line, "status=200", fixed = TRUE)
  testthat::expect_match(line, "dur_ms=7.2", fixed = TRUE)
  # One line, newline-terminated -- cat() adds nothing of its own.
  testthat::expect_equal(base::length(base::strsplit(line, "\n")[[1]]), 1L)
  testthat::expect_true(base::endsWith(line, "\n"))
})

test_that("a slow request is tagged so it can be grepped", {
  # The whole point: `docker logs sigrepo-api | grep SLOW` has to find it.
  slow <- log_line("POST", "/annotate/run", 200L, 2330, now = fixed_now, threshold_ms = 1000)
  testthat::expect_match(slow, "SLOW", fixed = TRUE)

  fast <- log_line("GET", "/signatures/search", 200L, 7, now = fixed_now, threshold_ms = 1000)
  testthat::expect_false(base::grepl("SLOW", fast, fixed = TRUE))

  # Boundary: at exactly the threshold it counts as slow, so a threshold set to
  # a round number does not silently exclude the case it was set to catch.
  at <- log_line("GET", "/x", 200L, 1000, now = fixed_now, threshold_ms = 1000)
  testthat::expect_match(at, "SLOW", fixed = TRUE)
})

test_that("a request with no recorded start is neither fast nor slow", {
  # If the preroute hook did not run there is no start time. Reporting that as
  # 0 ms would be a lie; tagging it SLOW would put noise in the grep someone
  # runs when hunting a real problem.
  line <- log_line("GET", "/x", 200L, NA_real_, now = fixed_now, threshold_ms = 1000)
  testthat::expect_match(line, "dur_ms=NA", fixed = TRUE)
  testthat::expect_false(base::grepl("SLOW", line, fixed = TRUE))

  null_line <- log_line(NULL, NULL, NULL, NULL, now = fixed_now, threshold_ms = 1000)
  testthat::expect_match(null_line, "done ? ? status=200 dur_ms=NA", fixed = TRUE)
})

test_that("the slow threshold is configurable and rejects nonsense", {
  withr_env <- function(value, expr) {
    old <- base::Sys.getenv("SIGREPO_SLOW_REQUEST_MS", unset = NA)
    base::on.exit({
      if (base::is.na(old)) base::Sys.unsetenv("SIGREPO_SLOW_REQUEST_MS")
      else base::Sys.setenv(SIGREPO_SLOW_REQUEST_MS = old)
    }, add = TRUE)
    base::Sys.setenv(SIGREPO_SLOW_REQUEST_MS = value)
    expr()
  }
  base::Sys.unsetenv("SIGREPO_SLOW_REQUEST_MS")
  testthat::expect_equal(slow_threshold(), 1000)
  testthat::expect_equal(withr_env("250", slow_threshold), 250)
  # A typo in an env var must not disable the tagging silently.
  testthat::expect_equal(withr_env("junk", slow_threshold), 1000)
  testthat::expect_equal(withr_env("-5", slow_threshold), 1000)
  testthat::expect_equal(withr_env("0", slow_threshold), 1000)
})

test_that("the request log never touches the query string", {
  # THE security property of this feature. 31 endpoints accept api_key as a
  # query parameter, so anything that logged QUERY_STRING would write plaintext
  # API keys into the container logs -- and from there into wherever logs are
  # shipped. PATH_INFO excludes the query string; QUERY_STRING must not appear
  # anywhere in api.R.
  # Checked against PARSED CODE, not the raw text: the comments right above the
  # hook explain why QUERY_STRING must not be logged, and a plain grep matches
  # those and fails. Asking the parser separates the two -- documenting the
  # hazard is fine, reading the value is not.
  parsed <- base::parse(testthat::test_path("../../api/api.R"), keep.source = TRUE)
  tokens <- utils::getParseData(parsed)
  code_tokens <- tokens[tokens$token != "COMMENT", "text"]
  testthat::expect_false(base::any(base::grepl("QUERY_STRING", code_tokens, fixed = TRUE)))

  # Guard the guard: if the parse ever returns nothing, the assertion above
  # passes vacuously and this test stops protecting anything.
  testthat::expect_true(base::length(code_tokens) > 1000)
  # And confirm the check can actually see comments, so "no COMMENT tokens"
  # is not the reason it passed.
  testthat::expect_true(base::any(tokens$token == "COMMENT"))

  # And the formatter cannot reach it even by accident: it takes a path, not a
  # request object.
  testthat::expect_true("path" %in% base::names(base::formals(log_line)))
  testthat::expect_false("req" %in% base::names(base::formals(log_line)))
})

test_that("both a start line and a completion line are emitted", {
  src <- api_source()
  # They answer different questions. The filter proves a request ARRIVED; the
  # hook proves it FINISHED. A request that hangs until nginx gives up never
  # reaches the hook, so a start line with no matching completion line is the
  # only evidence it happened -- which is the failure this logging exists for.
  testthat::expect_match(src, "#* @filter logger", fixed = TRUE)
  testthat::expect_match(src, "#* @plumber", fixed = TRUE)
  testthat::expect_match(src, 'pr_hook(pr, "preroute"', fixed = TRUE)
  testthat::expect_match(src, 'pr_hook(pr, "postserialize"', fixed = TRUE)
})

test_that("api.R still parses and registers its routes", {
  # The @plumber block is new; a malformed one would break every route in the
  # file, and a unit test of the formatter would not notice.
  testthat::skip_if_not(base::requireNamespace("plumber", quietly = TRUE))
  pr <- base::tryCatch(
    base::suppressWarnings(plumber::plumb(file = testthat::test_path("../../api/api.R"))),
    error = function(e) e
  )
  testthat::expect_false(base::inherits(pr, "error"))
  testthat::expect_true(base::length(pr$routes) > 0)
})
