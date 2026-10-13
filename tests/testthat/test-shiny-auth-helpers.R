# The pure helpers behind the sign-in, profile and connection flows
# (legacy_app/utils/auth_utils.R) and the shared field_value() in utils.R.
for (pkg in c("shiny", "dplyr")) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}
source(testthat::test_path("../../api/lib/user.R"), local = globalenv())
source(testthat::test_path("../../legacy_app/utils/utils.R"), local = globalenv())
source(testthat::test_path("../../legacy_app/utils/auth_utils.R"), local = globalenv())

auth_users <- data.frame(
  user_name = c("ann", "bo"),
  user_email = c("Ann@Example.org", "bo@example.org"),
  active = c(1L, 0L),
  stringsAsFactors = FALSE
)

test_that("trimmed() reads a text input as one trimmed string", {
  expect_equal(trimmed("  ann  "), "ann")
  expect_equal(trimmed(NULL), "")
  expect_equal(trimmed(character(0)), "")
  expect_equal(trimmed(NA), "")
  expect_equal(trimmed(c("first", "second")), "first")
})

test_that("empty_problem() returns the message only for a blank value", {
  expect_equal(empty_problem("", "'Username' cannot be empty"), "'Username' cannot be empty")
  expect_null(empty_problem("ann", "'Username' cannot be empty"))
})

test_that("signin_user_problem() names an inactive account and hides an unknown one", {
  expect_equal(signin_user_problem(auth_users[0, ], "zed"), "Invalid username or password!")
  expect_equal(signin_user_problem(data.frame(), "zed"), "Invalid username or password!")
  expect_match(signin_user_problem(auth_users[2, ], "bo"), "User = 'bo' is currently inactive")
  expect_null(signin_user_problem(auth_users[1, ], "ann"))
})

test_that("email checks: format, then whether another account has it", {
  expect_equal(email_format_problem("not-an-email"), "Invalid email format.")
  expect_null(email_format_problem("ann@example.org"))

  expect_true(email_taken(auth_users, "ann@EXAMPLE.org"))
  expect_false(email_taken(auth_users, "cy@example.org"))
  expect_false(email_taken(data.frame(), "cy@example.org"))

  expect_match(email_taken_problem("bo@example.org", auth_users), "Email = 'bo@example.org' already existed")
  expect_null(email_taken_problem("cy@example.org", auth_users))
})

test_that("blank_for_na() shows missing profile fields as blanks", {
  shown <- blank_for_na(data.frame(
    user_name = "ann", user_first = NA, user_last = "NA", user_affiliation = "NULL",
    stringsAsFactors = FALSE
  ))
  expect_equal(shown$user_name, "ann")
  expect_equal(shown$user_first, "")
  expect_equal(shown$user_last, "")
  expect_equal(shown$user_affiliation, "")
})

test_that("api_connection() takes the API address from the environment with no fallback", {
  env <- function(name) c(API_HOST = "http://sigrepo-api:3838", API_PORT = "3838")[[name]]
  expect_equal(api_connection(env), list(api_host = "http://sigrepo-api:3838", api_port = 3838L))

  unset <- function(name) ""
  expect_equal(api_connection(unset), list(api_host = ""))
})

test_that("field_value() reads one display value with a default for anything missing", {
  row <- data.frame(signature_name = "alpha", user_name = NA, phenotype = "", score = 1.5, stringsAsFactors = FALSE)
  expect_equal(field_value(row, "signature_name"), "alpha")
  expect_equal(field_value(row, "user_name"), "Not available")
  expect_equal(field_value(row, "phenotype"), "Not available")
  expect_equal(field_value(row, "missing_column"), "Not available")
  expect_equal(field_value(NULL, "signature_name"), "Not available")
  expect_equal(field_value(row, "score"), "1.5")
  expect_equal(field_value(row, "user_name", default = "0"), "0")
})
