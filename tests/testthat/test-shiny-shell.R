# The page shell (app_src/app_ui.R): the sign-in form's keyboard handling.
#
# The inline handlers used to call login_keypress(e). Inside an inline handler
# the event is `event`, so every keypress threw a ReferenceError and Enter did
# nothing; the button carried the same call on click, which threw too
# (issue #159).
for (pkg in c("shinyjs", "shiny", "DT", "httr", "jsonlite", "dplyr", "ggplot2")) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}

app_dir <- testthat::test_path("../../legacy_app")
shell_env <- new.env(parent = globalenv())
source(testthat::test_path("../../api/lib/msigdb_cache.R"), local = shell_env)
for (file in c(
  "utils/utils.R", "utils/signature_utils.R", "utils/home_utils.R", "utils/compare_utils.R",
  "utils/annotate_utils.R", "modals/manage_users_modal.R", "modals/delete_modal.R",
  "modals/upload_modal.R", "modules/auth_module.R", "modules/home_module.R", "modules/signature_module.R",
  "modules/collection_module.R", "modules/annotate_module.R", "modules/compare_module.R",
  "modules/reference_module.R", "modules/feedback_module.R", "app_src/app_ui.R"
)) {
  source(file.path(app_dir, file), local = shell_env)
}
shell_html <- as.character(htmltools::renderTags(shell_env$ui)$html)

test_that("Enter in either sign-in field reaches login_keypress with the event", {
  handlers <- regmatches(shell_html, gregexpr("login_keypress\\([a-z]+\\)", shell_html))[[1]]
  expect_equal(handlers, c("login_keypress(event)", "login_keypress(event)"))
})

test_that("Enter sends both field values before it clicks the button", {
  # Shiny's text bindings debounce keystrokes, so a click straight after the
  # last character could reach the server before the password did.
  js <- paste(readLines(file.path(app_dir, "www", "assets", "js", "app.js"), warn = FALSE), collapse = "\n")
  keypress <- regmatches(js, regexpr("function login_keypress\\(e\\)\\{.*?\\n\\}", js))
  expect_length(keypress, 1)
  expect_true(grepl('setInputValue("username"', keypress, fixed = TRUE))
  expect_true(grepl('setInputValue("password"', keypress, fixed = TRUE))
  expect_lt(
    regexpr('setInputValue("password"', keypress, fixed = TRUE),
    regexpr('getElementById("sign_in_btn").click()', keypress, fixed = TRUE)
  )
})

test_that("the sign-in button has no inline click handler", {
  button <- regmatches(shell_html, regexpr('<button[^>]*id="sign_in_btn"[^>]*>', shell_html))
  expect_length(button, 1)
  expect_false(grepl("onclick", button, fixed = TRUE))
})
