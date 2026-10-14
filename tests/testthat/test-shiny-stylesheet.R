# The app's look lives in one stylesheet, www/assets/css/sigrepo.css (issue
# #157). These tests keep it that way: no module carries inline CSS, every tab
# renders inside the shared page skeleton, and every class name the app emits
# has a rule in the stylesheet, so a class renamed on one side cannot go stale
# on the other.

app_dir <- testthat::test_path("../../legacy_app")
css_dir <- file.path(app_dir, "www", "assets", "css")
stylesheet <- file.path(css_dir, "sigrepo.css")

app_r_files <- list.files(
  file.path(app_dir, c("app_src", "modules", "modals")),
  pattern = "[.]R$", full.names = TRUE
)

read_app_file <- function(path) readLines(path, warn = FALSE)

# Classes that Bootstrap 3, Shiny, DataTables and Font Awesome style for us.
framework_class <- function(x) {
  grepl(
    paste0(
      "^(btn|btn-[a-z]+|btn-group|dropdown-toggle|dropdown-menu|dropdown-menu-right|caret|",
      "action-button|shiny-[a-z-]+|alert|alert-[a-z]+|label|label-[a-z]+|text-[a-z]+|",
      "form-group|form-control|input-sm|checkbox|radio|row|col-[a-z]+-[0-9]+|container-fluid|",
      "compact|stripe|hover|nowrap|cell-border|dataTables_[a-z]+|",
      "fa|fas|far|fab|fa-[a-z0-9-]+)$"
    ),
    x
  )
}

# Every token inside class = "..." / class='...' literals in the app's R files.
emitted_classes <- function(files) {
  text <- unlist(lapply(files, read_app_file))
  literals <- unlist(regmatches(text, gregexpr("class *= *[\"'][^\"']+[\"']", text)))
  literals <- sub("^class *= *[\"']", "", literals)
  literals <- sub("[\"']$", "", literals)
  tokens <- unique(trimws(unlist(strsplit(literals, "[[:space:]]+"))))
  tokens <- tokens[nzchar(tokens) & !grepl("[^A-Za-z0-9_-]", tokens)]
  sort(tokens)
}

test_that("sigrepo.css is the app's only stylesheet", {
  expect_true(file.exists(stylesheet))
  expect_equal(list.files(css_dir, pattern = "[.]css$"), "sigrepo.css")
})

test_that("no module, modal or shell file carries inline CSS", {
  with_style <- app_r_files[vapply(app_r_files, function(f) {
    any(grepl("tags\\$style", read_app_file(f), fixed = FALSE))
  }, logical(1))]
  expect_equal(basename(with_style), character(0))

  # Only the shell links a stylesheet, and it links the shared one.
  with_link <- app_r_files[vapply(app_r_files, function(f) {
    any(grepl("tags\\$link", read_app_file(f)))
  }, logical(1))]
  expect_equal(basename(with_link), "app_ui.R")
  expect_true(any(grepl("assets/css/sigrepo.css", read_app_file(file.path(app_dir, "app_src", "app_ui.R")), fixed = TRUE)))
})

test_that("every class the app emits has a rule in sigrepo.css", {
  css <- paste(readLines(stylesheet, warn = FALSE), collapse = "\n")
  classes <- emitted_classes(app_r_files)
  classes <- classes[!framework_class(classes)]
  expect_gt(length(classes), 20)

  missing <- classes[!vapply(classes, function(cls) {
    grepl(paste0("\\.", cls, "([^A-Za-z0-9_-]|$)"), css)
  }, logical(1))]
  expect_equal(missing, character(0))
})

test_that("every tab's UI renders inside the shared page skeleton with no inline style", {
  for (pkg in c("shinyjs", "shiny", "DT", "httr", "jsonlite", "dplyr", "ggplot2")) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
  }
  env <- new.env(parent = globalenv())
  source(testthat::test_path("../../api/lib/msigdb_cache.R"), local = env)
  for (file in c(
    "utils/utils.R", "utils/signature_utils.R", "utils/home_utils.R", "utils/compare_utils.R",
    "utils/annotate_utils.R", "modals/manage_users_modal.R", "modals/delete_modal.R",
    "modals/upload_modal.R", "modules/home_module.R", "modules/signature_module.R",
    "modules/collection_module.R", "modules/annotate_module.R", "modules/compare_module.R",
    "modules/reference_module.R", "modules/feedback_module.R"
  )) {
    source(file.path(app_dir, file), local = env)
  }

  tabs <- list(
    home = env$home_module_ui("home"),
    signatures = env$signature_module_ui("signatures"),
    collections = env$collection_module_ui("collections"),
    annotate = env$annotate_module_ui("annotate"),
    compare = env$compare_module_ui("compare"),
    references = env$reference_module_ui("references"),
    feedback = env$feedbackUI("feedback")
  )

  for (name in names(tabs)) {
    html <- as.character(htmltools::renderTags(tabs[[name]])$html)
    expect_false(grepl("<style", html, fixed = TRUE), info = name)
    expect_true(grepl('class="sr-page"', html, fixed = TRUE), info = name)
    expect_true(grepl('class="page-titlebar"', html, fixed = TRUE), info = name)
  }
})
