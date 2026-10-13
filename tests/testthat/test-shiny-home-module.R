# The Home tab's module, run under testServer() with a canned signature frame.
#
# The tab reads signature_db() and shows four totals, three charts and the
# organism legend. These tests pin the ids the module renders into and what
# they contain, so a change to the layout that drops a card is caught here
# rather than by eye.
for (pkg in c("shinyjs", "shiny", "dplyr", "ggplot2")) {
  suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
}
source(testthat::test_path("../../legacy_app/utils/home_utils.R"), local = FALSE)
source(testthat::test_path("../../legacy_app/modules/home_module.R"), local = FALSE)

home_db_rows <- data.frame(
  signature_id = 1:6,
  organism = c("Mus musculus", "Homo sapiens", "Mus musculus", "Mus musculus", NA, "Homo sapiens"),
  assay_type = c("transcriptomics", "transcriptomics", "proteomics", "transcriptomics", "metabolomics", "proteomics"),
  user_name = c("ann", "bo", "ann", "cy", "ann", "bo"),
  stringsAsFactors = FALSE
)

# testServer() quotes its expression, so splice the caller's in. It runs on a
# private later loop for the same reason helper-shiny-signature.R gives.
run_home_module <- function(expr, rows = home_db_rows) {
  signature_db <- reactive(rows)
  later::with_temp_loop(eval(bquote(testServer(
    home_module_server,
    args = list(
      signature_db = .(signature_db),
      parent_session = NULL
    ),
    .(substitute(expr))
  )), envir = parent.frame()))
}

test_that("the stat cards show the totals for the visible signatures", {
  run_home_module({
    expect_equal(output$stat_signatures, "6")
    expect_equal(output$stat_users, "3")
    expect_equal(output$stat_organisms, "2")
    expect_equal(output$stat_assays, "3")
  })
})

test_that("the organism legend lists organisms largest first", {
  run_home_module({
    html <- output$organism_legend$html
    expect_true(regexpr("Mus musculus", html, fixed = TRUE) < regexpr("Homo sapiens", html, fixed = TRUE))
    expect_true(grepl("Unknown", html, fixed = TRUE))
  })
})

test_that("with no signatures the cards show zero and the charts still draw", {
  run_home_module(rows = data.frame(), {
    expect_equal(output$stat_signatures, "0")
    expect_equal(output$stat_users, "0")
    expect_type(output$organism_plot$src, "character")
    expect_type(output$assay_plot$src, "character")
    expect_type(output$top_users_plot$src, "character")
  })
})
