# The Home tab's helpers, exercised without Shiny or a database.
#
# The tab mirrors the React dashboard (web/src/pages/DashboardPage.tsx): four
# totals, a count per organism and per assay, and the five most active
# contributors. These helpers turn the searchSignature() frame into those
# numbers and into the ggplot figures the cards show, so the module itself is
# only layout.
source(testthat::test_path("../../legacy_app/utils/home_utils.R"), local = FALSE)

# The columns searchSignature() returns that the Home tab reads. One organism
# is NA to stand in for a signature whose organism row is missing, which the
# API's LEFT JOIN also lets through.
home_rows <- function() {
  data.frame(
    signature_id = 1:6,
    organism = c("Mus musculus", "Homo sapiens", "Mus musculus", "Mus musculus", NA, "Homo sapiens"),
    assay_type = c("transcriptomics", "transcriptomics", "proteomics", "transcriptomics", "metabolomics", "proteomics"),
    user_name = c("ann", "bo", "ann", "cy", "ann", "bo"),
    stringsAsFactors = FALSE
  )
}

# ---- home_summary_counts ----------------------------------------------------

test_that("summary counts distinct users, organisms and assays", {
  counts <- home_summary_counts(home_rows())
  expect_equal(counts$total_signatures, 6L)
  expect_equal(counts$total_users, 3L)
  expect_equal(counts$total_organisms, 2L)
  expect_equal(counts$total_assays, 3L)
})

test_that("summary of an empty frame is all zeros", {
  counts <- home_summary_counts(data.frame())
  expect_equal(unname(unlist(counts)), c(0L, 0L, 0L, 0L))
})

test_that("summary tolerates a frame without the organism column", {
  rows <- home_rows()
  rows$organism <- NULL
  expect_equal(home_summary_counts(rows)$total_organisms, 0L)
})

# ---- home_count_by ----------------------------------------------------------

test_that("count_by returns name/value rows sorted by value descending", {
  counts <- home_count_by(home_rows(), "assay_type")
  expect_identical(names(counts), c("name", "value"))
  expect_identical(counts$name, c("transcriptomics", "proteomics", "metabolomics"))
  expect_identical(counts$value, c(3L, 2L, 1L))
})

test_that("count_by labels a missing value as Unknown instead of dropping it", {
  counts <- home_count_by(home_rows(), "organism")
  expect_true("Unknown" %in% counts$name)
  expect_equal(sum(counts$value), 6L)
})

test_that("count_by keeps only the top n rows when asked", {
  counts <- home_count_by(home_rows(), "user_name", n = 2)
  expect_identical(counts$name, c("ann", "bo"))
})

test_that("count_by of an empty frame or absent column is a zero-row frame", {
  empty <- home_count_by(data.frame(), "organism")
  expect_identical(names(empty), c("name", "value"))
  expect_equal(nrow(empty), 0L)
  expect_equal(nrow(home_count_by(home_rows(), "no_such_column")), 0L)
})

# ---- home_legend_tags -------------------------------------------------------

test_that("legend shows the top five entries with a palette dot each", {
  counts <- data.frame(
    name = paste0("org", 1:7),
    value = 7:1,
    stringsAsFactors = FALSE
  )
  html <- as.character(home_legend_tags(counts))
  expect_equal(lengths(regmatches(html, gregexpr("legend-item", html))), 5L)
  expect_true(grepl("org1", html, fixed = TRUE))
  expect_false(grepl("org6", html, fixed = TRUE))
  expect_true(grepl(home_palette()[1], html, fixed = TRUE))
})

# ---- plots ------------------------------------------------------------------

test_that("each chart builder returns a ggplot for real counts", {
  counts <- home_count_by(home_rows(), "organism")
  expect_s3_class(home_donut_plot(counts), "ggplot")
  expect_s3_class(home_bar_plot(counts), "ggplot")
  expect_s3_class(home_hbar_plot(counts), "ggplot")
})

test_that("each chart builder still returns a drawable ggplot for no rows", {
  # Building a plot opens the default graphics device, which would otherwise
  # leave an Rplots.pdf behind in the test directory.
  grDevices::pdf(NULL)
  on.exit(grDevices::dev.off(), add = TRUE)
  none <- home_count_by(data.frame(), "organism")
  for (builder in list(home_donut_plot, home_bar_plot, home_hbar_plot)) {
    p <- builder(none)
    expect_s3_class(p, "ggplot")
    expect_no_error(ggplot2::ggplot_build(p))
  }
})
