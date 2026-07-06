source(testthat::test_path("../../api/lib/feature_data.R"), local = FALSE)

test_that("parse_organism_filter splits, trims, sorts, and dedupes comma-separated organisms", {
  expect_equal(parse_organism_filter("Human, Mouse"), c("Human", "Mouse"))
  expect_equal(parse_organism_filter(c("Mouse", " mouse ", "Human")), sort(unique(trimws(c("Mouse", " mouse ", "Human")))))
  expect_equal(parse_organism_filter(NULL), NULL)
  expect_equal(parse_organism_filter(""), "")
  expect_equal(parse_organism_filter(NA), NA)
})
