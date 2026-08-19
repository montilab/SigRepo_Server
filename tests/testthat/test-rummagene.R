# Rummagene response parsing, with no network: the GraphQL payloads here are
# trimmed copies of real responses from rummagene.com.
#
# Sourced in the same order api.R uses (sort(list.files(...)) -- alphabetical),
# because that ordering is exactly what the regression below is about.
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene.R"), local = FALSE)

test_that("%||% is length-safe, so a multi-hit response does not abort parsing", {
  # api.R sources api/lib/*.R alphabetically, so annotate.R's %||% wins over
  # the one guarded inside rummagene.R -- every later file gets this one.
  # It used to call is.na(x) inside ||, which since R 4.3 is a hard error for
  # anything not of length one. rummagene_enrich() does `enrich$nodes %||%
  # list()`, so ANY successful multi-hit enrichment died with
  # "'length = N' in coercion to 'logical(1)'", where N was the requested limit.
  expect_equal(NULL %||% "fallback", "fallback")
  expect_equal(NA %||% "fallback", "fallback")
  expect_equal(NA_character_ %||% "fallback", "fallback")
  expect_equal("value" %||% "fallback", "value")

  # The cases that used to throw.
  expect_equal(list(1, 2, 3) %||% list(), list(1, 2, 3))
  expect_equal(character(0) %||% "fallback", character(0))
  expect_equal(c(1, NA, 3) %||% "fallback", c(1, NA, 3))
})

test_that("rummagene_hit flattens a node, preferring structured PMC info", {
  node <- list(
    pvalue = 1e-9, adjPvalue = 2e-6, oddsRatio = 12.5, nOverlap = 4,
    geneSets = list(nodes = list(list(
      term = "PMC1234567-table1.xlsx-2-Gene_symbol",
      description = "a description",
      nGeneIds = 40,
      geneSetPmcsById = list(nodes = list(list(pmcInfoByPmcid = list(
        pmcid = "PMC1234567", title = "A paper", yr = 2021, doi = "10.1/xyz"
      ))))
    )))
  )

  hit <- rummagene_hit(node)
  expect_equal(hit$pmcid, "PMC1234567")
  expect_equal(hit$title, "A paper")
  expect_equal(hit$n_overlap, 4)
  expect_equal(hit$n_geneset, 40)
  expect_equal(hit$pmc_url, "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC1234567/")

  # Every field must be a scalar: the route hands this straight to the JSON
  # serializer with auto_unbox, so a stray vector would silently reshape the
  # payload the UI reads.
  for (field in names(hit)) {
    expect_length(hit[[field]], 1)
  }
})

test_that("rummagene_hit falls back to the PMC id embedded in the term", {
  # Real responses often carry no geneSetPmcsById at all.
  node <- list(
    pvalue = 0.01, adjPvalue = 0.4, oddsRatio = 3, nOverlap = 2,
    geneSets = list(nodes = list(list(
      term = "PMC7654321-supp.docx-1-Gene", description = NULL, nGeneIds = 10
    )))
  )

  hit <- rummagene_hit(node)
  expect_equal(hit$pmcid, "PMC7654321")
  expect_equal(hit$pmc_url, "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC7654321/")
  expect_true(is.na(hit$title))
})

test_that("rummagene_hit returns NULL when a node carries no gene set", {
  expect_null(rummagene_hit(list(pvalue = 1, geneSets = list(nodes = list()))))
  expect_null(rummagene_hit(list(pvalue = 1)))
})

test_that("rummagene_enrich rejects a query too small to be meaningful", {
  expect_error(rummagene_enrich(c("TP53")), "at least two gene symbols")
  expect_error(rummagene_enrich(c("TP53", "", NA)), "at least two gene symbols")
})
