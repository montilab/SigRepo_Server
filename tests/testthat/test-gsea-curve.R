# Pure numeric behaviour of api/lib/gsea_curve.R -- nothing else sourced this
# file before, and nothing called compute_gsea_curve()/downsample_curve(), so
# the enrichment-score sign convention, the negative-enrichment leading-edge
# inversion, the zero-weight-sum fallback, and the downsample first/last-point
# guarantee were all unpinned. No DB, no network: every case here is a small,
# hand-checkable ranked list.
source(testthat::test_path("../../api/lib/gsea_curve.R"), local = FALSE)

test_that("compute_gsea_curve reports a positive ES with the leading edge up to the peak", {
  # Hits concentrated at the top of the ranking: the running score should
  # climb to its peak at the second hit, then decay to 0 by miss_penalty
  # steps of 1/4 (4 misses among 6 genes).
  ranked <- stats::setNames(c(10, 8, 6, -4, -7, -9), c("g1", "g2", "g3", "g4", "g5", "g6"))
  out <- compute_gsea_curve(ranked, c("g1", "g2"), power = 1)

  expect_false(base::is.null(out))
  expect_equal(out$n_total, 6L)
  expect_equal(out$hit_positions, c(1L, 2L))
  expect_equal(out$es_direction, "positive")
  expect_equal(out$es_index, 2L)
  # (10/18 + 8/18) == 1 exactly: the running score after both hits is the
  # entire hit weight, none of it spent yet on the miss penalty.
  expect_equal(out$es_score, 1, tolerance = 1e-8)
  expect_equal(out$leading_edge_genes, c("g1", "g2"))
  expect_equal(out$trailing_edge_genes, character(0))
  expect_equal(out$n_leading, 2L)
  # n_total (6) is well under the downsample_curve default of 600, so the
  # curve should carry every point untouched.
  expect_equal(out$curve$position, 1:6)
})

test_that("compute_gsea_curve reports a negative ES with the leading edge taken from the bottom of the ranking", {
  # Same ranked list, but the hits now sit at the bottom (most negative
  # scores). The running score should bottom out at the second-to-last gene
  # and the leading edge should be read from the tail upward, not from the
  # top down.
  ranked <- stats::setNames(c(10, 8, 6, -4, -7, -9), c("g1", "g2", "g3", "g4", "g5", "g6"))
  out <- compute_gsea_curve(ranked, c("g5", "g6"), power = 1)

  expect_false(base::is.null(out))
  expect_equal(out$hit_positions, c(5L, 6L))
  expect_equal(out$es_direction, "negative")
  expect_equal(out$es_index, 4L)
  # Four straight miss-penalty steps of -1/4 land exactly on -1 before either
  # hit is reached.
  expect_equal(out$es_score, -1, tolerance = 1e-8)
  # The leading edge is the tail of the ranking (index >= es_index), not the
  # genes before it -- getting this backwards is the bug this test exists to
  # catch.
  expect_equal(out$leading_edge_genes, c("g5", "g6"))
  expect_equal(out$trailing_edge_genes, character(0))
  expect_equal(out$n_leading, 2L)
})

test_that("compute_gsea_curve falls back to equal weighting when every hit scores exactly zero", {
  # power = 1 (the default) makes hit_weights = abs(score)^1. With both hits
  # scoring 0, that is c(0, 0) and weight_sum == 0 -- naively 0/0 -> NaN,
  # which cumsum() would then propagate through the rest of the running
  # score. The fallback should instead weight the two hits equally, giving a
  # fully finite, hand-checkable result.
  ranked <- stats::setNames(c(5, 3, 0, 0, -2, -6), c("g1", "g2", "g3", "g4", "g5", "g6"))
  out <- compute_gsea_curve(ranked, c("g3", "g4"), power = 1)

  expect_false(base::is.null(out))
  expect_false(base::is.nan(out$es_score))
  expect_equal(out$hit_positions, c(3L, 4L))
  # Equal weighting over 2 hits (1/2 each) against a miss penalty of 1/4
  # (4 misses) peaks at exactly +0.5 right after the second hit.
  expect_equal(out$es_direction, "positive")
  expect_equal(out$es_index, 4L)
  expect_equal(out$es_score, 0.5, tolerance = 1e-8)
  expect_equal(out$leading_edge_genes, c("g3", "g4"))
  expect_equal(out$n_leading, 2L)
})

test_that("compute_gsea_curve returns NULL when the gene set does not overlap the ranking", {
  ranked <- stats::setNames(c(3, 1, -2), c("g1", "g2", "g3"))
  expect_null(compute_gsea_curve(ranked, c("not_in_the_ranking")))
})

test_that("compute_gsea_curve returns NULL rather than erroring on an empty ranking", {
  expect_null(compute_gsea_curve(stats::setNames(numeric(0), character(0)), c("g1")))
})

test_that("downsample_curve passes small curves through untouched", {
  out <- downsample_curve(c(1, 2, 3, -4, -5), max_points = 600)
  expect_equal(out$position, 1:5)
  expect_equal(out$running_score, c(1, 2, 3, -4, -5))
})

test_that("downsample_curve keeps the first and last point and never exceeds max_points", {
  running_score <- base::seq(0, 1, length.out = 1000)
  out <- downsample_curve(running_score, max_points = 50)

  expect_true(base::length(out$position) <= 50)
  expect_equal(base::length(out$position), base::length(out$running_score))
  expect_equal(out$position[1], 1L)
  expect_equal(out$position[base::length(out$position)], 1000L)
  expect_equal(out$running_score[1], 0)
  expect_equal(out$running_score[base::length(out$running_score)], 1)
})
