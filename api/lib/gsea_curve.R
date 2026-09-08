# Running-enrichment curve and leading edge for a GSEA run.
#
# hypeR reports one row per gene set -- label, score, pval, fdr, geneset,
# signature, overlap, hits -- and nothing about WHERE in the ranking the hits
# fell. The leading edge (the genes up to the peak of the running score) is the
# part people actually interpret, so it has to be computed here.
#
# This is the calculation the Shiny app did in
# shiny/modules/annotate_module.R, kept deliberately close to it so results do
# not silently change between the two interfaces.

# Weighted running enrichment score over a ranked signature.
#
#   ranked_scores  named numeric, sorted by score descending (names = genes)
#   geneset_genes  character
#   power          hit weighting. 0 is the classic unweighted KS statistic;
#                  1 is hypeR's default and what "GSEA" means here.
#
# Returns list(curve, es_index, es_score, es_direction, leading_edge_genes,
# trailing_edge_genes, n_leading), or NULL when the gene set does not overlap
# the ranking at all.
compute_gsea_curve <- function(ranked_scores, geneset_genes, power = 1) {
  signature_genes <- base::names(ranked_scores)
  signature_scores <- base::as.numeric(ranked_scores)

  valid <- !base::is.na(signature_scores) & !base::is.na(signature_genes) & base::nzchar(signature_genes)
  signature_genes <- signature_genes[valid]
  signature_scores <- signature_scores[valid]
  if (base::length(signature_scores) == 0) {
    return(NULL)
  }

  geneset_genes <- base::unique(base::as.character(geneset_genes))
  geneset_genes <- geneset_genes[!base::is.na(geneset_genes) & base::nzchar(geneset_genes)]

  hit_index <- base::which(signature_genes %in% geneset_genes)
  if (base::length(hit_index) == 0) {
    return(NULL)
  }

  hit_weights <- base::abs(signature_scores[hit_index])^power
  weight_sum <- base::sum(hit_weights)
  # All-zero scores (or power 0) would divide by zero; fall back to equal
  # weighting, which is the unweighted statistic.
  if (!base::is.finite(weight_sum) || weight_sum == 0) {
    hit_weights <- base::rep(1, base::length(hit_index))
    weight_sum <- base::sum(hit_weights)
  }

  n_total <- base::length(signature_scores)
  n_miss <- n_total - base::length(hit_index)
  miss_penalty <- if (n_miss > 0) 1 / n_miss else 0

  increments <- base::rep(-miss_penalty, n_total)
  increments[hit_index] <- hit_weights / weight_sum
  running_score <- base::cumsum(increments)

  max_idx <- base::which.max(running_score)
  min_idx <- base::which.min(running_score)

  # The enrichment score is the running score's largest DEVIATION from zero,
  # which may be negative -- a set concentrated at the bottom of the ranking is
  # as meaningful as one at the top, and the leading edge is then the tail.
  if (base::abs(running_score[max_idx]) >= base::abs(running_score[min_idx])) {
    es_index <- max_idx
    es_score <- running_score[max_idx]
    es_direction <- "positive"
    leading_edge_hits <- hit_index[hit_index <= es_index]
    trailing_edge_hits <- hit_index[hit_index > es_index]
  } else {
    es_index <- min_idx
    es_score <- running_score[min_idx]
    es_direction <- "negative"
    leading_edge_hits <- hit_index[hit_index >= es_index]
    trailing_edge_hits <- hit_index[hit_index < es_index]
  }

  base::list(
    # Downsampled for transport: the browser draws a line a few hundred pixels
    # wide, and a 20,000-point curve would be megabytes of JSON to render the
    # same shape. Hit positions are kept exactly -- they are the tick marks.
    curve = downsample_curve(running_score, max_points = 600),
    hit_positions = base::as.integer(hit_index),
    n_total = base::as.integer(n_total),
    es_index = base::as.integer(es_index),
    es_score = base::as.numeric(es_score),
    es_direction = es_direction,
    leading_edge_genes = base::unname(signature_genes[leading_edge_hits]),
    trailing_edge_genes = base::unname(signature_genes[trailing_edge_hits]),
    n_leading = base::length(leading_edge_hits)
  )
}

# Keep the shape, drop the point count. Always retains the first and last point
# so the curve starts and ends where it should.
downsample_curve <- function(running_score, max_points = 600) {
  n <- base::length(running_score)
  if (n <= max_points) {
    idx <- base::seq_len(n)
  } else {
    idx <- base::unique(base::c(1L, base::as.integer(base::seq(1, n, length.out = max_points)), n))
  }
  base::list(
    position = base::as.integer(idx),
    running_score = base::round(base::as.numeric(running_score[idx]), 6)
  )
}
