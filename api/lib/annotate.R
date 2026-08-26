# Gene set enrichment analysis backing /annotate/*. Calls hypeR::hypeR()
# directly instead of SigRepo::runHypeR() -- runHypeR() resolves its
# signature via SigRepo::getSignature(), which authorizes through
# SigRepo::checkPermissions() (the *DB connection's own login*, not the
# api_key holder). Since the REST API always connects through one shared
# service-account login (see conn_handler in api.R), that would either leak
# private signatures to callers who shouldn't see them or wrongly hide
# signatures from callers who should -- the same class of bug fixed for
# signature/collection writes, but here on a read path with real disclosure
# risk. Instead: fetch the signature ourselves via fetch_signature_context()
# (which already authorizes against the real api_key caller), build the
# query vector by hand, and call hypeR::hypeR() directly -- SigRepo/
# checkPermissions never enter the picture.
#
# Depends on api/lib/common.R (db_connect_local), api/lib/signature.R
# (fetch_signature_context), and api/lib/difexp.R (load_difexp_rds).

# NULL-or-missing coalesce. The length guard is load-bearing: api.R sources
# api/lib/*.R alphabetically, so this definition wins over the one in
# rummagene.R and is what every later file actually gets. Without the guard,
# `is.na(x)` on anything longer than one element yields a vector, and since R
# 4.3 `||` rejects that outright -- which is how `enrich$nodes %||% list()` in
# rummagene.R died with "'length = 25' in coercion to 'logical(1)'" for a
# perfectly valid 25-node response. Length 0 would fail the same way.
`%||%` <- function(x, y) {
  if (base::is.null(x) || (base::length(x) == 1L && base::is.na(x))) y else x
}

# assay_type -> the reference table holding feature_id -> gene_symbol.
# Enrichment against MSigDB needs gene symbols; only these two assay types
# have a features table with a gene_symbol column today.
enrichment_reference_table <- function(assay_type) {
  switch(assay_type,
    "transcriptomics" = "transcriptomics_features",
    "proteomics" = "proteomics_features",
    NULL
  )
}

# feature_id -> gene_symbol for the given ids, from the appropriate
# reference table. Returns a named character vector (names = feature_id).
lookup_gene_symbols <- function(conn, ref_table, feature_ids) {
  feature_ids <- base::unique(feature_ids[!base::is.na(feature_ids)])
  if (base::length(feature_ids) == 0) {
    return(base::character())
  }

  query <- base::sprintf(
    "SELECT feature_id, gene_symbol FROM %s WHERE feature_id IN (%s)",
    ref_table, base::paste(feature_ids, collapse = ",")
  )
  tbl <- DBI::dbGetQuery(conn, query)
  symbols <- base::trimws(base::as.character(tbl$gene_symbol))
  stats::setNames(symbols, base::as.character(tbl$feature_id))
}

# feature_name -> gene_symbol for the given names within one organism (the
# reference tables' natural key is (feature_name, organism_id) -- see their
# UNIQUE constraint), from the appropriate reference table. Returns a named
# character vector (names = feature_name). This is what lets a kstest run
# use every gene difexp actually measured, not just the ones that happen to
# also be in the signature's own curated feature set (see
# resolve_single_enrichment_query()).
lookup_gene_symbols_by_feature_name <- function(conn, ref_table, feature_names, organism_id) {
  feature_names <- base::unique(feature_names[!base::is.na(feature_names) & base::nzchar(feature_names)])
  if (base::length(feature_names) == 0 || base::is.na(organism_id)) {
    return(base::character())
  }

  query <- base::sprintf(
    "SELECT feature_name, gene_symbol FROM %s WHERE organism_id = %d AND feature_name IN (%s)",
    ref_table, base::as.integer(organism_id), base::paste(DBI::dbQuoteLiteral(conn, feature_names), collapse = ",")
  )
  tbl <- DBI::dbGetQuery(conn, query)
  symbols <- base::trimws(base::as.character(tbl$gene_symbol))
  stats::setNames(symbols, base::as.character(tbl$feature_name))
}

# Builds the query hypeR::hypeR() expects for a single signature:
#   - "hypergeometric": an unnamed character vector of gene symbols, from
#     the signature's stored (already-curated) feature set.
#   - "kstest" (rank-based/GSEA-style): a named numeric vector, gene symbol
#     -> score, from the signature's *difexp* table -- the full unfiltered
#     ranked results (typically hundreds-to-thousands of rows), not the
#     curated signature subset, which is what a rank statistic actually
#     needs for real statistical power. Requires has_difexp = 1. Gene
#     symbols are resolved, in order of preference: (1) difexp's own
#     gene_symbol column, if present; (2) difexp's own feature_name column,
#     resolved against the reference table by (feature_name, organism_id);
#     (3) for older difexp shapes with neither, difexp rows are matched
#     back to gene symbols via their shared probe_id with the signature's
#     own (much smaller) curated feature set -- a real fallback, but one
#     that silently discards every difexp row outside that curated subset,
#     so it should only kick in when (1)/(2) genuinely aren't available.
#
# Returns list(ok = FALSE, reason, message?, signature_name = NULL or the
# resolved name if the failure happened after it was looked up) or
# list(ok = TRUE, query = <vector>, signature_name = ...).
resolve_single_enrichment_query <- function(auth, signature_hashkey, test, difexp_dir) {
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = TRUE,
    max_features = 5000,
    auth = auth
  )
  if (base::is.null(context)) {
    return(base::list(ok = FALSE, reason = "not_found", signature_name = NULL))
  }

  signature_name <- context$signature$signature_name
  assay_type <- context$signature$assay_type
  ref_table <- enrichment_reference_table(assay_type)
  if (base::is.null(ref_table)) {
    return(base::list(
      ok = FALSE, reason = "unsupported_assay_type", signature_name = signature_name,
      message = base::sprintf("Enrichment is not supported for assay_type = '%s'.", assay_type)
    ))
  }
  if (base::length(context$features) == 0) {
    return(base::list(ok = FALSE, reason = "no_features", signature_name = signature_name))
  }

  probe_ids <- base::vapply(context$features, function(f) base::as.character(f$probe_id %||% NA), character(1))
  feature_ids <- base::vapply(context$features, function(f) base::suppressWarnings(base::as.integer(f$feature_id %||% NA)), integer(1))

  conn <- NULL
  symbol_by_feature_id <- base::tryCatch({
    conn <- db_connect_local()
    lookup_gene_symbols(conn, ref_table, feature_ids)
  }, finally = {
    if (!base::is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })

  # probe_id -> gene_symbol, for joining difexp rows (which carry probe_id,
  # not feature_id) back to a symbol.
  symbol_by_probe_id <- stats::setNames(
    symbol_by_feature_id[base::as.character(feature_ids)],
    probe_ids
  )

  if (identical(test, "hypergeometric")) {
    query_vector <- base::unique(symbol_by_probe_id[!base::is.na(symbol_by_probe_id) & base::nzchar(symbol_by_probe_id)])
    query_vector <- base::unname(query_vector)
    if (base::length(query_vector) == 0) {
      return(base::list(ok = FALSE, reason = "no_gene_symbols", signature_name = signature_name))
    }
    return(base::list(ok = TRUE, query = query_vector, signature_name = signature_name))
  }

  # kstest
  if (!base::isTRUE(base::as.logical(context$signature$has_difexp))) {
    return(base::list(ok = FALSE, reason = "no_difexp", signature_name = signature_name))
  }

  difexp_tbl <- load_difexp_rds(difexp_dir, signature_hashkey)
  if (base::is.null(difexp_tbl) || !base::is.data.frame(difexp_tbl) || base::nrow(difexp_tbl) == 0) {
    return(base::list(ok = FALSE, reason = "no_difexp", signature_name = signature_name))
  }

  score_col <- if ("score" %in% base::colnames(difexp_tbl)) "score" else NULL
  if (base::is.null(score_col)) {
    return(base::list(
      ok = FALSE, reason = "unsupported_difexp_shape", signature_name = signature_name,
      message = base::sprintf(
        "difexp for this signature does not have the expected 'score' column (has: %s).",
        base::paste(base::colnames(difexp_tbl), collapse = ", ")
      )
    ))
  }
  difexp_scores <- base::suppressWarnings(base::as.numeric(difexp_tbl[[score_col]]))

  difexp_symbols <- NULL
  if ("gene_symbol" %in% base::colnames(difexp_tbl)) {
    candidate <- base::trimws(base::as.character(difexp_tbl$gene_symbol))
    if (base::any(base::nzchar(candidate) & !base::is.na(candidate))) {
      difexp_symbols <- candidate
    }
  }
  if (base::is.null(difexp_symbols) && "feature_name" %in% base::colnames(difexp_tbl)) {
    organism_id <- base::suppressWarnings(base::as.integer(context$signature$organism_id))
    feature_names <- base::trimws(base::as.character(difexp_tbl$feature_name))
    conn2 <- NULL
    symbol_by_feature_name <- base::tryCatch({
      conn2 <- db_connect_local()
      lookup_gene_symbols_by_feature_name(conn2, ref_table, feature_names, organism_id)
    }, finally = {
      if (!base::is.null(conn2)) base::suppressWarnings(DBI::dbDisconnect(conn2))
    })
    candidate <- base::unname(symbol_by_feature_name[feature_names])
    if (base::any(!base::is.na(candidate) & base::nzchar(candidate))) {
      difexp_symbols <- candidate
    }
  }
  if (base::is.null(difexp_symbols)) {
    if (!"probe_id" %in% base::colnames(difexp_tbl)) {
      return(base::list(
        ok = FALSE, reason = "unsupported_difexp_shape", signature_name = signature_name,
        message = base::sprintf(
          "difexp for this signature does not have a gene_symbol, feature_name, or probe_id column to resolve gene symbols from (has: %s).",
          base::paste(base::colnames(difexp_tbl), collapse = ", ")
        )
      ))
    }
    difexp_probe_ids <- base::as.character(difexp_tbl$probe_id)
    difexp_symbols <- base::unname(symbol_by_probe_id[difexp_probe_ids])
  }

  keep <- !base::is.na(difexp_symbols) & base::nzchar(difexp_symbols) & !base::is.na(difexp_scores)
  ranked <- stats::aggregate(difexp_scores[keep], by = base::list(symbol = difexp_symbols[keep]), FUN = function(x) x[base::which.max(base::abs(x))])
  if (base::nrow(ranked) == 0) {
    return(base::list(ok = FALSE, reason = "no_gene_symbols", signature_name = signature_name))
  }
  ranked <- ranked[base::order(-ranked$x), ]
  query_vector <- stats::setNames(ranked$x, ranked$symbol)

  base::list(ok = TRUE, query = query_vector, signature_name = signature_name)
}

# Resolves multiple signatures for a single hypeR run. Signatures that fail
# to resolve (wrong assay type, no difexp for a kstest run, etc.) are
# skipped rather than failing the whole request -- this is what makes
# "run enrichment on the basket" usable when the basket mixes signatures
# that do and don't qualify for the chosen test.
#
# Returns list(queries = <named list, name = disambiguated display label>,
# resolved = list(list(signature_hashkey=, signature_name=, label=,
# n_query=), ...), skipped = list(list(signature_hashkey=,
# signature_name=, reason=, message=), ...)).
resolve_enrichment_queries <- function(auth, signature_hashkeys, test, difexp_dir) {
  queries <- base::list()
  resolved <- base::list()
  skipped <- base::list()
  used_labels <- base::character()

  for (signature_hashkey in signature_hashkeys) {
    single <- resolve_single_enrichment_query(auth, signature_hashkey, test, difexp_dir)
    if (!single$ok) {
      skipped[[base::length(skipped) + 1]] <- base::list(
        signature_hashkey = signature_hashkey,
        signature_name = single$signature_name,
        reason = single$reason,
        message = single$message %||% NULL
      )
      next
    }

    label <- single$signature_name
    if (label %in% used_labels) {
      suffix <- 2
      candidate <- base::sprintf("%s (%d)", label, suffix)
      while (candidate %in% used_labels) {
        suffix <- suffix + 1
        candidate <- base::sprintf("%s (%d)", label, suffix)
      }
      label <- candidate
    }
    used_labels <- c(used_labels, label)

    queries[[label]] <- single$query
    resolved[[base::length(resolved) + 1]] <- base::list(
      signature_hashkey = signature_hashkey,
      signature_name = single$signature_name,
      label = label,
      n_query = base::length(single$query)
    )
  }

  base::list(queries = queries, resolved = resolved, skipped = skipped)
}

# Renders hypeR's own hyp_dots() (a ggplot object) to a PNG and returns it
# as a base64 data URI, so the frontend gets the actual hypeR plot rather
# than a reimplementation. `hyp` is always a multihyp here (run_enrichment()
# always calls hypeR::hypeR() with a named list, even for one signature --
# see resolve_enrichment_queries()), so hyp_dots() always returns a named
# list of per-signature ggplots. merge = TRUE combines them into one plot,
# but merge = TRUE panics if there's only one signature to plot, hence the
# branch. Returns NULL if there's nothing to plot (e.g. hyp_dots() itself
# errors on an empty result set).
# hyp_dots() draws the single-signature FDR axis with
# scale_y_continuous(trans = log10_trans()).
# Under ggplot2 4.x the break POSITIONS come back transformed a second time
# while the labels do not, so on real data the axis reads:
#
#   break labels    40      80      120        (sensible -log10(FDR) values)
#   break positions -1.602  -1.903  -2.079     (= -log10 of those labels)
#   panel range     -2.2 .. 59.9
#
# All three ticks land within half a unit of each other at the extreme left of
# a 62-unit panel and overprint into an unreadable smudge, which is what the
# plot has been showing. Nothing about the figure's size causes it and no
# amount of resizing fixes it.
#
# Replace that one scale with a reverse-log axis -- hypeR's own
# .reverselog_trans(10), which it defines for this purpose -- so positions and
# labels agree and the most significant gene sets stay on the right, the
# orientation hyp_dots intends. Everything else about the plot is left as
# hypeR built it.
#
# Only applies to the one-signature plot. The merged multi-signature plot is a
# different geometry -- aes(x = signature, y = label), gene sets down the y
# axis and significance in the colour -- with no continuous y to correct, and
# forcing a continuous scale onto that discrete axis breaks the plot outright.
.fix_hyp_dots_scale <- function(plot_obj) {
  base::tryCatch(
    base::suppressMessages(
      plot_obj + ggplot2::scale_y_continuous(
        trans = scales::trans_new(
          "reverselog-10",
          transform = function(x) -base::log(x, 10),
          inverse = function(x) 10^(-x),
          breaks = scales::log_breaks(base = 10),
          domain = c(1e-100, Inf)
        ),
        labels = function(v) base::format(v, scientific = TRUE, digits = 2)
      )
    ),
    error = function(e) plot_obj
  )
}

# The rows hyp_dots() will actually draw, and the longest label among them,
# for a hyp or a multihyp. Both drive the canvas size below.
.hyp_dots_extent <- function(hyp, fdr, top = 30) {
  frames <- if (methods::is(hyp, "multihyp")) {
    base::lapply(hyp$data, function(h) h$data)
  } else {
    base::list(hyp$data)
  }
  rows <- 0L
  longest <- 0L
  for (df in frames) {
    if (!base::is.data.frame(df) || base::nrow(df) == 0) next
    keep <- if ("fdr" %in% base::colnames(df)) df[!base::is.na(df$fdr) & df$fdr <= fdr, , drop = FALSE] else df
    n <- base::min(base::nrow(keep), top)
    rows <- rows + n
    if (n > 0 && "label" %in% base::colnames(keep)) {
      longest <- base::max(longest, base::max(base::nchar(base::as.character(utils::head(keep$label, n))), 0L))
    }
  }
  base::list(rows = rows, longest_label = longest, panels = base::length(frames))
}

# hypeR's own dot plot, rendered server-side to a data: URI.
#
# The canvas is sized from the plot's contents rather than fixed. It used to be
# a hard 900x520 at 130 dpi -- 6.9 x 4.0 inches -- for up to 30 gene sets,
# which crushed the rows together and squeezed the panel so hard that ggplot's
# x-axis tick labels overprinted into an unreadable smear. Gene set names run
# past 40 characters (HALLMARK_OXIDATIVE_PHOSPHORYLATION is 35), and the label
# column is drawn at full size regardless, so the space left for the panel
# shrinks as the names get longer.
render_hyp_dots_png <- function(hyp, fdr, res = 130,
                                row_height = 0.30, min_height = 3.2, max_height = 22,
                                label_char_width = 0.085, panel_width = 5.0, max_width = 20) {
  base::tryCatch({
    # hyp_dots() behaves differently for each shape hypeR can hand back, and
    # every combination below is load-bearing:
    #
    #   object              merge = TRUE          merge = FALSE
    #   hyp (single)        bare ggplot           bare ggplot  (merge ignored)
    #   multihyp (1 sig)    ERROR: rownames       list of 1
    #   multihyp (N sigs)   bare ggplot           list of N
    #
    # This used to branch on `length(hyp$data) > 1`, which is ambiguous: on a
    # multihyp that is the signature count, but on a hyp it is the COLUMN COUNT
    # of a data frame (8). It picked a working combination for all three shapes
    # by coincidence of two unrelated numbers, not by design -- a hyp landed on
    # merge = TRUE and a one-signature multihyp on merge = FALSE, which happen
    # to be the two that work. Ask the object what it is instead.
    is_multi <- methods::is(hyp, "multihyp")
    n_signatures <- if (is_multi) base::length(hyp$data) else 1L

    # Merging is only meaningful, and only safe, for a multihyp holding more
    # than one signature: it is the side-by-side comparison. A one-signature
    # multihyp raises "attempt to set 'rownames'", and a bare hyp ignores the
    # argument entirely.
    merged <- is_multi && n_signatures > 1L
    drawn <- hypeR::hyp_dots(hyp, top = 30, fdr = fdr, merge = merged)

    # Branch on what came BACK, not on what went in. Indexing [[1]] into a bare
    # ggplot raises "subscript out of bounds" under ggplot2 4.x, where a ggplot
    # is an S7 object and [[ dispatches to S7::prop(x, "meta").
    plot_obj <- if (ggplot2::is_ggplot(drawn)) {
      drawn
    } else if (base::is.list(drawn) && base::length(drawn) > 0) {
      drawn[[1]]
    } else {
      NULL
    }
    if (base::is.null(plot_obj)) return(NULL)

    # The axis correction belongs to the per-signature geometry
    # (x = label, y = significance). The merged plot is the other one --
    # x = signature, y = label, significance in the colour -- with no
    # continuous y to correct.
    if (!merged) plot_obj <- .fix_hyp_dots_scale(plot_obj)

    extent <- .hyp_dots_extent(hyp, fdr)
    if (extent$rows == 0) return(NULL)

    height <- base::max(min_height, base::min(max_height, 1.6 + extent$rows * row_height))
    width <- base::max(7.0, base::min(max_width, panel_width + extent$longest_label * label_char_width))

    tmp <- base::tempfile(fileext = ".png")
    on.exit(base::unlink(tmp), add = TRUE)
    ggplot2::ggsave(tmp, plot = plot_obj, width = width, height = height, dpi = res, units = "in", bg = "white",
                    limitsize = FALSE)
    raw_bytes <- base::readBin(tmp, "raw", file.info(tmp)$size)
    base::sprintf("data:image/png;base64,%s", jsonlite::base64_enc(raw_bytes))
  }, error = function(err) NULL)
}

# Runs hypeR::hypeR() across one or more signatures at once (hypeR's own
# multi-signature support -- a named list in, a `multihyp` out) and shapes
# the combined result table for the API response. There is no dotplot here
# (or anywhere in this response) any more -- that figure now lives behind
# GET /annotate/dotplot, rendered on demand from the hyp object
# run_enrichment_hyp_object() below builds, not returned with every run.
# Signatures that fail to resolve for the requested test are skipped (see
# resolve_enrichment_queries()) rather than failing the whole request,
# unless *none* resolve.
#
# Returns list(ok = FALSE, reason, message, skipped?) or
# list(ok = TRUE, resolved = <list of {signature_hashkey, signature_name,
# label, n_query}>, skipped = <list, same shape as above>,
# results = <data.frame, one row per (signature, geneset) hit, with a
# signature_label column identifying which input signature it came from>,
# geneset_source = "cache" | "live").
# "gsea" is not a hypeR test name -- hypeR offers hypergeometric and kstest,
# and GSEA is the kstest with hit weighting turned on. power = 1 weights each
# hit by its score (what GSEA means); power = 0 is the classic unweighted KS
# statistic. Keeping them as separate choices matches the Shiny app, which
# offered "KS Test" and "GSEA" as distinct methods.
.enrichment_hyper_test <- function(test) {
  if (identical(test, "hypergeometric")) "hypergeometric" else "kstest"
}
.enrichment_power <- function(test) {
  if (identical(test, "gsea")) 1 else 0
}

# Resolve queries and run hypeR, stopping at the hyp object.
#
# GET /annotate/dotplot needs the hyp object itself, not the tables
# run_enrichment() derives from it. Factored out rather than caching hyp
# objects server-side: caching would mean per-caller state in a single-process
# Plumber API for a figure most runs never download.
run_enrichment_hyp_object <- function(auth, signature_hashkeys, test = c("hypergeometric", "kstest", "gsea"),
                                      species = "Homo sapiens", collection = "H", subcollection = NULL,
                                      fdr = 0.05, difexp_dir, msigdb_cache_dir) {
  test <- base::match.arg(test)
  signature_hashkeys <- base::unique(signature_hashkeys[base::nzchar(signature_hashkeys)])
  if (base::length(signature_hashkeys) == 0) {
    return(base::list(ok = FALSE, reason = "no_signatures", message = "Select at least one signature."))
  }

  resolved <- resolve_enrichment_queries(auth, signature_hashkeys, test, difexp_dir)
  if (base::length(resolved$queries) == 0) {
    first <- resolved$skipped[[1]]
    # Carry `skipped` out: run_enrichment()'s error path reports which
    # signatures were dropped and why, and losing it would silently turn a
    # partial failure into an unexplained one.
    return(base::list(ok = FALSE, reason = first$reason %||% "enrichment_failed",
                      message = first$message, skipped = resolved$skipped))
  }

  geneset_result <- resolve_msigdb_genesets(msigdb_cache_dir, species, collection, subcollection %||% "")
  if (!geneset_result$ok) {
    return(base::list(ok = FALSE, reason = geneset_result$reason, message = geneset_result$message))
  }

  hyp <- base::tryCatch(
    hypeR::hypeR(
      signature = resolved$queries,
      genesets = geneset_result$genesets,
      test = .enrichment_hyper_test(test),
      power = .enrichment_power(test),
      fdr = fdr,
      plotting = FALSE,
      quiet = TRUE
    ),
    error = function(err) structure(base::list(message = err$message), class = "enrichment_run_error")
  )
  if (base::inherits(hyp, "enrichment_run_error")) {
    return(base::list(ok = FALSE, reason = "enrichment_failed", message = hyp$message))
  }

  base::list(ok = TRUE, hyp = hyp, resolved = resolved, geneset_source = geneset_result$source)
}

run_enrichment <- function(auth, signature_hashkeys, test = c("hypergeometric", "kstest", "gsea"),
                            species = "Homo sapiens", collection = "H", subcollection = NULL,
                            fdr = 0.05, difexp_dir, msigdb_cache_dir) {
  test <- base::match.arg(test)
  signature_hashkeys <- base::unique(signature_hashkeys[base::nzchar(signature_hashkeys)])
  if (base::length(signature_hashkeys) == 0) {
    return(base::list(ok = FALSE, reason = "no_signatures", message = "Select at least one signature."))
  }

  built <- run_enrichment_hyp_object(
    auth = auth, signature_hashkeys = signature_hashkeys, test = test,
    species = species, collection = collection, subcollection = subcollection,
    fdr = fdr, difexp_dir = difexp_dir, msigdb_cache_dir = msigdb_cache_dir
  )
  if (!base::isTRUE(built$ok)) {
    return(base::list(ok = FALSE, reason = built$reason, message = built$message,
                      skipped = built$skipped %||% base::list()))
  }
  hyp <- built$hyp
  resolved <- built$resolved
  geneset_result <- base::list(source = built$geneset_source)

  # Keep the hyp object's own shape: one entry per signature, carrying that
  # signature's hyp$info and its own results table.
  #
  # These used to be flattened into a single table with a signature_label
  # column, which loses what hypeR reports per signature -- signature size,
  # geneset collection, background -- and forces the reader to disentangle
  # several signatures' gene sets from one interleaved list. hypeR's own
  # rctbl_mhyp() presents a multihyp as a row per signature that expands to
  # that signature's results, and that is the structure this mirrors.
  per_signature <- base::lapply(resolved$resolved, function(meta) {
    one <- hyp$data[[meta$label]]
    df <- if (base::is.null(one)) NULL else one$data
    if (!base::is.data.frame(df) || base::nrow(df) == 0) {
      df <- base::data.frame()
    } else {
      df <- df[base::order(df$pval), , drop = FALSE]
    }
    base::list(
      signature_hashkey = meta$signature_hashkey,
      signature_name = meta$signature_name,
      label = meta$label,
      n_query = meta$n_query,
      n_enriched = base::nrow(df),
      # hyp$info verbatim: hypeR's own reproducibility record (version,
      # signature size/type, geneset collection, background, cutoffs, test).
      info = if (base::is.null(one)) base::list() else base::lapply(one$info, base::as.character),
      results = compact_table(df, max_rows = 500)
    )
  })

  base::list(
    ok = TRUE,
    resolved = resolved$resolved,
    skipped = resolved$skipped,
    signatures = per_signature,
    geneset_source = geneset_result$source
  )
}

# User-facing name for a requested test, for the messages below -- "GSEA"
# and "the KS test" read better than the raw "gsea" / "kstest" tokens in a
# sentence aimed at a human caller. Falls back to the raw token for
# anything unrecognized rather than guessing at a name for it.
.enrichment_test_display_name <- function(test) {
  base::switch(test %||% "",
    kstest = "the KS test",
    gsea = "GSEA",
    hypergeometric = "the hypergeometric test",
    test
  )
}

# Fallback text for reason = "no_difexp". Named for the requested test when
# one is supplied, so the caller learns both what is missing and what to do
# about it -- switch to hypergeometric, which is resolved from the
# signature's curated feature set and never reads difexp in the first
# place (see resolve_single_enrichment_query()). Without a test (NULL,
# the default), falls back to the older test-agnostic wording.
.enrichment_no_difexp_message <- function(test) {
  if (base::is.null(test)) {
    return("The selected signature has no stored difexp, which rank-based enrichment (KS test and GSEA) requires.")
  }
  base::sprintf(
    "This signature has no stored differential-expression table, which %s requires. Try the hypergeometric test, which uses the signature's feature set instead.",
    .enrichment_test_display_name(test)
  )
}

# Fallback text for reason = "unsupported_difexp_shape". In practice
# resolve_single_enrichment_query() always supplies its own dynamic
# `message` for this reason (naming the actual columns found on the
# offending difexp table), so this fallback is mostly a safety net -- but
# it follows the same test-aware shape as .enrichment_no_difexp_message()
# above for the (unlikely) case it is what actually gets shown.
.enrichment_unsupported_difexp_shape_message <- function(test) {
  if (base::is.null(test)) {
    return("The selected signature's stored difexp is not in a shape rank-based enrichment (KS test and GSEA) can use.")
  }
  base::sprintf(
    "This signature's stored differential-expression table is not in a shape %s can use. Try the hypergeometric test, which uses the signature's feature set instead.",
    .enrichment_test_display_name(test)
  )
}

# Maps a failed hypeR-based enrichment result's `reason` (as produced by
# resolve_single_enrichment_query(), resolve_enrichment_queries(),
# run_enrichment_hyp_object(), and run_enrichment() above) to an HTTP status
# and a human-readable message. POST /annotate/run (its non-GEM branch) and
# GET /annotate/dotplot both call run_enrichment_hyp_object() and must report
# the same failure the same way, so this is the one place that mapping is
# written down rather than kept as two copies that can drift. The GEM branch
# of /annotate/run runs a different pipeline (see run_gem_enrichment()) with
# its own reasons and status codes and does not use this.
#
# `test` is the test the caller actually requested ("hypergeometric" /
# "kstest" / "gsea"). It never changes the status code -- a signature that
# can't satisfy a rank-based test is equally un-satisfiable whether the
# caller asked for kstest or gsea -- it only lets the no_difexp /
# unsupported_difexp_shape fallback messages name the test that failed and
# point at the way out (hypergeometric, which needs the feature set, not
# difexp; see resolve_single_enrichment_query()). Optional (NULL default)
# so any call site that can't supply it keeps working.
#
# Status mapping principle -- extend this table when a new reason is added
# rather than inventing a one-off status elsewhere:
#   404 - the thing genuinely is not there: not_found, no_features,
#         not_cached
#   422 - it exists, but cannot satisfy THIS request: no_difexp,
#         unsupported_difexp_shape, no_gene_symbols, unsupported_assay_type
#   400 - the request itself is malformed: no_signatures, invalid_geneset
#   502 - an upstream fetch failed: fetch_failed
#
# An explicit `message` -- most reasons carry one; see the functions above --
# always wins over the generic fallback text, since it can say things this
# can't reconstruct from `reason` alone (e.g. which difexp columns were
# actually found).
#
# Returns list(status = <integer>, message = <string>).
enrichment_error_response <- function(reason, message = NULL, test = NULL) {
  status <- base::switch(reason %||% "",
    not_found = 404L,
    no_features = 404L,
    not_cached = 404L,
    no_difexp = 422L,
    unsupported_difexp_shape = 422L,
    no_gene_symbols = 422L,
    unsupported_assay_type = 422L,
    no_signatures = 400L,
    invalid_geneset = 400L,
    fetch_failed = 502L,
    500L
  )
  fallback_message <- base::switch(reason %||% "",
    not_found = "No signature found for the selected signature(s).",
    no_features = "The selected signature has no stored features.",
    no_difexp = .enrichment_no_difexp_message(test),
    unsupported_difexp_shape = .enrichment_unsupported_difexp_shape_message(test),
    no_gene_symbols = "None of the selected signature's features could be mapped to a gene symbol.",
    "Enrichment failed."
  )
  base::list(status = status, message = message %||% fallback_message)
}
