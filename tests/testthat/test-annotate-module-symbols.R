# The Shiny Annotate module's signature builders, exercised without Shiny or a
# database: the reference-table lookup is injected as a plain function with
# the contract lookup(feature_names, organism, assay_type) -> named character
# vector (names = feature_name, value = gene symbol, blank/NA = unknown).
source(testthat::test_path("../../api/lib/symbols.R"), local = FALSE)
source(testthat::test_path("../../legacy_app/modules/annotate_module.R"), local = FALSE)

make_sig <- function(name, feature_names, scores, assay = "transcriptomics", organism = "Homo sapiens", difexp = NULL) {
  list(
    metadata = list(signature_name = name, assay_type = assay, organism = organism),
    signature = data.frame(probe_id = feature_names, feature_name = feature_names, score = scores,
                           group_label = "All Features", stringsAsFactors = FALSE),
    difexp = difexp
  )
}
sig_entry <- function(name) stats::setNames(list(list(signature_name = name, signature_id = 1)), name)

recording_lookup <- function(map) {
  calls <- list()
  list(
    f = function(feature_names, organism, assay_type) {
      calls[[length(calls) + 1]] <<- list(feature_names = feature_names, organism = organism, assay_type = assay_type)
      map[base::intersect(feature_names, names(map))]
    },
    calls = function() calls
  )
}

ens <- c("ENSG00000141510", "ENSG00000135679", "ENSG00000000003")
ref <- c(ENSG00000141510 = "TP53", ENSG00000135679 = "MDM2", ENSG00000000003 = "")

test_that("hypergeometric builder resolves Ensembl feature names through the reference lookup", {
  lk <- recording_lookup(ref)
  out <- build_enrichment_signatures(list(make_sig("S", ens, c(2, -1.5, 3))), sig_entry("S"), symbol_lookup = lk$f)
  genes <- unique(unlist(out$vectors))
  expect_setequal(genes, c("TP53", "MDM2"))
  expect_length(lk$calls(), 1)
  expect_equal(lk$calls()[[1]]$organism, "Homo sapiens")
  expect_equal(lk$calls()[[1]]$assay_type, "transcriptomics")
  expect_false(any(grepl("raw feature_name", out$notes)))
  expect_true(any(grepl("2 of 3", out$notes)))
})

test_that("the reference lookup is not consulted when the difexp already carries symbols", {
  lk <- recording_lookup(ref)
  difexp <- data.frame(probe_id = ens, symbol = c("TP53", "MDM2", "TSPAN6"), score = c(2, -1.5, 3), stringsAsFactors = FALSE)
  out <- build_enrichment_signatures(list(make_sig("S", ens, c(2, -1.5, 3), difexp = difexp)), sig_entry("S"), symbol_lookup = lk$f)
  expect_setequal(unique(unlist(out$vectors)), c("TP53", "MDM2", "TSPAN6"))
  expect_length(lk$calls(), 0)
  expect_length(out$notes, 0)
})

test_that("hypergeometric builder falls back to raw identifiers with the warning when the reference has nothing", {
  lk <- recording_lookup(character())
  out <- build_enrichment_signatures(list(make_sig("S", ens, c(2, -1.5, 3))), sig_entry("S"), symbol_lookup = lk$f)
  expect_setequal(unique(unlist(out$vectors)), ens)
  expect_true(any(grepl("raw feature_name", out$notes)))
})

test_that("proteomics signatures never go through the transcriptomics reference lookup", {
  lk <- recording_lookup(c(P04637 = "P53"))
  out <- build_enrichment_signatures(list(make_sig("P", c("P04637", "Q00987"), c(1, -1), assay = "proteomics")), sig_entry("P"), symbol_lookup = lk$f)
  expect_length(lk$calls(), 0)
  expect_setequal(unique(unlist(out$vectors)), c("P04637", "Q00987"))
})

test_that("ranked builder resolves difexp feature names through the reference lookup when the difexp has no symbol column", {
  lk <- recording_lookup(ref)
  difexp <- data.frame(probe_id = ens, feature_name = ens, score = c(2, -1.5, 3), group_label = "All Features", stringsAsFactors = FALSE)
  out <- build_ranked_enrichment_signatures(list(make_sig("S", ens, c(2, -1.5, 3), difexp = difexp)), sig_entry("S"), mode = "gsea", symbol_lookup = lk$f)
  ranked <- out$vectors[[1]]
  expect_setequal(names(ranked), c("TP53", "MDM2"))
  expect_equal(unname(ranked[["TP53"]]), 2)
  expect_length(lk$calls(), 1)
  expect_true(any(grepl("2 of 3", out$notes)))
})

test_that("ranked builder prefers the difexp's own symbol column over the reference lookup", {
  lk <- recording_lookup(ref)
  difexp <- data.frame(probe_id = ens, feature_name = ens, symbol = c("TP53", "MDM2", "TSPAN6"), score = c(2, -1.5, 3), group_label = "All Features", stringsAsFactors = FALSE)
  out <- build_ranked_enrichment_signatures(list(make_sig("S", ens, c(2, -1.5, 3), difexp = difexp)), sig_entry("S"), mode = "gsea", symbol_lookup = lk$f)
  expect_setequal(names(out$vectors[[1]]), c("TP53", "MDM2", "TSPAN6"))
  expect_length(lk$calls(), 0)
})

test_that("ranked builder without a lookup keeps ranking raw feature names, as before", {
  difexp <- data.frame(probe_id = ens, feature_name = ens, score = c(2, -1.5, 3), group_label = "All Features", stringsAsFactors = FALSE)
  out <- build_ranked_enrichment_signatures(list(make_sig("S", ens, c(2, -1.5, 3), difexp = difexp)), sig_entry("S"), mode = "gsea")
  expect_setequal(names(out$vectors[[1]]), ens)
})
