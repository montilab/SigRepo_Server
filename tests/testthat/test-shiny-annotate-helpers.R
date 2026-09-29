# Helpers behind the Shiny Annotate tab (legacy_app/utils/annotate_utils.R).
# No database: signatures come from SigRepo's LLFS example, genesets are the
# synthetic fixture in helper-shiny-annotate.R.

load_annotate_app()

# ---- defaults -------------------------------------------------------------------

test_that("the tab's defaults are runHypeR()'s defaults", {
  skip_without_hyper_client()
  formal_defaults <- formals(SigRepo::runHypeR)
  # organism and test are required: runHypeR() gives them no default.
  expect_identical(formal_defaults$organism, quote(expr = ))
  expect_identical(formal_defaults$test, quote(expr = ))
  expect_false(any(c("direction", "absolute") %in% c(names(formal_defaults), names(ANNOTATE_DEFAULTS))))
  for (name in setdiff(names(ANNOTATE_DEFAULTS), c("msigdb_clean", "test"))) {
    expected <- eval(formal_defaults[[name]])
    if (name %in% c("ks_source", "fdr_scope")) {
      expected <- expected[1]
    }
    expect_identical(ANNOTATE_DEFAULTS[[name]], expected, info = name)
  }
  expect_identical(ANNOTATE_DEFAULTS$msigdb_clean, eval(formal_defaults$msigdb_clean))
  expect_identical(ANNOTATE_FGSEA_DEFAULTS, SigRepo:::HYPER_FGSEA_DEFAULT_ARGS)
  expect_identical(ANNOTATE_PROVENANCE_KEYS, SigRepo:::HYPER_PROVENANCE_KEYS)
})

test_that("default labels print the value", {
  expect_identical(annotate_default_text("fdr_scope"), "default (run)")
  expect_identical(annotate_default_text("split"), "default (on)")
  expect_identical(annotate_default_text("background"), "default (auto)")
  expect_identical(annotate_default_text("maxSize"), "default (Inf)")
})

# ---- gene lists -------------------------------------------------------------------

test_that("plain text is one gene list named genes", {
  expect_identical(annotate_parse_gene_lists("TP53, MYC\nCDK4 E2F1;TP53"), list(genes = c("TP53", "MYC", "CDK4", "E2F1")))
  expect_identical(annotate_parse_gene_lists(""), list())
  expect_identical(annotate_parse_gene_lists(NULL), list())
})

test_that("headers split blocks and scored lines make ranked lists", {
  lists <- annotate_parse_gene_lists("# up\nTP53\nMYC\n\n>ranked\nIL6 2.5\nTNF\t-1e-2\nCXCL8,0")
  expect_identical(names(lists), c("up", "ranked"))
  expect_identical(lists$up, c("TP53", "MYC"))
  expect_identical(lists$ranked, c(IL6 = 2.5, TNF = -0.01, CXCL8 = 0))
})

test_that("malformed gene lists are errors that name the list", {
  expect_error(annotate_parse_gene_lists("# a\nTP53 1\nMYC"), "'a' mixes")
  expect_error(annotate_parse_gene_lists("# a\nTP53\n# a\nMYC"), "'a' appears more than once")
  expect_error(annotate_parse_gene_lists("# a\n# b\nMYC"), "'a' has no genes")
  expect_error(annotate_parse_gene_lists("#\nMYC"), "needs a name")
  expect_error(annotate_parse_gene_lists("# r\nTP53 1\nTP53 2"), "scores TP53 more than once")
})

test_that("gene list files: GMT, CSV with list and score columns, and plain text", {
  gmt <- tempfile(fileext = ".gmt")
  writeLines(c("SET_A\tdesc\tTP53\tMYC", "SET_B\tdesc\tIL6"), gmt)
  expect_identical(annotate_read_gene_list_file(gmt, "sets.gmt"), list(SET_A = c("TP53", "MYC"), SET_B = "IL6"))

  csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(list = c("x", "x", "y"), Gene_Symbol = c("TP53", "MYC", "IL6"), score = c(1, -2, 3)), csv, row.names = FALSE)
  expect_identical(annotate_read_gene_list_file(csv, "ranks.csv"), list(x = c(TP53 = 1, MYC = -2), y = c(IL6 = 3)))

  csv2 <- tempfile(fileext = ".csv")
  write.csv(data.frame(symbol = c("TP53", "MYC")), csv2, row.names = FALSE)
  expect_identical(annotate_read_gene_list_file(csv2, "my genes.csv"), list(`my genes` = c("TP53", "MYC")))

  txt <- tempfile(fileext = ".txt")
  writeLines(c("# q", "TP53", "MYC"), txt)
  expect_identical(annotate_read_gene_list_file(txt, "q.txt"), list(q = c("TP53", "MYC")))
})

test_that("gene lists are checked against what each test needs", {
  sets <- list(a = c("TP53", "MYC"))
  ranked <- list(r = c(TP53 = 1, MYC = -1))
  expect_identical(annotate_check_gene_lists(list(), "kstest"), "Enter at least one gene list.")
  expect_length(annotate_check_gene_lists(c(sets, ranked), "kstest"), 0)
  expect_match(annotate_check_gene_lists(c(sets, ranked), "hypergeometric"), "'r' has scores")
  expect_match(annotate_check_gene_lists(c(sets, ranked), "fgsea"), "'a' has none")
  many <- stats::setNames(rep(sets, 11), sprintf("l%d", 1:11))
  expect_match(annotate_check_gene_lists(many, "hypergeometric"), "at most 10 gene lists")
  expect_identical(
    annotate_gene_list_preview(c(sets, ranked)),
    data.frame(name = c("a", "r"), kind = c("set", "ranked"), n_genes = c(2L, 2L), stringsAsFactors = FALSE)
  )
})

# ---- background ---------------------------------------------------------------------

test_that("every background mode becomes the runHypeR() form", {
  expect_null(annotate_build_background("default"))
  expect_identical(annotate_build_background("number", number = 20000), 20000)
  expect_identical(annotate_build_background("difexp"), "difexp")
  expect_identical(annotate_build_background("genes", genes_text = "TP53, MYC\nIL6"), c("TP53", "MYC", "IL6"))
  per <- data.frame(key = c("11", "12"), mode = c("number", "difexp"), value = c("15000", ""), stringsAsFactors = FALSE)
  expect_identical(annotate_build_background("per_signature", per_signature = per), list(`11` = 15000, `12` = "difexp"))
})

test_that("invalid backgrounds are errors the tab can show", {
  expect_error(annotate_build_background("number", number = NA), "positive number")
  expect_error(annotate_build_background("number", number = -5), "positive number")
  expect_error(annotate_build_background("genes", genes_text = "TP53"), "at least two genes")
  per <- data.frame(key = "LLFS", mode = "number", value = "abc", stringsAsFactors = FALSE)
  expect_error(annotate_build_background("per_signature", per_signature = per), "for 'LLFS' must be a positive number")
  expect_error(annotate_build_background("per_signature", per_signature = per[0, ]), "Add signatures")
})

# ---- genesets -------------------------------------------------------------------------

test_that("the MSigDB picker offers mouse collections only for mouse", {
  human <- annotate_msigdb_collections("Homo sapiens")
  mouse <- annotate_msigdb_collections("Mus musculus")
  expect_true(all(c("H", "C2", "C5") %in% human))
  expect_false(any(c("MH", "M2") %in% human))
  expect_true(all(c("H", "MH", "M5") %in% mouse))
  expect_identical(annotate_msigdb_subcollections("H"), "")
  expect_true("CP:REACTOME" %in% annotate_msigdb_subcollections("C2"))
  expect_identical(annotate_msigdb_subcollections("M5"), c("GO:BP", "GO:CC", "GO:MF", "MPT"))
})

test_that("cached genesets load as gsets from both the list and the table form", {
  skip_if_not_installed("hypeR")
  dir <- tempfile("msigdb-cache-")
  dir.create(dir)
  saveRDS(list(HALLMARK_A = c("TP53", "MYC"), HALLMARK_B = "IL6"), msigdb_cache_file(dir, "Homo sapiens", "H", ""))
  saveRDS(
    data.frame(gs_name = c("GOBP_X", "GOBP_X", "GOBP_Y"), gene_symbol = c("A", "B", "C"), db_version = "2026.1.Hs"),
    msigdb_cache_file(dir, "Homo sapiens", "C5", "GO:BP")
  )

  h <- annotate_load_msigdb("Homo sapiens", "H", "", cache_dir = dir, allow_fetch = FALSE)
  expect_s3_class(h$genesets, "gsets")
  expect_identical(h$genesets$genesets, list(HALLMARK_A = c("TP53", "MYC"), HALLMARK_B = "IL6"))
  expect_identical(h$description$origin, "cache")
  expect_identical(h$description$name, "H")
  expect_identical(h$description$n, 2L)

  go <- annotate_load_msigdb("Homo sapiens", "C5", "GO:BP", clean = TRUE, cache_dir = dir, allow_fetch = FALSE)
  expect_identical(go$genesets$name, "C5.GO:BP")
  expect_identical(go$genesets$version, "2026.1.Hs")
  expect_identical(lengths(go$genesets$genesets), c(X = 2L, Y = 1L))
  expect_match(annotate_genesets_label(go$description), "^C5.GO:BP: 2 genesets \\(Homo sapiens, MSigDB cache 2026.1.Hs\\)$")
})

test_that("a cache miss fetches live only when the server allows it", {
  skip_if_not_installed("hypeR")
  dir <- tempfile("msigdb-cache-")
  dir.create(dir)
  expect_error(
    annotate_load_msigdb("Mus musculus", "MH", "", cache_dir = dir, allow_fetch = FALSE),
    "not in this server's MSigDB cache"
  )

  seen <- NULL
  fetcher <- function(genesets, ...) {
    seen <<- c(list(genesets = genesets), list(...))
    hypeR::gsets$new(list(HALLMARK_Z = "Trp53"), name = "MH", version = "msigdbr 25.1.1", quiet = TRUE)
  }
  mh <- annotate_load_msigdb("Mus musculus", "MH", "", cache_dir = dir, allow_fetch = TRUE, fetcher = fetcher)
  expect_identical(seen, list(genesets = "msigdb", msigdb_species = "Mus musculus", msigdb_collection = "MH",
                              msigdb_subcollection = NULL, msigdb_clean = FALSE))
  expect_identical(mh$description$origin, "msigdbr")
  expect_identical(mh$description$version, "msigdbr 25.1.1")
})

test_that("custom genesets parse from text, GMT and CSV", {
  expect_identical(annotate_parse_custom_geneset_text("mine", "TP53, MYC\nTP53"), list(mine = c("TP53", "MYC")))
  expect_error(annotate_parse_custom_geneset_text("", "TP53"), "name")
  expect_error(annotate_parse_custom_geneset_text("mine", " "), "at least one gene")

  csv <- tempfile(fileext = ".csv")
  write.csv(data.frame(geneset_name = c("s1", "s1", "s2"), gene_symbol = c("A", "B", "C")), csv, row.names = FALSE)
  expect_identical(annotate_parse_custom_geneset_file(csv, "sets.csv"), list(s1 = c("A", "B"), s2 = "C"))

  bad <- tempfile(fileext = ".gmt")
  writeLines("only_a_name", bad)
  expect_error(annotate_parse_custom_geneset_file(bad, "bad.gmt"), "no valid genesets")

  skip_if_not_installed("hypeR")
  custom <- annotate_load_custom(list(mine = c("TP53", "MYC")))
  expect_s3_class(custom$genesets, "gsets")
  expect_identical(custom$description$source, "custom")
  expect_match(annotate_genesets_label(custom$description), "^Custom: 1 genesets \\(custom upload\\)$")
})

# ---- arguments ----------------------------------------------------------------------

annotate_settings <- function(...) utils::modifyList(ANNOTATE_DEFAULTS, utils::modifyList(list(organism = "Homo sapiens"), list(...)))

test_that("organism and test are always sent, first after the signatures", {
  gs <- structure(list(), class = "gsets")
  for (source in c("repository", "upload", "genes")) {
    args <- annotate_build_args(source, "conn", signature_ids = 11, omic_signatures = list(a = 1), gene_lists = list(g = "TP53"),
                                genesets = gs, settings = annotate_settings(organism = "Mus musculus", test = "kstest"))
    expect_identical(args$organism, "Mus musculus", info = source)
    expect_identical(args$test, "kstest", info = source)
    expect_false(any(c("direction", "absolute") %in% names(args)), info = source)
  }
})

test_that("signatures of another organism are a problem, ignoring case", {
  expect_identical(annotate_organism_problem(c("a", "b"), c("Homo sapiens", "homo SAPIENS "), "Homo sapiens"), character())
  expect_identical(annotate_organism_problem("a", "Mus musculus", ""), character())
  expect_identical(
    annotate_organism_problem(c("a", "b", "c"), c("Homo sapiens", "Mus musculus", NA), "Homo sapiens"),
    "A run tests one organism: 'b' (Mus musculus), 'c' (no organism recorded) are not Homo sapiens. Change the organism in Step 2 or run them separately."
  )
  expect_match(annotate_organism_problem("m", "Mus musculus", "Homo sapiens"), "'m' (Mus musculus) is not Homo sapiens. Change the organism in Step 2 or run it separately.", fixed = TRUE)
})

test_that("each source sends only its own signature argument", {
  gs <- structure(list(), class = "gsets")
  repo <- annotate_build_args("repository", conn_handler = "conn", signature_ids = c(11, 12), genesets = gs, settings = annotate_settings())
  expect_identical(repo$conn_handler, "conn")
  expect_identical(repo$signature_id, c(11, 12))
  expect_false(any(c("omic_signature", "signature") %in% names(repo)))

  up <- annotate_build_args("upload", conn_handler = "conn", omic_signatures = list(a = 1), genesets = gs, settings = annotate_settings())
  expect_identical(up$omic_signature, list(a = 1))
  expect_false(any(c("signature_id", "signature") %in% names(up)))

  genes <- annotate_build_args("genes", conn_handler = "conn", gene_lists = list(g = "TP53"), genesets = gs, settings = annotate_settings())
  expect_identical(genes$signature, list(g = "TP53"))
  expect_false(any(c("conn_handler", "signature_id", "omic_signature", "split") %in% names(genes)))
})

test_that("each test sends only the settings it uses", {
  gs <- structure(list(), class = "gsets")
  hyper <- annotate_build_args("repository", "conn", signature_ids = 11, genesets = gs,
                               settings = annotate_settings(split = FALSE, min_query_genes = 2, background = 20000))
  expect_identical(hyper[c("test", "split", "min_query_genes", "background")],
                   list(test = "hypergeometric", split = FALSE, min_query_genes = 2, background = 20000))
  expect_false(any(c("ks_source", "power", "seed", "fgsea_args") %in% names(hyper)))
  expect_false(hyper$verbose)

  ks <- annotate_build_args("repository", "conn", signature_ids = 11, genesets = gs,
                            settings = annotate_settings(test = "kstest", ks_source = "signature"))
  expect_identical(ks[c("ks_source", "score_col", "power")],
                   list(ks_source = "signature", score_col = "score", power = 1))
  expect_false(any(c("split", "min_query_genes", "seed", "background") %in% names(ks)))

  fg <- annotate_build_args("upload", "conn", omic_signatures = list(a = 1), genesets = gs,
                            settings = annotate_settings(test = "fgsea", seed = 7,
                                                         fgsea_args = list(sampleSize = 101, minSize = 15, maxSize = Inf)))
  expect_identical(fg$seed, 7)
  expect_identical(fg$fgsea_args, list(minSize = 15))

  native_ks <- annotate_build_args("genes", gene_lists = list(g = c(A = 1)), genesets = gs, settings = annotate_settings(test = "kstest"))
  expect_false(any(c("ks_source", "score_col") %in% names(native_ks)))
})

test_that("preview arguments are the ones prepareHypeRSignatures() takes", {
  skip_without_hyper_client()
  args <- annotate_build_args("upload", "conn", omic_signatures = list(a = 1), genesets = annotate_fixture_genesets(),
                              settings = annotate_settings(test = "kstest"))
  prepared <- annotate_prepare_args(args)
  expect_true(all(names(prepared) %in% names(formals(SigRepo::prepareHypeRSignatures))))
  expect_false(any(c("genesets", "fdr", "power", "organism") %in% names(prepared)))
  expect_identical(prepared$test, "kstest")
})

test_that("running keeps warnings and returns errors", {
  ok <- annotate_run(list(x = 1), runner = function(x) {
    warning("used background = 23467 instead")
    "result"
  })
  expect_identical(ok$result, "result")
  expect_identical(ok$warnings, "used background = 23467 instead")
  expect_null(ok$error)

  bad <- annotate_run(list(), runner = function() stop("\nNo query is left.\n"))
  expect_null(bad$result)
  expect_identical(bad$error, "No query is left.")

  skip_without_hyper_client()
  preview <- annotate_preview(
    annotate_build_args("upload", NULL, omic_signatures = list(LLFS = annotate_llfs()), genesets = annotate_fixture_genesets(),
                        settings = annotate_settings())
  )
  expect_null(preview$error)
  expect_identical(preview$result$info$query, c("LLFS | Group1", "LLFS | Group2"))
})

# ---- reading results ------------------------------------------------------------------

test_that("results read the same for a hyp and a multihyp", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()
  expect_identical(names(annotate_result_hyps(res$kstest)), c("LLFS_Aging_Gene_2023 | up", "LLFS_Aging_Gene_2023 | down"))
  single <- annotate_result_hyps(res$kstest_single)
  expect_length(single, 1)
  expect_identical(names(single), "LLFS_Aging_Gene_2023 | up")
  expect_identical(annotate_result_test(res$fgsea), "fgsea")
  expect_true(annotate_is_ranked(res$kstest))
  expect_false(annotate_is_ranked(res$hypergeometric))
  expect_error(annotate_result_hyps(list()), "not a hypeR")
})

test_that("the results table stacks every query under its signature", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()
  for (name in names(res)) {
    table <- annotate_results_table(res[[name]])
    hyps <- annotate_result_hyps(res[[name]])
    expect_identical(nrow(table), sum(vapply(hyps, function(h) nrow(h$data), integer(1))), info = name)
    expect_identical(names(table)[1:5], c("query", "signature_name", "group_label", "direction", "label"), info = name)
  }
  hyper <- annotate_results_table(res$hypergeometric)
  expect_setequal(unique(hyper$signature_name), c("LLFS", "LLFS_copy"))
  expect_true(all(c("overlap", "background", "hits") %in% names(hyper)))
  expect_true(all(c("es", "nes", "le") %in% names(annotate_results_table(res$fgsea))))
})

test_that("a query's genesets are listed most significant first", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()$fgsea
  query <- names(annotate_result_hyps(res))[1]
  labels <- annotate_query_genesets(res, query)
  data <- annotate_result_hyps(res)[[query]]$data
  expect_setequal(labels, data$label)
  expect_identical(labels[1], data$label[order(data$fdr, data$pval, data$label)][1])
  expect_identical(annotate_query_genesets(res, "no such query"), character())
})

test_that("the summary counts queries, genesets and significant genesets", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()$hypergeometric
  s <- annotate_summary(res, fdr = 0.05)
  table <- annotate_results_table(res)
  expect_identical(s$test, "hypergeometric")
  expect_identical(s$n_queries, 4L)
  expect_identical(s$n_genesets, 24L)
  expect_identical(s$genesets_name, "FIXTURE (1)")
  expect_identical(s$n_significant, length(unique(table$label[table$fdr <= 0.05])))
  expect_identical(s$fdr_scope, "run")
  expect_true(length(s$backgrounds) >= 1)
})

test_that("dot plot size follows plotHypeRDots()'s documented rule and clamps", {
  expect_identical(annotate_dot_size(NULL, 2), list(width = 7, height = 3.2))
  dots <- data.frame(label = sprintf("SET_%d", 1:10), label_abrv = sprintf("SET_%d", 1:10),
                     query_label = "up", signature_code = "S1", stringsAsFactors = FALSE)
  size <- annotate_dot_size(dots, n_queries = 2)
  expect_equal(size$width, max(7, 5 + 0.085 * 6 + 1.2 * 2))
  expect_equal(size$height, 1.6 + 0.30 * 10 + 0.05 * 2)
  big <- data.frame(label = sprintf("SET_%d", 1:200), label_abrv = strrep("x", 50), query_label = "q",
                    signature_code = "S1", stringsAsFactors = FALSE)
  expect_identical(annotate_dot_size(big, n_queries = 40), list(width = 20, height = 22))
})

test_that("an enrichment summary reports each test's own measures and genes", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()
  fg <- res$fgsea
  query <- names(annotate_result_hyps(fg))[1]
  geneset <- annotate_query_genesets(fg, query)[1]
  s <- annotate_enrichment_summary(fg, query, geneset)
  expect_true(all(c("NES", "Enrichment score (ES)", "FDR", "Hits in ranking") %in% s$table$Measure))
  expect_false("hypeR score" %in% s$table$Measure)
  expect_identical(s$genes_label, "Leading edge genes")
  expect_true(length(s$genes) > 0)

  ks <- res$kstest
  kq <- names(annotate_result_hyps(ks))[1]
  ksum <- annotate_enrichment_summary(ks, kq, annotate_query_genesets(ks, kq)[1])
  expect_true("hypeR score" %in% ksum$table$Measure)
  expect_false("NES" %in% ksum$table$Measure)

  hyper <- res$hypergeometric
  hq <- names(annotate_result_hyps(hyper))[1]
  hg <- annotate_query_genesets(hyper, hq)[1]
  hs <- annotate_enrichment_summary(hyper, hq, hg)
  row <- annotate_result_hyps(hyper)[[hq]]$data
  row <- row[row$label == hg, ]
  expect_identical(hs$table$Value[hs$table$Measure == "Overlap"], as.character(row$overlap))
  expect_identical(length(hs$genes), as.integer(row$overlap))
})

# ---- R code ---------------------------------------------------------------------------

test_that("the R code parses and writes only non-default arguments", {
  args <- annotate_build_args("repository", "conn", signature_ids = c(11, 12), genesets = structure(list(), class = "gsets"),
                              settings = annotate_settings(test = "fgsea", fdr_scope = "query",
                                                           fgsea_args = list(minSize = 15), background = list(`11` = 20000, `12` = "difexp")))
  code <- annotate_r_code(
    args,
    genesets_description = list(source = "msigdb", species = "Mus musculus", collection = "M2", subcollection = "CP:REACTOME",
                                clean = TRUE, origin = "cache", version = "2026.1.Mm"),
    plots = list(dots = list(fdr = 0.05, top = 20), enrichment = list(geneset = "REACTOME_X", query = "alpha | up"),
                 map = list(type = "emap", query = "alpha | up", similarity_cutoff = 0.2))
  )
  expect_silent(parse(text = code))
  expect_match(code, "conn_handler <- SigRepo::newConnHandler(...)", fixed = TRUE)
  expect_match(code, "msigdb_species = \"Mus musculus\", msigdb_collection = \"M2\", msigdb_subcollection = \"CP:REACTOME\", msigdb_clean = TRUE)", fixed = TRUE)
  expect_match(code, "signature_id = c(11, 12)", fixed = TRUE)
  expect_match(code, "organism = \"Homo sapiens\"", fixed = TRUE)
  expect_match(code, "test = \"fgsea\"", fixed = TRUE)
  expect_match(code, "fdr_scope = \"query\"", fixed = TRUE)
  expect_match(code, "fgsea_args = list(minSize = 15)", fixed = TRUE)
  expect_match(code, "background = list(\"11\" = 20000, \"12\" = \"difexp\")", fixed = TRUE)
  expect_false(grepl("direction =|power =|seed =|verbose =|pval =", code))
  expect_match(code, "SigRepo::plotHypeRDots(res, fdr = 0.05, top = 20)", fixed = TRUE)
  expect_match(code, "SigRepo::plotHypeREnrichment(res, geneset = \"REACTOME_X\", query = \"alpha | up\")", fixed = TRUE)
  expect_match(code, "SigRepo::hypeRToExcel(res", fixed = TRUE)
})

test_that("R code names uploads, custom genesets and long gene lists by reference", {
  gs <- structure(list(), class = "gsets")
  up <- annotate_r_code(annotate_build_args("upload", "conn", omic_signatures = list(a = 1), genesets = gs, settings = annotate_settings()),
                        genesets_description = list(source = "custom"))
  expect_silent(parse(text = up))
  expect_match(up, "omic_signature <- readRDS(", fixed = TRUE)
  expect_match(up, "omic_signature = omic_signature", fixed = TRUE)
  expect_match(up, "genesets <- readRDS(\"annotate_genesets.rds\")", fixed = TRUE)

  short <- annotate_r_code(annotate_build_args("genes", gene_lists = list(g = c("TP53", "MYC")), genesets = gs, settings = annotate_settings()),
                           genesets_description = list(source = "custom"))
  expect_match(short, "signature <- list(g = c(\"TP53\", \"MYC\"))", fixed = TRUE)
  expect_match(short, "genesets <- readRDS(\"annotate_genesets.rds\")", fixed = TRUE)
  long <- annotate_r_code(annotate_build_args("genes", gene_lists = list(g = sprintf("GENE%d", 1:2000)), genesets = gs, settings = annotate_settings()))
  expect_match(long, "signature <- readRDS(\"annotate_gene_lists.rds\")", fixed = TRUE)
  expect_silent(parse(text = long))
})

test_that("empty plot controls fall back to their default, others to at least the minimum", {
  expect_identical(annotate_number_or(NULL, 20, min = 1), 20)
  expect_identical(annotate_number_or(NA, 20, min = 1), 20)
  expect_identical(annotate_number_or(0, 20, min = 1), 1)
  expect_identical(annotate_number_or(35, 20, min = 1), 35)
})

test_that("integer inputs from the browser compare equal to the defaults", {
  gs <- structure(list(), class = "gsets")
  args <- annotate_build_args("repository", "conn", signature_ids = 11, genesets = gs,
                              settings = annotate_settings(test = "fgsea", seed = 1L, power = 1L,
                                                           fgsea_args = list(sampleSize = 101L, minSize = 15L, maxSize = Inf)))
  expect_identical(args$fgsea_args, list(minSize = 15))
  expect_identical(args$seed, 1)
  code <- annotate_r_code(args, list(source = "custom"))
  expect_false(grepl("seed =|power =|sampleSize|15L", code))
  expect_match(code, "fgsea_args = list(minSize = 15)", fixed = TRUE)
})

