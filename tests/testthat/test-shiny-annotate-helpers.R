# Helpers behind the Shiny Annotate tab (legacy_app/utils/annotate_utils.R).
# No database: signatures come from SigRepo's LLFS example, genesets are the
# synthetic fixture in helper-shiny-annotate.R.

load_annotate_app()

# ---- defaults -------------------------------------------------------------------

test_that("the tab's defaults are runHypeR()'s defaults", {
  skip_without_hyper_client()
  formal_defaults <- formals(SigRepo::runHypeR)
  for (name in setdiff(names(ANNOTATE_DEFAULTS), "msigdb_clean")) {
    expected <- eval(formal_defaults[[name]])
    if (name %in% c("test", "direction", "ks_source", "fdr_scope")) {
      expected <- expected[1]
    }
    expect_identical(ANNOTATE_DEFAULTS[[name]], expected, info = name)
  }
  expect_identical(ANNOTATE_DEFAULTS$msigdb_clean, eval(formal_defaults$msigdb_clean))
  expect_identical(ANNOTATE_FGSEA_DEFAULTS, SigRepo:::HYPER_FGSEA_DEFAULT_ARGS)
  expect_identical(ANNOTATE_PROVENANCE_KEYS, SigRepo:::HYPER_PROVENANCE_KEYS)
})

test_that("default labels print the value, with fgsea's own direction", {
  expect_identical(annotate_default_text("fdr_scope"), "default (run)")
  expect_identical(annotate_default_text("split"), "default (on)")
  expect_identical(annotate_default_text("background"), "default (auto)")
  expect_identical(annotate_default_text("maxSize"), "default (Inf)")
  expect_identical(annotate_default_text("direction"), "default (up)")
  expect_identical(annotate_default_text("direction", test = "fgsea"), "default (both)")
  expect_identical(annotate_default_direction("kstest"), "up")
  expect_identical(annotate_default_direction("fgsea"), "both")
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

test_that("hyperdb rgsets choices come from <source>_v<version>.rds files", {
  available <- data.frame(
    source = c("KEGG", "METABOANALYST", "REACTOME", "REFMET"),
    gsets = c("KEGG_v92.0.rds", "METABOANALYST_DRUG_v5.0.rds", "REACTOME_v70.0.rds", "README.md")
  )
  choices <- annotate_rgsets_choices(available)
  expect_identical(choices$source, c("KEGG", "REACTOME"))
  expect_identical(choices$version, c("92.0", "70.0"))
  expect_identical(choices$label, c("KEGG (v92.0)", "REACTOME (v70.0)"))
  expect_identical(annotate_rgsets_choices(NULL)$source, c("KEGG", "REACTOME"))

  expect_error(
    annotate_load_rgsets("SMPDB", "2.75", loader = function(...) hypeR::gsets$new(list(a = "x"), name = "s", version = "1", quiet = TRUE)),
    "not a hierarchy"
  )
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
