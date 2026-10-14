# The Shiny Annotate module (legacy_app/modules/annotate_module.R) under
# testServer(). runHypeR(), prepareHypeRSignatures() and geneset loading are
# replaced by stubs that record their arguments and return real results from
# helper-shiny-annotate.R, so no database or network is needed.

load_annotate_app()

fixture_loader <- function(request) {
  list(
    genesets = annotate_fixture_genesets(),
    description = list(source = "msigdb", species = "Homo sapiens", collection = "H", subcollection = "",
                       clean = FALSE, origin = "cache", name = "FIXTURE", version = "1", n = 24L)
  )
}

recording_runner <- function(result) {
  calls <- list()
  runner <- function(...) {
    calls[[length(calls) + 1]] <<- list(...)
    result
  }
  list(runner = runner, calls = function() calls)
}

test_that("repository picks are kept by id and capped at ten", {
  skip_without_hyper_client()
  many <- data.frame(
    signature_id = 1:12, signature_name = sprintf("sig%02d", 1:12), organism = "Homo sapiens",
    type = "bi-directional", assay_type = "transcriptomics", phenotype = "p", has_difexp = 1L,
    user_name = "devadmin", stringsAsFactors = FALSE
  )
  run_annotate_module(args = list(signature_db = reactive(many)), {
    session$setInputs(source = "repository", test = "hypergeometric")
    session$setInputs(signature_table_rows_selected = c(3, 1))
    expect_identical(picks(), c("3", "1"))
    session$setInputs(signature_table_rows_selected = 1:12)
    expect_length(picks(), 10)
    expect_identical(picks()[1:2], c("3", "1"))
    expect_match(output$signature_summary$html, "the last 2 picked were left out")
    session$setInputs(clear_picks = 1)
    expect_identical(picks(), character())
  })
})

test_that("a run needs signatures and loaded genesets, and says so", {
  skip_without_hyper_client()
  run_annotate_module(args = list(geneset_loader = fixture_loader), {
    session$setInputs(source = "repository", test = "hypergeometric", background_mode = "default")
    r <- readiness()
    expect_false(r$ready)
    expect_false(r$preview_ready)
    expect_true(any(grepl("Pick at least one signature", r$problems)))
    expect_true(any(grepl("Load genesets", r$problems)))

    session$setInputs(signature_table_rows_selected = 1)
    expect_true(readiness()$preview_ready)
    expect_false(readiness()$ready)

    session$setInputs(geneset_source = "msigdb", species = "Homo sapiens", collection = "H", subcollection = "", load_genesets = 1)
    expect_identical(genesets_state()$description$name, "FIXTURE")
    expect_true(readiness()$ready)
    expect_match(output$genesets_status$html, "FIXTURE: 24 genesets")
  })
})

test_that("ranked tests flag signatures without a difexp, and another organism blocks the run", {
  skip_without_hyper_client()
  run_annotate_module(args = list(geneset_loader = fixture_loader), {
    session$setInputs(source = "repository", test = "kstest", background_mode = "default")
    session$setInputs(signature_table_rows_selected = c(2, 3), load_genesets = 1)
    r <- readiness()
    expect_true(any(grepl("'beta' has no difexp table", r$notes)))
    expect_false(r$ready)
    expect_true(r$preview_ready)
    expect_true(any(grepl("'gamma' (Mus musculus) is not Homo sapiens", r$problems, fixed = TRUE)))

    # Picking only human signatures runs.
    session$setInputs(signature_table_rows_selected = c(1, 2))
    expect_true(readiness()$ready)

    # The organism is required.
    session$setInputs(species = "")
    expect_true(any(grepl("Choose the organism", readiness()$problems)))
  })
})

test_that("changing the organism drops loaded MSigDB genesets but keeps custom ones", {
  skip_without_hyper_client()
  run_annotate_module(args = list(geneset_loader = fixture_loader), {
    session$setInputs(geneset_source = "msigdb", load_genesets = 1)
    expect_false(is.null(genesets_state()))
    session$setInputs(species = "Mus musculus")
    expect_null(genesets_state())
  })
  custom_loader <- function(request) {
    list(genesets = annotate_fixture_genesets(), description = list(source = "custom", name = "Custom", n = 24L))
  }
  run_annotate_module(args = list(geneset_loader = custom_loader), {
    session$setInputs(geneset_source = "custom", load_genesets = 1)
    session$setInputs(species = "Mus musculus")
    expect_false(is.null(genesets_state()))
  })
})

test_that("only a collection the picker offers is asked for", {
  # The dropdown can hold a value it no longer offers: the last organism's
  # collection after a change of organism, or anything a browser sends. What
  # is asked of the loader is checked against what is offered, so a withheld
  # collection cannot be loaded by any route.
  skip_without_hyper_client()
  dir <- tempfile("msigdb-cache-")
  dir.create(dir)
  annotate_fixture_cache(dir, "Homo sapiens", c("H", "C2/CGP", "C3/TFT:GTRD"))
  withr::local_envvar(MSIGDB_CACHE_DIR = dir)
  asked <- list()
  recording_loader <- function(request) {
    asked[[length(asked) + 1]] <<- request
    fixture_loader(request)
  }
  ask <- function(species, collection, subcollection) {
    run_annotate_module(args = list(geneset_loader = recording_loader), {
      session$setInputs(species = .(species))
      session$setInputs(geneset_source = "msigdb", collection = .(collection), subcollection = .(subcollection),
                        load_genesets = 1)
    })
    asked[[length(asked)]][c("species", "collection", "subcollection")]
  }

  expect_identical(ask(species = "Homo sapiens", collection = "H", subcollection = ""),
                   list(species = "Homo sapiens", collection = "H", subcollection = ""))
  expect_identical(ask(species = "Homo sapiens", collection = "C3", subcollection = "TFT:GTRD"),
                   list(species = "Homo sapiens", collection = "C3", subcollection = "TFT:GTRD"))
  # cached, but withheld
  expect_identical(ask(species = "Homo sapiens", collection = "C2", subcollection = "CGP")$collection, "")
  # offered for another organism
  expect_identical(ask(species = "Rattus norvegicus", collection = "H", subcollection = "")$collection, "")
  # a subcollection that is not cached
  expect_identical(ask(species = "Homo sapiens", collection = "C3", subcollection = "MIR:MIRDB"),
                   list(species = "Homo sapiens", collection = "", subcollection = ""))
})

test_that("gene lists are checked against the test before a run", {
  skip_without_hyper_client()
  run_annotate_module(args = list(geneset_loader = fixture_loader), {
    session$setInputs(source = "genes", test = "fgsea", background_mode = "default", gene_text = "# plain\nTP53\nMYC")
    session$setInputs(load_genesets = 1)
    expect_false(readiness()$ready)
    expect_true(any(grepl("'plain' has none", readiness()$problems)))

    session$setInputs(gene_text = "# ranked\nTP53 2\nMYC -1\nIL6 0.5")
    expect_true(readiness()$ready)

    session$setInputs(background_mode = "per_signature")
    expect_true(any(grepl("gene lists take one background", readiness()$problems)))

    session$setInputs(background_mode = "default", gene_text = "# bad\nTP53 1\nMYC")
    expect_true(any(grepl("'bad' mixes", readiness()$problems)))
  })
})

test_that("preview and run call the client with the repository arguments", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()$fgsea
  run <- recording_runner(res)
  preview <- recording_runner(list(
    info = data.frame(query = "alpha", signature_id = "11", stringsAsFactors = FALSE),
    skipped = data.frame(signature = character(), reason = character(), message = character())
  ))
  run_annotate_module(args = list(geneset_loader = fixture_loader, runner = run$runner, previewer = preview$runner), {
    session$setInputs(source = "repository", test = "fgsea",
                      background_mode = "number", background_number = 20000)
    session$setInputs(signature_table_rows_selected = 1, load_genesets = 1)

    session$setInputs(preview = 1)
    prev_args <- .(preview$calls)()[[1]]
    expect_identical(prev_args$signature_id, 11)
    expect_identical(prev_args$conn_handler, "user-conn")
    expect_false("genesets" %in% names(prev_args))
    expect_identical(preview_state()$result$info$query, "alpha")

    session$setInputs(run = 1)
    args <- .(run$calls)()[[1]]
    expect_identical(args$signature_id, 11)
    expect_identical(args$organism, "Homo sapiens")
    expect_identical(args$test, "fgsea")
    # Everything the tab no longer exposes is sent at runHypeR()'s own default,
    # so the R code leaves it out.
    expect_length(args$fgsea_args, 0)
    expect_identical(args[c("fdr_scope", "pval", "fdr", "ks_source", "score_col", "power", "seed")],
                     list(fdr_scope = "run", pval = 1, fdr = 1, ks_source = "difexp", score_col = "score", power = 1, seed = 1))
    expect_false(any(c("absolute", "direction") %in% names(args)))
    expect_identical(args$background, 20000)
    expect_identical(args$genesets, annotate_fixture_genesets())
    expect_identical(run_state()$result, .(res))
    expect_null(run_state()$error)
    expect_match(output$result_summary$html, "GSEA \\(fgsea\\)")
    expect_false(grepl("settings have changed", output$run_messages$html))

    session$setInputs(background_number = 15000)
    expect_match(output$run_messages$html, "settings have changed since this run")
  })
})

test_that("uploads and gene lists are sent as their own arguments", {
  skip_without_hyper_client()
  run <- recording_runner(annotate_fixture_results()$native)
  path <- tempfile(fileext = ".rds")
  saveRDS(list(LLFS = annotate_llfs()), path)
  run_annotate_module(args = list(geneset_loader = fixture_loader, runner = run$runner), {
    session$setInputs(source = "upload", test = "hypergeometric", split = FALSE, min_query_genes = 4,
                      background_mode = "default")
    session$setInputs(upload = annotate_upload_of(.(path), "llfs.rds"), load_genesets = 1)
    expect_match(output$signature_summary$html, "LLFS")
    session$setInputs(run = 1)
    args <- .(run$calls)()[[1]]
    expect_identical(names(args$omic_signature), "LLFS")
    expect_false(args$split)
    expect_false("signature_id" %in% names(args))

    session$setInputs(source = "genes", gene_text = "# a\nTP53\nMYC\n# b\nIL6\nTNF")
    session$setInputs(run = 2)
    args <- .(run$calls)()[[2]]
    expect_identical(args$signature, list(a = c("TP53", "MYC"), b = c("IL6", "TNF")))
    expect_false(any(c("conn_handler", "omic_signature", "split") %in% names(args)))
  })
})

test_that("runHypeR() errors and warnings reach the results card", {
  skip_without_hyper_client()
  failing <- function(...) {
    warning("Default background: the difexp looks filtered")
    stop("\nNo query is left after removing empty queries.\n")
  }
  run_annotate_module(args = list(geneset_loader = fixture_loader, runner = failing), {
    session$setInputs(source = "genes", test = "hypergeometric", background_mode = "default", gene_text = "TP53\nMYC")
    session$setInputs(load_genesets = 1, run = 1)
    expect_null(run_state()$result)
    html <- output$run_messages$html
    expect_match(html, "The enrichment could not run")
    expect_match(html, "No query is left")
    expect_match(html, "1 warning from runHypeR")
  })
})

test_that("every result view renders for a real fgsea result", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()$fgsea
  run_annotate_module(args = list(geneset_loader = fixture_loader, runner = function(...) res), {
    session$setInputs(source = "genes", test = "fgsea", background_mode = "default",
                      gene_text = "# r\nTP53 2\nMYC -1", load_genesets = 1, run = 1)
    session$setInputs(dot_val = "fdr", dot_cutoff = 0.25, dot_top = 10, dot_color = "score", dot_size = "geneset",
                      dot_abrv = 50, dot_key = TRUE)
    expect_true(is.list(output$dot_plot))
    expect_identical(dot_settings()$color_by, "score")
    expect_identical(dot_settings()$fdr, 0.25)

    query <- names(annotate_result_hyps(.(res)))[1]
    geneset <- annotate_query_genesets(.(res), query)[1]
    session$setInputs(enrichment_query = query, enrichment_geneset = geneset)
    expect_true(is.list(output$enrichment_plot))
    expect_match(output$enrichment_details$html, "Leading edge genes")

    code <- output$r_code
    expect_match(code, "SigRepo::runHypeR(", fixed = TRUE)
    expect_match(code, "organism = \"Homo sapiens\"", fixed = TRUE)
    expect_match(code, "test = \"fgsea\"", fixed = TRUE)
    expect_match(code, "SigRepo::plotHypeRDots(res, fdr = 0.25, top = 10, color_by = \"score\")", fixed = TRUE)
    expect_match(code, sprintf("SigRepo::plotHypeREnrichment(res, geneset = \"%s\"", geneset), fixed = TRUE)
    expect_false(grepl("plotHypeRMap", code))
    expect_silent(parse(text = code))

    expect_true(nchar(output$results_table) > 0)
    expect_match(output$hyper_table$html, "reactable")
  })
})

test_that("an empty dot plot says which cutoff to raise", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()$native
  run_annotate_module(args = list(geneset_loader = fixture_loader, runner = function(...) res), {
    session$setInputs(source = "genes", test = "hypergeometric", background_mode = "default",
                      gene_text = "TP53\nMYC", load_genesets = 1, run = 1)
    session$setInputs(dot_val = "pval", dot_cutoff = 1e-300, dot_top = 20, dot_color = "significance",
                      dot_size = "geneset", dot_abrv = 50, dot_key = TRUE)
    expect_identical(nrow(dot_data()), 0L)
    expect_match(output$dot_hint$html, "No geneset has p-value ≤ 1e-300")
    session$setInputs(dot_cutoff = 1)
    expect_true(nrow(dot_data()) > 0)
    expect_error(output$dot_hint)
  })
})

test_that("a Results row picks its query and geneset for the Enrichment tab, even before the tab has opened", {
  skip_without_hyper_client()
  res <- annotate_fixture_results()$kstest
  run_annotate_module(args = list(geneset_loader = fixture_loader, runner = function(...) res), {
    session$setInputs(source = "genes", test = "kstest", background_mode = "default",
                      gene_text = "# r\nTP53 2\nMYC -1", load_genesets = 1, run = 1)
    table <- results_table()
    row <- which(table$query == names(annotate_result_hyps(.(res)))[2])[3]
    session$setInputs(results_table_rows_selected = row)
    expect_identical(enrichment_pick(), list(query = table$query[row], geneset = table$label[row]))

    controls <- output$enrichment_controls$html
    expect_match(controls, sprintf("<option value=\"%s\" selected>", table$query[row]), fixed = TRUE)
    expect_match(controls, sprintf("<option value=\"%s\" selected>", table$label[row]), fixed = TRUE)

    # A new run with the same query names starts from a clean pick, and the
    # geneset dropdown is rebuilt with that query's genesets.
    session$setInputs(enrichment_query = table$query[row], enrichment_geneset = table$label[row], run = 2)
    expect_null(enrichment_pick())
    expect_match(output$enrichment_controls$html, table$label[row], fixed = TRUE)
  })
})
