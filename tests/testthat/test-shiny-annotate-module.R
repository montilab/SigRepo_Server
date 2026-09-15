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
    direction_type = "bi-directional", assay_type = "transcriptomics", phenotype = "p", has_difexp = 1L,
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

test_that("ranked tests flag signatures without a difexp and a species mismatch", {
  skip_without_hyper_client()
  run_annotate_module(args = list(geneset_loader = fixture_loader), {
    session$setInputs(source = "repository", test = "kstest", ks_source = "difexp", score_col = "score", background_mode = "default")
    session$setInputs(signature_table_rows_selected = c(2, 3), load_genesets = 1)
    notes <- readiness()$notes
    expect_true(any(grepl("'beta' has no difexp table", notes)))
    expect_true(any(grepl("'gamma' is not Homo sapiens", notes)))
    expect_true(readiness()$ready)
  })
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
    session$setInputs(
      source = "repository", test = "fgsea", direction = "both", ks_source = "difexp", score_col = "score",
      power = 1, seed = 1, sample_size = 101, min_size = 15, max_size = NA,
      fdr_scope = "run", pval = 1, fdr = 1, background_mode = "number", background_number = 20000
    )
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
    expect_identical(args$test, "fgsea")
    expect_identical(args$fgsea_args, list(minSize = 15))
    expect_identical(args$background, 20000)
    expect_identical(args$genesets, annotate_fixture_genesets())
    expect_identical(run_state()$result, .(res))
    expect_null(run_state()$error)
    expect_match(output$result_summary$html, "GSEA \\(fgsea\\)")
    expect_false(grepl("settings have changed", output$run_messages$html))

    session$setInputs(fdr_scope = "query")
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
                      fdr_scope = "run", pval = 1, fdr = 1, background_mode = "default")
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
    session$setInputs(source = "genes", test = "fgsea", direction = "both", background_mode = "default",
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

    session$setInputs(map_type = "emap", map_query = query, map_val = "fdr", map_cutoff = 1, map_top = 25,
                      map_metric = "jaccard_similarity", map_similarity = 0.05)
    expect_false(is.null(map_outcome()$result) && length(map_outcome()$warnings) == 0)

    code <- output$r_code
    expect_match(code, "SigRepo::runHypeR(", fixed = TRUE)
    expect_match(code, "test = \"fgsea\"", fixed = TRUE)
    expect_match(code, "SigRepo::plotHypeRDots(res, fdr = 0.25, top = 10, color_by = \"score\")", fixed = TRUE)
    expect_match(code, sprintf("SigRepo::plotHypeREnrichment(res, geneset = \"%s\"", geneset), fixed = TRUE)
    expect_silent(parse(text = code))

    expect_true(nchar(output$results_table) > 0)
    expect_true(nchar(output$provenance_table) > 0)
    expect_match(output$hyper_table$html, "reactable")
  })
})
