# Shared by the Shiny Annotate tab's tests (test-shiny-annotate-*.R): the
# LLFS example signature, synthetic genesets built from it, real runHypeR()
# results of every test, and a testServer() runner for the module. Only
# defines functions; a test file calls load_annotate_app() itself.

# Attach packages in app_src/bootstrap.R's order so the module sees the same
# masking it does in the app (jsonlite::validate() hides shiny::validate()),
# then source the module and the helpers it uses.
load_annotate_app <- function() {
  for (pkg in c("shinyjs", "shiny", "DT", "rmarkdown", "httr", "jsonlite", "dplyr", "ggplot2", "promises", "future")) {
    suppressWarnings(suppressPackageStartupMessages(library(pkg, character.only = TRUE)))
  }
  source(testthat::test_path("../../api/lib/msigdb_cache.R"), local = globalenv())
  for (file in c("utils/utils.R", "utils/compare_utils.R", "utils/annotate_utils.R", "modules/annotate_module.R")) {
    path <- testthat::test_path("../../legacy_app", file)
    if (file.exists(path)) {
      source(path, local = globalenv())
    }
  }
}

# The tab needs the overhauled hypeR client (SigRepo branch HypeR-Review): run
# the tests with SIGREPO_DIR pointing at it, loaded through pkgload. It needs
# the version where organism and test are required and the ranked tests always
# test both ends (no direction argument).
skip_without_hyper_client <- function() {
  testthat::skip_if_not_installed("hypeR")
  runhyper_args <- names(formals(SigRepo::runHypeR))
  testthat::skip_if_not(
    all(c("fdr_scope", "organism") %in% runhyper_args) && !("direction" %in% runhyper_args) &&
      exists("plotHypeRDots", asNamespace("SigRepo")),
    "installed SigRepo predates the hypeR client overhaul (runHypeR organism, no direction, plotHypeRDots)"
  )
}

annotate_llfs <- function() {
  env <- new.env()
  utils::data("LLFS_Aging_Gene_2023", package = "SigRepo", envir = env)
  env$LLFS_Aging_Gene_2023
}

# 24 genesets over the LLFS difexp symbols and 3,000 made-up genes: eight drawn
# mostly from the top of the score ranking, eight from the bottom, eight at
# random. Enough structure for every test to find something, no network needed.
annotate_fixture_genesets <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    sig <- annotate_llfs()
    difexp <- sig$difexp[!is.na(sig$difexp$gene_symbol) & nzchar(sig$difexp$gene_symbol), ]
    ranked <- unique(difexp$gene_symbol[order(difexp$score, decreasing = TRUE)])
    filler <- sprintf("FAKEGENE%d", seq_len(3000))
    old_seed <- if (exists(".Random.seed", envir = globalenv())) get(".Random.seed", envir = globalenv())
    on.exit(if (!is.null(old_seed)) assign(".Random.seed", old_seed, envir = globalenv()), add = TRUE)
    set.seed(20260915)
    make <- function(pool) unique(c(sample(pool, 20), sample(filler, 30)))
    n <- length(ranked)
    sets <- c(
      stats::setNames(lapply(1:8, function(i) make(ranked[1:120])), sprintf("SET_TOP_%d", 1:8)),
      stats::setNames(lapply(1:8, function(i) make(ranked[(n - 119):n])), sprintf("SET_BOTTOM_%d", 1:8)),
      stats::setNames(lapply(1:8, function(i) make(ranked)), sprintf("SET_RANDOM_%d", 1:8))
    )
    cache <<- hypeR::gsets$new(sets, name = "FIXTURE", version = "1", quiet = TRUE)
    cache
  }
})

# Real results for each test, computed once per session.
annotate_fixture_results <- local({
  cache <- NULL
  function() {
    if (!is.null(cache)) {
      return(cache)
    }
    sig <- annotate_llfs()
    gs <- annotate_fixture_genesets()
    quietly <- function(expr) suppressWarnings(suppressMessages(expr))
    cache <<- list(
      hypergeometric = quietly(SigRepo::runHypeR(omic_signature = list(LLFS = sig, LLFS_copy = sig), organism = "Homo sapiens",
                                                 test = "hypergeometric", genesets = gs, verbose = FALSE)),
      kstest = quietly(SigRepo::runHypeR(omic_signature = sig, organism = "Homo sapiens", test = "kstest", genesets = gs, verbose = FALSE)),
      kstest_single = quietly(SigRepo::runHypeR(omic_signature = sig, organism = "Homo sapiens", test = "kstest", genesets = gs,
                                                verbose = FALSE)$data[[1]]),
      fgsea = quietly(SigRepo::runHypeR(omic_signature = sig, organism = "Homo sapiens", test = "fgsea", genesets = gs, verbose = FALSE)),
      native = quietly(SigRepo::runHypeR(
        signature = list(top = gs$genesets$SET_TOP_1, bottom = gs$genesets$SET_BOTTOM_1),
        organism = "Homo sapiens", test = "hypergeometric", genesets = gs
      ))
    )
    cache
  }
})

# What fileInput() hands the server for one uploaded file.
annotate_upload_of <- function(path, file_name) {
  data.frame(name = file_name, size = file.size(path), type = "", datapath = path, stringsAsFactors = FALSE)
}

annotate_db_rows <- data.frame(
  signature_id = c(11, 12, 13),
  signature_name = c("alpha", "beta", "gamma"),
  organism = c("Homo sapiens", "Homo sapiens", "Mus musculus"),
  type = c("bi-directional", "uni-directional", "categorical"),
  assay_type = "transcriptomics",
  phenotype = c("a vs b", "c", "d"),
  has_difexp = c(1L, 0L, 1L),
  user_name = "devadmin",
  stringsAsFactors = FALSE
)

# testServer() evaluates its expression inside the module, where the test's
# own variables are out of sight, so the caller marks them .(name) and they are
# spliced in as values first. It runs on a private later loop so DB pools left
# by other test files cannot hang output reads (issue #81).
run_annotate_module <- function(expr, args = list()) {
  defaults <- list(
    signature_db = reactive(annotate_db_rows),
    user_conn_handler = reactive("user-conn")
  )
  body <- do.call(bquote, list(substitute(expr), where = parent.frame()))
  # The organism select starts at Homo sapiens in the app; testServer() starts
  # every input empty.
  body <- bquote({
    session$setInputs(species = "Homo sapiens")
    .(body)
  })
  later::with_temp_loop(eval(bquote(testServer(
    annotate_module_server,
    args = .(utils::modifyList(defaults, args)),
    .(body)
  ))))
}
