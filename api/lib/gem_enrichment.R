# hypeR-GEM enrichment: metabolite signatures mapped to enzyme-coding genes.
#
# The Shiny app offered this as "GEM Hypergeometric" and "GEM Weighted"
# (shiny/modules/annotate_module.R). It is a different pipeline from the hypeR
# path in annotate.R, not a variant of it: hypeR.GEM first maps each metabolite
# to the genes whose enzymes act on it, using a genome-scale metabolic model,
# and only then runs gene-level enrichment. So this cannot reuse
# resolve_single_enrichment_query(), which already produces gene symbols.
#
# Applies to metabolomics signatures only. On the current repository that is 3
# of 293, which is a reason to fail clearly rather than mysteriously.
#
# Depends on api/lib/signature.R (fetch_signature_context), api/lib/difexp.R
# (load_difexp_rds), api/lib/msigdb_cache.R (resolve_msigdb_genesets) and the
# conn_handler built in api.R.

# Whether this deployment can run GEM at all. hypeR.GEM is an optional
# dependency -- the SigRepo client ships runHyperGEM() regardless, so the
# wrapper existing proves nothing about the package being installed.
gem_unavailable_reason <- function() {
  if (!base::requireNamespace("hypeR.GEM", quietly = TRUE)) {
    return("The hypeR.GEM package is not installed on this server.")
  }

  # Attaching, not just loading, is required. hypeR.GEM::signature2gene()
  # reaches its metabolic models with data("Human_GEM_tables_di", envir = ...)
  # and passes no `package=` argument, so utils::data() looks only at packages
  # on the search path. Loading the namespace (which is all requireNamespace()
  # and SigRepo's getExportedValue() do) is not enough: the call fails with
  # "object 'Human_GEM_tables_di' not found" even though the model ships with
  # the package. Attach lazily here so a deployment that never runs GEM does
  # not pay for it at boot.
  if (!"package:hypeR.GEM" %in% base::search()) {
    attached <- base::tryCatch({
      base::suppressPackageStartupMessages(
        base::library("hypeR.GEM", character.only = TRUE)
      )
      TRUE
    }, error = function(e) base::conditionMessage(e))
    if (!base::isTRUE(attached)) {
      return(base::sprintf("The hypeR.GEM package could not be attached: %s", attached))
    }
  }
  NULL
}

is_gem_test <- function(test) {
  base::identical(test, "gem_weighted") || base::identical(test, "gem_hypergeo")
}

# The Shiny method names, mapped to what runHyperGEM() actually takes.
.gem_method <- function(test) {
  if (base::identical(test, "gem_weighted")) "weighted" else "unweighted"
}

# hypeR.GEM::signature2gene() matches species against
# c("Human", "Mouse", "Rat", "Zebrafish", "Worm", "Other") with match.arg(),
# which is case-sensitive -- so runHyperGEM()'s own default of "human" would
# error before doing any work. The UI and the rest of the annotate flow speak
# MSigDB binomials ("Homo sapiens"), so accept either and normalise here.
.gem_species <- function(species) {
  key <- base::tolower(base::trimws(base::as.character(species %||% "")))
  base::switch(
    key,
    "homo sapiens" = ,
    "human" = "Human",
    "mus musculus" = ,
    "mouse" = "Mouse",
    "rattus norvegicus" = ,
    "rat" = "Rat",
    "danio rerio" = ,
    "zebrafish" = "Zebrafish",
    "caenorhabditis elegans" = ,
    "worm" = "Worm",
    "Human"
  )
}

# hypeR.GEM's packaged metabolic models key their genes by Ensembl ID
# (Human-GEM gene_df is ENSG..., and the metabolite->gene lists match), while
# MSigDB genesets are gene symbols. signature2gene(ensemble_id = TRUE) is what
# converts one to the other, via gprofiler2::gconvert() -- but runHyperGEM()
# defaults it to FALSE, which leaves Ensembl IDs that cannot match a single
# symbol, so every geneset scores zero hits and the run "succeeds" with an
# empty table. Detect it from the shipped model rather than hard-coding, so a
# future model that already ships symbols keeps working.
#
# NOTE: the conversion is a live call to the g:Profiler web service. A server
# without outbound network access will fail here, not silently degrade.
.gem_uses_ensembl_ids <- local({
  cached <- base::list()
  function(gem_species) {
    key <- base::as.character(gem_species)
    if (!base::is.null(cached[[key]])) return(cached[[key]])
    ids <- base::tryCatch({
      env <- base::new.env()
      utils::data(list = base::paste0(key, "_GEM_tables"), package = "hypeR.GEM", envir = env)
      base::as.character(base::get(base::paste0(key, "_GEM_tables"), envir = env)$gene_df$name)
    }, error = function(e) base::character(0))
    ids <- ids[!base::is.na(ids) & base::nzchar(ids)]
    # TRUE when the model is Ensembl-keyed; TRUE on an empty/unknown read too,
    # since that matches every model hypeR.GEM currently ships.
    result <- base::length(ids) == 0 ||
      base::mean(base::grepl("^ENS[A-Z]*G[0-9]{6,}", ids)) > 0.5
    cached[[key]] <<- result
    result
  }
})

# hypeR.GEM keys metabolites on a column that must exist in BOTH the signature
# table and the metabolic model's metabolite table. The model
# (hypeR.GEM:::*_GEM_tables$meta_df) offers name / fullname / refmet_name, so
# in practice the key is refmet_name -- but createOmicSignature() collapses
# whatever metabolite nomenclature a signature uses into a single
# `feature_name` column and drops metabolomics_nomenclature from metadata, so
# the built OmicSignature has no refmet_name and runHyperGEM() fails with
# "Signature '<name>' is missing metabolomics reference column 'refmet_name'".
#
# Resolve it from the repository instead of renaming feature_name: a signature
# stored under HMDB or InChIKey nomenclature has non-RefMet feature_names, and
# renaming those would hand GEM identifiers that silently match nothing.
# Joining metabolite_reference by feature_id gets the RefMet name whatever the
# signature was deposited under.
gem_attach_refmet_names <- function(omic_signature, signature_id, reference_key) {
  sig_tbl <- omic_signature$signature
  if (!base::is.data.frame(sig_tbl) || reference_key %in% base::colnames(sig_tbl)) {
    return(omic_signature)
  }
  if (!"probe_id" %in% base::colnames(sig_tbl)) {
    return(omic_signature)
  }

  conn <- SigRepo::conn_init(conn_handler)
  on.exit(base::suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
  mapping <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT sfs.probe_id, mr.%s AS reference_value
       FROM signature_feature_set sfs
       JOIN metabolite_reference mr ON mr.metabolite_id = sfs.feature_id
      WHERE sfs.signature_id = %d AND FIND_IN_SET('metabolomics', sfs.assay_type) > 0",
    DBI::dbQuoteIdentifier(conn, reference_key), base::as.integer(signature_id)
  ))
  if (!base::is.data.frame(mapping) || base::nrow(mapping) == 0) {
    return(omic_signature)
  }

  lookup <- stats::setNames(base::as.character(mapping$reference_value),
                           base::as.character(mapping$probe_id))
  sig_tbl[[reference_key]] <- base::unname(lookup[base::as.character(sig_tbl$probe_id)])
  omic_signature$signature <- sig_tbl
  omic_signature
}

# Returns list(ok = TRUE, signature_name, reference_key, method, results,
# n_metabolites, n_genes) or list(ok = FALSE, reason, message).
run_gem_enrichment <- function(auth, signature_hashkey, test, difexp_dir, msigdb_cache_dir,
                               species = "Homo sapiens", collection = "H", subcollection = NULL,
                               directional = TRUE, reference_key = NULL, fdr = 0.05,
                               background = 23467) {
  blocked <- gem_unavailable_reason()
  if (!base::is.null(blocked)) {
    return(base::list(ok = FALSE, reason = "unavailable", message = blocked))
  }

  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = FALSE,
    auth = auth
  )
  if (base::is.null(context) || base::is.null(context$signature)) {
    return(base::list(ok = FALSE, reason = "not_found",
                      message = "Signature not found, or you do not have access to it."))
  }

  db_row <- base::as.data.frame(context$signature, stringsAsFactors = FALSE)
  signature_name <- base::as.character(db_row$signature_name[1] %||% signature_hashkey)
  assay_type <- base::trimws(base::tolower(base::as.character(db_row$assay_type[1] %||% "")))

  # GEM maps METABOLITES to genes. A transcriptomics signature already is
  # genes, so there is nothing for it to map -- the result would be misleading
  # rather than empty, which is why this refuses instead of degrading.
  if (!base::identical(assay_type, "metabolomics")) {
    return(base::list(
      ok = FALSE, reason = "unsupported_assay_type", signature_name = signature_name,
      message = base::sprintf(
        "GEM enrichment maps metabolites to enzyme-coding genes, so it only applies to metabolomics signatures (this one is '%s').",
        assay_type
      )
    ))
  }

  # hypeR.GEM::enrichment() requires a plain named list and rejects anything
  # else outright ("genesets must be a list object!"), so the hypeR gsets R6
  # object the client would build from msigdb_collection= is not usable here.
  # resolve_msigdb_genesets() already returns the named list, so pass it in
  # directly and leave runHyperGEM()'s MSigDB branch untouched.
  geneset_result <- resolve_msigdb_genesets(msigdb_cache_dir, species, collection, subcollection %||% "")
  if (!base::isTRUE(geneset_result$ok)) {
    return(base::list(ok = FALSE, reason = geneset_result$reason, signature_name = signature_name,
                      message = geneset_result$message))
  }

  difexp <- NULL
  if (base::isTRUE(base::as.logical(db_row$has_difexp[1]))) {
    difexp <- load_difexp_rds(difexp_dir, signature_hashkey)
  }

  gem_species <- .gem_species(species)
  gem_reference_key <- reference_key %||% "refmet_name"

  omic_signature <- base::tryCatch(
    build_omic_signature(db_row = db_row, difexp = difexp),
    error = function(e) e
  )
  if (base::inherits(omic_signature, "error")) {
    return(base::list(ok = FALSE, reason = "signature_build_failed", signature_name = signature_name,
                      message = base::sprintf("Could not build an OmicSignature: %s", base::conditionMessage(omic_signature))))
  }

  attached <- base::tryCatch(
    gem_attach_refmet_names(omic_signature, db_row$signature_id[1], gem_reference_key),
    error = function(e) e
  )
  if (base::inherits(attached, "error")) {
    return(base::list(ok = FALSE, reason = "reference_lookup_failed", signature_name = signature_name,
                      message = base::sprintf("Could not resolve metabolite '%s' values: %s",
                                              gem_reference_key, base::conditionMessage(attached))))
  }
  omic_signature <- attached

  n_mapped <- base::sum(!base::is.na(omic_signature$signature[[gem_reference_key]]) &
                          base::nzchar(base::as.character(omic_signature$signature[[gem_reference_key]] %||% "")))
  if (base::isTRUE(n_mapped == 0)) {
    return(base::list(
      ok = FALSE, reason = "no_reference_values", signature_name = signature_name,
      message = base::sprintf(
        "None of this signature's metabolites have a '%s' value in metabolite_reference, so there is nothing for the metabolic model to map.",
        gem_reference_key
      )
    ))
  }

  run <- base::tryCatch(
    SigRepo::runHyperGEM(
      omic_signature = omic_signature,
      genesets = geneset_result$genesets,
      reference_key = gem_reference_key,
      species = gem_species,
      directional = base::isTRUE(directional),
      method = .gem_method(test),
      ensemble_id = .gem_uses_ensembl_ids(gem_species),
      # runHyperGEM() defaults background to NULL, but hypeR.GEM uses it in
      # plain arithmetic (`background - n_genesets`) with no NULL guard, so a
      # NULL silently collapses every geneset to numeric(0). Pass hypeR's own
      # default so the two annotate paths share one universe size.
      background = background,
      verbose = FALSE
    ),
    error = function(e) e
  )
  if (base::inherits(run, "error")) {
    return(base::list(ok = FALSE, reason = "gem_failed", signature_name = signature_name,
                      message = base::sprintf("GEM enrichment failed: %s", base::conditionMessage(run))))
  }

  base::list(
    ok = TRUE,
    signature_name = signature_name,
    reference_key = base::as.character(run$reference_key %||% NA_character_),
    method = .gem_method(test),
    geneset_source = geneset_result$source,
    n_metabolites = gem_count_metabolites(run$signatures),
    n_genes = gem_count_genes(run$gem_object),
    results = gem_result_table(run$result, fdr)
  )
}

# How many metabolites went in, summed over the group/direction splits
# prepareHyperGEMSignatures() produces. Reported so a thin result is
# distinguishable from a broken mapping.
gem_count_metabolites <- function(signatures) {
  if (!base::is.list(signatures) || base::length(signatures) == 0) return(0L)
  base::length(base::unique(base::unlist(base::lapply(signatures, function(s) {
    if (base::is.data.frame(s)) s[[1]] else base::as.character(s)
  }), use.names = FALSE)))
}

# How many genes the metabolic model mapped them onto.
gem_count_genes <- function(gem_object) {
  tbls <- gem_object$gene_tables
  if (!base::is.list(tbls) || base::length(tbls) == 0) return(0L)
  genes <- base::unlist(base::lapply(tbls, function(t) {
    if (base::is.data.frame(t)) {
      col <- base::intersect(c("gene", "genes", "gene_symbol", "symbol"), base::colnames(t))
      if (base::length(col) > 0) base::as.character(t[[col[1]]]) else base::rownames(t)
    } else {
      base::as.character(t)
    }
  }), use.names = FALSE)
  base::length(base::unique(genes[!base::is.na(genes) & base::nzchar(genes)]))
}

# hypeR.GEM's result carries the same $data shape hypeR uses, but the exact
# wrapper differs by method and version, so unwrap defensively rather than
# assuming one.
gem_result_table <- function(gem_result, fdr = 0.05) {
  df <- NULL
  if (base::is.data.frame(gem_result)) {
    df <- gem_result
  } else if (base::is.list(gem_result)) {
    if (base::is.data.frame(gem_result$data)) {
      df <- gem_result$data
    } else {
      inner <- if (base::is.list(gem_result$data)) gem_result$data else gem_result
      parts <- base::lapply(base::names(inner), function(nm) {
        one <- inner[[nm]]
        one <- if (base::is.data.frame(one)) one else one$data
        if (!base::is.data.frame(one) || base::nrow(one) == 0) return(NULL)
        one$signature_label <- nm
        one
      })
      parts <- base::Filter(base::Negate(base::is.null), parts)
      if (base::length(parts) > 0) {
        cols <- base::Reduce(base::intersect, base::lapply(parts, base::colnames))
        df <- base::do.call(base::rbind, base::lapply(parts, function(p) p[, cols, drop = FALSE]))
      }
    }
  }
  if (!base::is.data.frame(df) || base::nrow(df) == 0) {
    return(base::list())
  }
  if ("fdr" %in% base::colnames(df)) {
    df <- df[!base::is.na(df$fdr) & df$fdr <= fdr, , drop = FALSE]
    df <- df[base::order(df$fdr), , drop = FALSE]
  } else if ("pval" %in% base::colnames(df)) {
    df <- df[base::order(df$pval), , drop = FALSE]
  }
  compact_table(df, max_rows = 500)
}
