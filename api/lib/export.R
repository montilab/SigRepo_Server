# Signature export/download for the Signatures page "basket" (bulk
# download of selected signatures), ported from the Shiny app's basket
# feature in legacy_app/modules/signature_module.R.
#
# Doesn't call SigRepo::getSignature() -- like SigRepo::runHypeR(), it
# authorizes via SigRepo::checkPermissions(), which reads the *DB
# connection's own login* as the acting user, not the api_key holder (see
# api/lib/annotate.R for the full reasoning). Instead this builds an
# equivalent metadata+signature+difexp export from fetch_signature_context()
# (already authorizes against the real caller) and load_difexp_rds().
#
# Depends on api/lib/common.R (db_connect_local), api/lib/signature.R
# (fetch_signature_context), and api/lib/difexp.R (load_difexp_rds).

# No real signature has anywhere near this many features; passing it as
# max_features effectively means "no cap" without changing
# fetch_signature_context()'s contract for its other (capped) callers.
EXPORT_MAX_FEATURES <- 1000000

# Returns list(ok = FALSE, reason) or
# list(ok = TRUE, signature_name, export = list(metadata, signature, difexp)).
# `export` is what gets saveRDS()'d -- readRDS()-compatible with what
# Shiny's own basket download already produces (metadata/signature/difexp).
build_signature_export <- function(auth, signature_hashkey, difexp_dir) {
  context <- fetch_signature_context(
    signature_hashkey = signature_hashkey,
    include_features = TRUE,
    max_features = EXPORT_MAX_FEATURES,
    auth = auth
  )
  if (base::is.null(context)) {
    return(base::list(ok = FALSE, reason = "not_found"))
  }

  difexp_tbl <- NULL
  if (base::isTRUE(base::as.logical(context$signature$has_difexp))) {
    difexp_tbl <- load_difexp_rds(difexp_dir, signature_hashkey)
  }

  # context$features is already compact_table()'d (list-of-row-lists) for
  # JSON responses; reconstitute it as a proper data.frame for an RDS export
  # meant to be reloaded in R.
  feature_tbl <- if (base::length(context$features) > 0) {
    dplyr::bind_rows(context$features)
  } else {
    base::data.frame()
  }

  base::list(
    ok = TRUE,
    signature_name = context$signature$signature_name,
    export = base::list(
      metadata = context$signature,
      signature = feature_tbl,
      difexp = difexp_tbl
    )
  )
}

export_safe_filename <- function(signature_name) {
  base::gsub("[^A-Za-z0-9_-]", "_", signature_name)
}

# Builds a zip of signature_<name>.rds files for a basket of signatures.
# Silently skips signatures the caller can't (or can no longer) see, the
# same permissive behavior Shiny's basket download had. Returns
# list(ok = FALSE, reason) or
# list(ok = TRUE, zip_path, included = <hashkeys>, skipped = <hashkeys>).
build_signature_basket_zip <- function(auth, signature_hashkeys, difexp_dir) {
  signature_hashkeys <- base::unique(signature_hashkeys)
  if (base::length(signature_hashkeys) == 0) {
    return(base::list(ok = FALSE, reason = "empty_basket"))
  }

  export_dir <- base::file.path(base::tempdir(), base::sprintf("signature_basket_%d", base::as.integer(base::Sys.time())))
  base::dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)

  included <- base::character()
  skipped <- base::character()
  exported_files <- base::character()

  for (hk in signature_hashkeys) {
    result <- build_signature_export(auth, hk, difexp_dir)
    if (!result$ok) {
      skipped <- c(skipped, hk)
      next
    }
    out_file <- base::file.path(export_dir, base::sprintf("signature_%s.rds", export_safe_filename(result$signature_name)))
    base::saveRDS(result$export, out_file)
    exported_files <- c(exported_files, out_file)
    included <- c(included, hk)
  }

  if (base::length(exported_files) == 0) {
    return(base::list(ok = FALSE, reason = "none_exported"))
  }

  zip_path <- base::file.path(base::tempdir(), base::sprintf("signature_basket_%d.zip", base::as.integer(base::Sys.time())))
  utils::zip(zipfile = zip_path, files = exported_files, flags = "-j")

  base::list(ok = TRUE, zip_path = zip_path, included = included, skipped = skipped)
}
