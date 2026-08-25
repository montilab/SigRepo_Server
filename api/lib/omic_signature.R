# One place to build an OmicSignature from a database row.
#
# Why this exists rather than calling SigRepo:::createOmicSignature() directly:
# the released client (SigRepo master, and therefore the montilab/sigrepo image
# production runs) declares
#
#   createOmicSignature(conn_handler, db_signature_tbl)
#
# while the `difexp` / `fetch_difexp` arguments callers here want live only in
# an uncommitted working copy. Passing them to the released client raises
# "unused arguments (difexp = difexp, fetch_difexp = FALSE)". compare.R did
# exactly that, and its tryCatch reported the result as "Fewer than two of the
# selected signatures could be loaded", which is why the real cause stayed
# hidden -- /signatures/compare has been failing on every call in production.
#
# So: detect what the installed client accepts, and pass the extra arguments
# only when they exist. When they don't, difexp cannot be injected, and the
# fallback declines to build a signature that has one rather than letting the
# client issue an HTTP GET back to this same single-process Plumber server --
# that self-call is the deadlock the fetch_difexp = FALSE argument was added to
# avoid in the first place.
#
# Depends on the `conn_handler` global defined in api.R.

# Cached because formals() on a namespace-internal function is cheap but this
# runs per signature per request.
.omic_signature_supports_difexp <- local({
  cached <- NULL
  function() {
    if (base::is.null(cached)) {
      args <- base::tryCatch(
        base::names(base::formals(SigRepo:::createOmicSignature)),
        error = function(e) base::character(0)
      )
      cached <<- base::all(c("difexp", "fetch_difexp") %in% args)
    }
    cached
  }
})

# Returns an OmicSignature, or throws. `difexp` may be NULL.
build_omic_signature <- function(db_row, difexp = NULL) {
  # createOmicSignature() is internal (not exported), so it must be reached
  # with ::: rather than ::.
  if (.omic_signature_supports_difexp()) {
    return(SigRepo:::createOmicSignature(
      conn_handler = conn_handler,
      db_signature_tbl = db_row,
      difexp = difexp,
      fetch_difexp = FALSE
    ))
  }

  if (base::isTRUE(base::as.logical(db_row$has_difexp[1]))) {
    base::stop(
      "This signature has a difexp table, and the SigRepo client installed on ",
      "this server cannot accept one directly (createOmicSignature() is missing ",
      "the 'difexp' and 'fetch_difexp' arguments). Fetching it would make the ",
      "API call itself and deadlock. Update the SigRepo client, or use a ",
      "signature without a difexp table."
    )
  }

  SigRepo:::createOmicSignature(
    conn_handler = conn_handler,
    db_signature_tbl = db_row
  )
}
