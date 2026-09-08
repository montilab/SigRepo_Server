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
#
# `require_difexp` is how a caller that will never read the difexp (GEM: see
# api/lib/gem_enrichment.R, which only ever uses omic_signature$signature)
# says so explicitly. Without it, every has_difexp = 1 signature refused
# below regardless of whether the caller actually wanted the difexp -- which
# is what made GEM fail on every metabolomics signature on a server running
# the 2-arg client, even though GEM never touches a difexp at all. Callers
# that DO read the difexp (compare.R) leave this at its default TRUE, so they
# keep refusing rather than silently building a signature that is missing
# data they asked for.
build_omic_signature <- function(db_row, difexp = NULL, require_difexp = TRUE) {
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
    if (require_difexp) {
      base::stop(
        "This signature has a difexp table, and the SigRepo client installed on ",
        "this server cannot accept one directly (createOmicSignature() is missing ",
        "the 'difexp' and 'fetch_difexp' arguments). Fetching it would make the ",
        "API call itself and deadlock. Update the SigRepo client, or use a ",
        "signature without a difexp table."
      )
    }

    # require_difexp = FALSE means the caller genuinely does not need it -- GEM
    # reads only $signature. But the released client decides for itself: on
    # `db_signature_tbl$has_difexp[1] == TRUE` it issues an httr::GET to this
    # API's own /get_difexp. Inside a single-process Plumber worker that is a
    # self-call, and it deadlocks -- strictly worse than the error above.
    # Simply skipping the stop() is therefore not enough.
    #
    # So tell the client there is nothing to fetch. This zeroes the flag on OUR
    # COPY of the row for this one constructor call; the database row and every
    # other caller are untouched, and the OmicSignature just carries no difexp,
    # which is exactly what a difexp-free caller asked for.
    db_row$has_difexp <- 0L
  }

  SigRepo:::createOmicSignature(
    conn_handler = conn_handler,
    db_signature_tbl = db_row
  )
}
