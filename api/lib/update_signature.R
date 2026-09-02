# Editing a signature's metadata in place, for its owner or an admin.
#
# WHY NOT SigRepo::updateSignature(): that function authorises through
# SigRepo::checkPermissions(), which identifies the caller as
# DBI::dbGetInfo(conn)$user -- the DATABASE CONNECTION's login, not the person
# holding the api_key. This API connects as one privileged account, so
# delegating would authorise EVERY api_key holder as that account and let
# anyone edit anyone's signature. Same reasoning as the header of
# api/lib/create_signature.R, which reimplements upload for the same reason.
# Authorisation here is resolved against the caller with
# user_has_owner_or_editor_access() (api/lib/collection.R).
#
# WHY A PATCH, NOT A REPLACE: updateSignature() takes a whole OmicSignature and
# rebuilds the signature from it, so correcting a typo in a description would
# mean regenerating the object and re-resolving every feature. This edits the
# metadata columns and leaves signature_feature_set and the difexp untouched.
#
# Depends on api/lib/common.R (db_connect_local), api/lib/collection.R
# (user_has_owner_or_editor_access, collection_hash) and
# api/lib/create_signature.R (lookup_id, sql_value).

# Columns a caller may edit, mapped to how each is written.
#
# A whitelist rather than a filter over the caller's keys: these names are
# interpolated into an UPDATE, so an unrecognised key must never reach the SQL.
# Notably absent and deliberately so:
#   signature_name  -- UNIQUE(signature_name, user_name), and signature_hashkey
#                      is derived from it, so renaming is a different operation
#   user_name       -- ownership is not transferable by editing a field
#   organism/assay_type/direction_type -- changing these would invalidate the
#                      stored feature set, which this function does not touch
UPDATE_SIGNATURE_FIELDS <- base::c(
  "description", "keywords", "covariates", "PMID", "year", "visibility", "phenotype"
)

# Update one signature's metadata.
#
# `fields` is a partial patch: a name that is absent is left alone, so the UI
# can send only what changed. Returns list(ok = TRUE, updated = <names>) or
# list(ok = FALSE, reason, message) with reason one of "not_found",
# "forbidden", "no_fields".
update_signature_metadata <- function(conn, auth, signature_hashkey, fields) {
  signature_tbl <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT signature_id, user_name FROM signatures WHERE signature_hashkey = %s LIMIT 1",
    DBI::dbQuoteLiteral(conn, base::as.character(signature_hashkey)[1])
  ))
  if (base::nrow(signature_tbl) == 0) {
    return(base::list(ok = FALSE, reason = "not_found",
                      message = "No signature with that hashkey."))
  }
  signature_id <- base::as.integer(signature_tbl$signature_id[1])

  # Resolved against the api_key caller, never the DB login -- see the header.
  allowed <- user_has_owner_or_editor_access(
    conn = conn, auth = auth,
    owner_user_name = signature_tbl$user_name[1],
    access_table = "signature_access",
    access_id_col = "signature_id",
    access_id_val = signature_id
  )
  if (!base::isTRUE(allowed)) {
    return(base::list(ok = FALSE, reason = "forbidden",
                      message = "You do not have permission to edit this signature."))
  }

  fields <- fields %||% base::list()
  supplied <- base::intersect(base::names(fields), UPDATE_SIGNATURE_FIELDS)
  if (base::length(supplied) == 0) {
    return(base::list(ok = FALSE, reason = "no_fields",
                      message = base::sprintf(
                        "Nothing to update. Editable fields are: %s.",
                        base::paste(UPDATE_SIGNATURE_FIELDS, collapse = ", "))))
  }

  assignments <- base::character(0)
  for (name in supplied) {
    value <- fields[[name]]

    if (base::identical(name, "phenotype")) {
      # phenotype is an FK, not a string column. Upload creates a missing
      # phenotype rather than refusing (create_signature.R:422); editing does
      # the same, or a correction could not introduce a phenotype the
      # repository does not already hold.
      phenotype <- base::trimws(base::as.character(value)[1])
      if (base::is.na(phenotype) || !base::nzchar(phenotype)) {
        next
      }
      phenotype_id <- lookup_id(conn, "phenotypes", "phenotype_id", "phenotype", phenotype)
      if (base::is.null(phenotype_id)) {
        DBI::dbExecute(conn, base::sprintf(
          "INSERT INTO phenotypes (phenotype) VALUES (%s)", DBI::dbQuoteLiteral(conn, phenotype)))
        phenotype_id <- lookup_id(conn, "phenotypes", "phenotype_id", "phenotype", phenotype)
      }
      assignments <- c(assignments, base::sprintf("phenotype_id = %d", base::as.integer(phenotype_id)))
      next
    }

    if (base::identical(name, "visibility")) {
      # Stored as 0/1 with a CHECK constraint, so coerce rather than pass a
      # caller's "true"/"yes"/1 through untranslated.
      assignments <- c(assignments, base::sprintf(
        "visibility = %d", base::as.integer(normalize_flag(value, default = FALSE))))
      next
    }

    assignments <- c(assignments, base::sprintf("%s = %s", name, sql_value(conn, value)))
  }

  if (base::length(assignments) == 0) {
    return(base::list(ok = FALSE, reason = "no_fields",
                      message = "Nothing to update."))
  }

  DBI::dbExecute(conn, base::sprintf(
    "UPDATE signatures SET %s WHERE signature_id = %d",
    base::paste(assignments, collapse = ", "), signature_id
  ))

  base::list(ok = TRUE, updated = supplied)
}
