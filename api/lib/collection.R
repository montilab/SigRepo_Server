# Collection CRUD backing /collections/* routes. Depends on api/lib/common.R
# (db_connect_local, compact_table).
#
# SigRepo's own addCollection/deleteCollection/addSignatureToCollection/
# addUserToCollection all authorize via SigRepo::checkPermissions(), which
# reads the *DB connection's own login* as the acting user (dbGetInfo(conn)
# $user). That's correct in Shiny, where each session's connection genuinely
# is logged in as that person, but wrong for the REST API, which always
# connects through one shared service-account login (see conn_handler in
# api.R) -- see delete_signature() in api/lib/signature.R for the same
# reasoning applied to signatures. Collection reads/writes here are
# reimplemented directly against the tables, authorizing against the real
# api_key holder (auth$user_name/auth$user_role) instead.

collection_hash <- function(...) {
  digest::digest(base::tolower(base::paste0(...)), algo = "md5", serialize = FALSE)
}

# TRUE if auth's user has owner/editor rights on a resource: they're its
# original uploader, they hold an owner/editor grant in the resource's own
# *_access table, or they're an admin. Mirrors the ownership checks
# SigRepo::deleteSignature/deleteCollection/addSignatureToCollection perform
# internally (owner column OR *_access table), just resolved against the
# api_key caller instead of the DB connection's login.
user_has_owner_or_editor_access <- function(conn, auth, owner_user_name, access_table, access_id_col, access_id_val) {
  if (identical(auth$user_role, "admin")) return(TRUE)
  if (identical(owner_user_name, auth$user_name)) return(TRUE)

  filter_coln_val <- stats::setNames(
    base::list(access_id_val, auth$user_name, c("owner", "editor")),
    c(access_id_col, "user_name", "access_type")
  )
  access_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = access_table,
    return_var = "user_name",
    filter_coln_var = c(access_id_col, "user_name", "access_type"),
    filter_coln_val = filter_coln_val,
    filter_var_by = c("AND", "AND"),
    check_db_table = TRUE
  )
  base::nrow(access_tbl) > 0
}

# TRUE if auth's user may *see* a collection: it's public, they own it, they
# hold any access grant (owner/editor/viewer), or they're an admin. Broader
# than user_has_owner_or_editor_access() -- read access includes viewers.
collection_visible_to <- function(conn, auth, collection_row) {
  if (identical(auth$user_role, "admin")) return(TRUE)
  if (base::isTRUE(base::as.logical(collection_row$visibility[1]))) return(TRUE)
  if (identical(collection_row$user_name[1], auth$user_name)) return(TRUE)

  access_tbl <- SigRepo::lookup_table_sql(
    conn = conn,
    db_table_name = "collection_access",
    return_var = "user_name",
    filter_coln_var = c("collection_id", "user_name"),
    filter_coln_val = base::list("collection_id" = collection_row$collection_id[1], "user_name" = auth$user_name),
    filter_var_by = "AND",
    check_db_table = TRUE
  )
  base::nrow(access_tbl) > 0
}

# List collections visible to the caller (public, owned, or granted access),
# with a computed member count. Admins see every collection.
search_collections <- function(conn, auth, keyword = NULL, limit = 50) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) limit <- 50
  limit <- base::min(limit, 200)

  query <- "
    SELECT c.collection_id, c.collection_name, c.description, c.user_name,
           c.visibility, c.date_created, c.collection_hashkey,
           (SELECT COUNT(*) FROM signature_collection_access sca WHERE sca.collection_id = c.collection_id) AS num_signatures
    FROM collection c
    WHERE 1=1
  "

  if (!identical(auth$user_role, "admin")) {
    user_literal <- DBI::dbQuoteLiteral(conn, auth$user_name)
    query <- base::paste(query, base::sprintf(
      "AND (c.visibility = 1 OR c.user_name = %s OR c.collection_id IN (SELECT collection_id FROM collection_access WHERE user_name = %s))",
      user_literal, user_literal
    ))
  }

  if (!is.null(keyword) && base::nzchar(base::trimws(keyword[1]))) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", base::trimws(keyword[1])))
    query <- base::paste(query, base::sprintf("AND (c.collection_name LIKE %s OR c.description LIKE %s)", like, like))
  }

  query <- base::paste(query, "ORDER BY c.collection_name ASC LIMIT", limit)
  DBI::dbGetQuery(conn, query)
}

# Member signatures of a collection, in roughly the same shape as
# search_signatures(). Callers must already have verified visibility via
# collection_visible_to() -- this does no access checking itself.
list_collection_signatures <- function(conn, collection_id) {
  query <- base::sprintf("
    SELECT s.signature_hashkey, s.signature_name, o.organism, p.phenotype, s.assay_type, s.visibility
    FROM signature_collection_access sca
    INNER JOIN signatures s ON s.signature_id = sca.signature_id
    LEFT JOIN organisms o ON s.organism_id = o.organism_id
    LEFT JOIN phenotypes p ON s.phenotype_id = p.phenotype_id
    WHERE sca.collection_id = %d
    ORDER BY s.signature_name ASC
  ", collection_id)
  DBI::dbGetQuery(conn, query)
}

# Returns list(ok, reason) or list(ok = TRUE, collection = <list>, signatures = <compact_table>)
get_collection_detail <- function(auth, collection_hashkey) {
  conn <- NULL
  base::tryCatch({
    conn <- db_connect_local()

    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, db_table_name = "collection", return_var = "*",
      filter_coln_var = "collection_hashkey",
      filter_coln_val = base::list("collection_hashkey" = collection_hashkey),
      check_db_table = TRUE
    )
    if (base::nrow(collection_tbl) == 0) {
      return(base::list(ok = FALSE, reason = "not_found"))
    }
    if (!collection_visible_to(conn, auth, collection_tbl)) {
      return(base::list(ok = FALSE, reason = "forbidden"))
    }

    signatures_tbl <- list_collection_signatures(conn, collection_tbl$collection_id[1])
    base::list(
      ok = TRUE,
      collection = base::as.list(collection_tbl[1, , drop = FALSE]),
      signatures = compact_table(signatures_tbl, max_rows = 500)
    )
  }, finally = {
    if (!is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })
}

# Returns list(ok = FALSE, reason, message) or list(ok = TRUE, collection_hashkey)
create_collection <- function(auth, collection_name, description = "", visibility = FALSE) {
  if (!auth$user_role %in% c("editor", "admin")) {
    return(base::list(ok = FALSE, reason = "forbidden"))
  }

  collection_name <- base::trimws(base::as.character(collection_name)[1])
  if (base::is.na(collection_name) || !base::nzchar(collection_name)) {
    return(base::list(ok = FALSE, reason = "invalid", message = "collection_name cannot be empty."))
  }
  description <- base::trimws(base::as.character(description)[1])
  if (base::is.na(description)) description <- ""

  conn <- NULL
  base::tryCatch({
    conn <- db_connect_local()
    hashkey <- collection_hash(collection_name, auth$user_name)

    existing <- SigRepo::lookup_table_sql(
      conn = conn, db_table_name = "collection", return_var = "collection_hashkey",
      filter_coln_var = "collection_hashkey", filter_coln_val = base::list("collection_hashkey" = hashkey),
      check_db_table = TRUE
    )
    if (base::nrow(existing) > 0) {
      return(base::list(ok = FALSE, reason = "duplicate", message = "You already have a collection with this name."))
    }

    DBI::dbExecute(conn, base::sprintf(
      "INSERT INTO collection (collection_name, description, user_name, visibility, collection_hashkey) VALUES (%s, %s, %s, %d, %s)",
      DBI::dbQuoteLiteral(conn, collection_name),
      if (base::nzchar(description)) DBI::dbQuoteLiteral(conn, description) else "NULL",
      DBI::dbQuoteLiteral(conn, auth$user_name),
      if (base::isTRUE(visibility)) 1L else 0L,
      DBI::dbQuoteLiteral(conn, hashkey)
    ))

    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, db_table_name = "collection", return_var = "collection_id",
      filter_coln_var = "collection_hashkey", filter_coln_val = base::list("collection_hashkey" = hashkey),
      check_db_table = TRUE
    )
    cid <- collection_tbl$collection_id[1]

    access_hashkey <- collection_hash(cid, auth$user_name)
    DBI::dbExecute(conn, base::sprintf(
      "INSERT INTO collection_access (collection_id, user_name, access_type, access_collection_hashkey) VALUES (%d, %s, 'owner', %s)",
      cid, DBI::dbQuoteLiteral(conn, auth$user_name), DBI::dbQuoteLiteral(conn, access_hashkey)
    ))

    base::list(ok = TRUE, collection_hashkey = hashkey)
  }, finally = {
    if (!is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })
}

# Returns list(ok = FALSE, reason) or list(ok = TRUE, collection_name)
delete_collection_by_hashkey <- function(auth, collection_hashkey) {
  if (!auth$user_role %in% c("editor", "admin")) {
    return(base::list(ok = FALSE, reason = "forbidden"))
  }

  conn <- NULL
  base::tryCatch({
    conn <- db_connect_local()

    collection_tbl <- SigRepo::lookup_table_sql(
      conn = conn, db_table_name = "collection", return_var = c("collection_id", "collection_name", "user_name"),
      filter_coln_var = "collection_hashkey", filter_coln_val = base::list("collection_hashkey" = collection_hashkey),
      check_db_table = TRUE
    )
    if (base::nrow(collection_tbl) == 0) {
      return(base::list(ok = FALSE, reason = "not_found"))
    }

    cid <- collection_tbl$collection_id[1]
    if (!user_has_owner_or_editor_access(conn, auth, collection_tbl$user_name[1], "collection_access", "collection_id", cid)) {
      return(base::list(ok = FALSE, reason = "forbidden"))
    }

    # Children before parent so this works without disabling FK checks.
    DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_collection_access WHERE collection_id = %d", cid))
    DBI::dbExecute(conn, base::sprintf("DELETE FROM collection_access WHERE collection_id = %d", cid))
    DBI::dbExecute(conn, base::sprintf("DELETE FROM collection WHERE collection_id = %d", cid))

    base::list(ok = TRUE, collection_name = collection_tbl$collection_name[1])
  }, finally = {
    if (!is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })
}

# Shared setup for add/remove-signature-to-collection: resolves both rows
# and checks the caller has owner/editor access to *both* the signature and
# the collection (mirrors SigRepo::addSignatureToCollection's dual check).
# Returns list(ok = FALSE, reason) or list(ok = TRUE, signature_id, collection_id).
resolve_collection_signature_access <- function(conn, auth, collection_hashkey, signature_hashkey) {
  if (!auth$user_role %in% c("editor", "admin")) {
    return(base::list(ok = FALSE, reason = "forbidden"))
  }

  signature_tbl <- SigRepo::lookup_table_sql(
    conn = conn, db_table_name = "signatures", return_var = c("signature_id", "user_name"),
    filter_coln_var = "signature_hashkey", filter_coln_val = base::list("signature_hashkey" = signature_hashkey),
    check_db_table = TRUE
  )
  if (base::nrow(signature_tbl) == 0) {
    return(base::list(ok = FALSE, reason = "signature_not_found"))
  }

  collection_tbl <- SigRepo::lookup_table_sql(
    conn = conn, db_table_name = "collection", return_var = c("collection_id", "user_name"),
    filter_coln_var = "collection_hashkey", filter_coln_val = base::list("collection_hashkey" = collection_hashkey),
    check_db_table = TRUE
  )
  if (base::nrow(collection_tbl) == 0) {
    return(base::list(ok = FALSE, reason = "collection_not_found"))
  }

  sid <- signature_tbl$signature_id[1]
  cid <- collection_tbl$collection_id[1]

  if (!user_has_owner_or_editor_access(conn, auth, signature_tbl$user_name[1], "signature_access", "signature_id", sid)) {
    return(base::list(ok = FALSE, reason = "forbidden"))
  }
  if (!user_has_owner_or_editor_access(conn, auth, collection_tbl$user_name[1], "collection_access", "collection_id", cid)) {
    return(base::list(ok = FALSE, reason = "forbidden"))
  }

  base::list(ok = TRUE, signature_id = sid, collection_id = cid)
}

# Returns list(ok = FALSE, reason) or list(ok = TRUE, already_member = TRUE/FALSE)
add_signature_to_collection <- function(auth, collection_hashkey, signature_hashkey) {
  conn <- NULL
  base::tryCatch({
    conn <- db_connect_local()
    resolved <- resolve_collection_signature_access(conn, auth, collection_hashkey, signature_hashkey)
    if (!resolved$ok) return(resolved)

    already <- SigRepo::lookup_table_sql(
      conn = conn, db_table_name = "signature_collection_access", return_var = "collection_id",
      filter_coln_var = c("collection_id", "signature_id"),
      filter_coln_val = base::list("collection_id" = resolved$collection_id, "signature_id" = resolved$signature_id),
      filter_var_by = "AND", check_db_table = TRUE
    )
    if (base::nrow(already) > 0) {
      return(base::list(ok = TRUE, already_member = TRUE))
    }

    hashkey <- collection_hash(resolved$collection_id, resolved$signature_id)
    DBI::dbExecute(conn, base::sprintf(
      "INSERT INTO signature_collection_access (collection_id, signature_id, signature_collection_hashkey) VALUES (%d, %d, %s)",
      resolved$collection_id, resolved$signature_id, DBI::dbQuoteLiteral(conn, hashkey)
    ))
    base::list(ok = TRUE, already_member = FALSE)
  }, finally = {
    if (!is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })
}

# Returns list(ok = FALSE, reason) or list(ok = TRUE)
remove_signature_from_collection <- function(auth, collection_hashkey, signature_hashkey) {
  conn <- NULL
  base::tryCatch({
    conn <- db_connect_local()
    resolved <- resolve_collection_signature_access(conn, auth, collection_hashkey, signature_hashkey)
    if (!resolved$ok) return(resolved)

    DBI::dbExecute(conn, base::sprintf(
      "DELETE FROM signature_collection_access WHERE collection_id = %d AND signature_id = %d",
      resolved$collection_id, resolved$signature_id
    ))
    base::list(ok = TRUE)
  }, finally = {
    if (!is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })
}

# Shared error-shaping for the add/remove-signature-to-collection routes.
collection_signature_error_response <- function(res, result, collection_hashkey, signature_hashkey) {
  if (!result$ok && identical(result$reason, "signature_not_found")) {
    return(json_error(res, 404, base::sprintf("No signature found for signature_hashkey = '%s'.", signature_hashkey)))
  }
  if (!result$ok && identical(result$reason, "collection_not_found")) {
    return(json_error(res, 404, base::sprintf("No collection found for collection_hashkey = '%s'.", collection_hashkey)))
  }
  if (!result$ok && identical(result$reason, "forbidden")) {
    return(json_error(res, 403, "You do not have permission to modify this collection."))
  }
  json_response(res, 200, payload = base::list(MESSAGES = "OK", already_member = base::isTRUE(result$already_member)))
}
