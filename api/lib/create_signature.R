# Signature upload backing POST /signatures/upload. Accepts two shapes:
#
#  - A real `OmicSignature` R6 object (montilab/OmicSignature, currently
#    installed as v1.3.0 -- NOT the unmerged S4-migration branch some
#    exploration briefly assumed was live) -- e.g.
#    `saveRDS(OmicSignature$new(metadata, signature, difexp), f)`. This is
#    the shape Shiny's own "Upload Signature" feature works with
#    (SigRepo::addSignature(omic_signature = readRDS(f))), and what a real
#    user actually has on hand. Fields come from `$metadata` (an R6 active
#    binding returning a flat list; required keys checked by
#    OmicSignature's private$checkMetadata(): signature_name, phenotype,
#    organism, direction_type, assay_type) and `$signature` (a data.frame
#    keyed by `feature_name`, not a DB id -- resolved below via the same
#    feature_hashkey lookup addTranscriptomicsSignatureSet()/
#    addProteomicsSignatureSet() use).
#  - This API's own /signatures/export output (list(metadata, signature,
#    difexp)), so Export -> Upload keeps round-tripping. Its feature rows
#    already carry a resolved `feature_id` (straight from
#    signature_feature_set), so no name lookup is needed for that shape.
#
# Deliberately NOT the same feature as Shiny's "Create" flow (CSV upload +
# column-mapping + metadata form that *builds* an OmicSignature from
# scratch) -- that's materially more work and out of scope here.
#
# Doesn't call SigRepo::addSignature() (or addTranscriptomicsSignatureSet/
# addProteomicsSignatureSet/addUserToSignature/addPhenotype/addKeyword,
# which it calls internally) -- every one of those is checkPermissions()-
# gated against the *DB connection's own login*, not the api_key holder,
# same reasoning as every other write in api/lib/*.R this session. This
# reimplements the same steps (in the same order, for the same
# delete-on-failure rollback SigRepo's own addSignature() uses, since
# there's no wrapping DB transaction) authorized against the real caller.
#
# Depends on api/lib/common.R (db_connect_local), api/lib/annotate.R
# (enrichment_reference_table), api/lib/collection.R (collection_hash --
# a generic tolower+md5 helper, reused here for feature/access hash keys --
# note it computes the SAME formula SigRepo::createHashKey() uses:
# md5(tolower(paste0(...))), so it doubles as the feature_hashkey function),
# and api/lib/difexp.R (save_difexp_rds).

REQUIRED_UPLOAD_METADATA_FIELDS <- c("signature_name", "direction_type", "assay_type", "organism", "phenotype")

meta_str <- function(metadata, field) {
  value <- metadata[[field]]
  if (base::is.null(value) || base::length(value) == 0 || base::is.na(value[1])) {
    return(NULL)
  }
  base::trimws(base::as.character(value[1]))
}

# Normalizes either accepted shape into a common
# list(metadata, feature_tbl, difexp_tbl, feature_key, platform_field), or
# list(ok = FALSE, ...) if `uploaded` matches neither. `feature_key` says
# whether feature_tbl's rows resolve to a DB feature via `feature_name`
# (real OmicSignature objects) or already carry a resolved `feature_id`
# (this API's own /signatures/export shape). `platform_field` says which
# metadata key holds the platform value ("platform" on a real
# OmicSignature -- the OmicSignature package's own field name -- vs.
# "platform_name" on /signatures/export's output, which joins in the DB
# column name directly).
normalize_upload <- function(uploaded) {
  if (base::inherits(uploaded, "OmicSignature")) {
    return(base::list(
      ok = TRUE,
      metadata = uploaded$metadata,
      feature_tbl = uploaded$signature,
      difexp_tbl = uploaded$difexp,
      feature_key = "feature_name",
      platform_field = "platform"
    ))
  }
  if (base::is.list(uploaded) && !base::is.null(uploaded$metadata) && !base::is.null(uploaded$signature)) {
    return(base::list(
      ok = TRUE,
      metadata = uploaded$metadata,
      feature_tbl = uploaded$signature,
      difexp_tbl = uploaded$difexp,
      feature_key = "feature_id",
      platform_field = "platform_name"
    ))
  }
  base::list(
    ok = FALSE, reason = "invalid_upload",
    message = "Uploaded file must be an OmicSignature object (e.g. saveRDS(OmicSignature::OmicSignature(...))) or an .rds produced by /signatures/export."
  )
}

# Returns list(ok = FALSE, reason = "invalid_upload", message) or NULL (valid).
validate_upload_shape <- function(norm) {
  metadata <- norm$metadata
  feature_tbl <- norm$feature_tbl

  missing_meta <- base::Filter(function(f) base::is.null(meta_str(metadata, f)), REQUIRED_UPLOAD_METADATA_FIELDS)
  if (base::length(missing_meta) > 0) {
    return(base::list(
      ok = FALSE, reason = "invalid_upload",
      message = base::sprintf("Uploaded metadata is missing required field(s): %s.", base::paste(missing_meta, collapse = ", "))
    ))
  }

  required_cols <- if (norm$feature_key == "feature_name") c("probe_id", "feature_name") else c("probe_id", "feature_id")
  direction_type <- meta_str(metadata, "direction_type")
  if (!base::is.null(direction_type) && direction_type != "uni-directional") {
    required_cols <- c(required_cols, "group_label")
  }

  if (!base::is.data.frame(feature_tbl) || base::nrow(feature_tbl) == 0 ||
      base::any(!required_cols %in% base::colnames(feature_tbl))) {
    return(base::list(
      ok = FALSE, reason = "invalid_upload",
      message = base::sprintf(
        "Uploaded 'signature' must be a non-empty table with columns: %s.",
        base::paste(required_cols, collapse = ", ")
      )
    ))
  }

  NULL
}

# id_col's value for a single row matched on filter_col = filter_val, or
# NULL if no row (or more than one -- these are all unique-by-name lookup
# tables) matches.
lookup_id <- function(conn, table, id_col, filter_col, filter_val) {
  query <- base::sprintf(
    "SELECT %s FROM %s WHERE %s = %s LIMIT 1",
    id_col, table, filter_col, DBI::dbQuoteLiteral(conn, filter_val)
  )
  tbl <- DBI::dbGetQuery(conn, query)
  if (base::nrow(tbl) == 0) NULL else tbl[[id_col]][1]
}

# SQL literal for a scalar: quoted string, bare number, or NULL for
# missing/NA/empty. Mirrors the ad-hoc quoting style already used in
# api/lib/collection.R's create_collection()/api.R inserts.
sql_value <- function(conn, value) {
  if (base::is.null(value) || base::length(value) == 0 || base::is.na(value[1]) ||
      (base::is.character(value) && !base::nzchar(value[1]))) {
    return("NULL")
  }
  if (base::is.numeric(value)) {
    return(base::as.character(value[1]))
  }
  DBI::dbQuoteLiteral(conn, base::as.character(value[1]))
}

# Resolves feature_tbl's rows to DB feature_ids in `ref_table`, per
# norm$feature_key. Returns list(ok = TRUE, feature_ids = <integer vector,
# one per row of feature_tbl>) or list(ok = FALSE, reason =
# "unknown_features", message).
resolve_feature_ids <- function(conn, feature_tbl, ref_table, organism_id, feature_key) {
  if (feature_key == "feature_id") {
    requested <- base::suppressWarnings(base::as.integer(feature_tbl$feature_id))
    unique_ids <- base::unique(requested[!base::is.na(requested)])
    known_ids <- if (base::length(unique_ids) > 0) {
      DBI::dbGetQuery(conn, base::sprintf(
        "SELECT feature_id FROM %s WHERE feature_id IN (%s)", ref_table, base::paste(unique_ids, collapse = ",")
      ))$feature_id
    } else {
      base::integer(0)
    }
    unknown <- base::setdiff(unique_ids, known_ids)
    if (base::length(unknown) > 0) {
      return(base::list(
        ok = FALSE, reason = "unknown_features",
        message = base::sprintf(
          "%d uploaded feature(s) do not exist in %s and cannot be uploaded: %s.",
          base::length(unknown), ref_table, base::paste(utils::head(unknown, 20), collapse = ", ")
        )
      ))
    }
    return(base::list(ok = TRUE, feature_ids = requested))
  }

  # feature_key == "feature_name": resolve the same way
  # SigRepo::addTranscriptomicsSignatureSet()/addProteomicsSignatureSet() do
  # -- a feature_hashkey = md5(tolower(paste0(feature_name, organism_id)))
  # bulk lookup against the reference table, per-organism.
  feature_names <- base::as.character(feature_tbl$feature_name)
  hashkeys <- base::vapply(feature_names, function(fn) collection_hash(fn, organism_id), base::character(1))
  unique_hashkeys <- base::unique(hashkeys)
  lookup <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT feature_id, feature_hashkey FROM %s WHERE feature_hashkey IN (%s)",
    ref_table, base::paste(DBI::dbQuoteLiteral(conn, unique_hashkeys), collapse = ",")
  ))
  id_by_hashkey <- stats::setNames(lookup$feature_id, lookup$feature_hashkey)
  resolved <- base::unname(id_by_hashkey[hashkeys])
  unknown <- base::unique(feature_names[base::is.na(resolved)])
  if (base::length(unknown) > 0) {
    return(base::list(
      ok = FALSE, reason = "unknown_features",
      message = base::sprintf(
        "%d uploaded feature(s) do not exist in %s and cannot be uploaded: %s.",
        base::length(unknown), ref_table, base::paste(utils::head(unknown, 20), collapse = ", ")
      )
    ))
  }
  base::list(ok = TRUE, feature_ids = base::as.integer(resolved))
}

# Returns list(ok = FALSE, reason, message) or
# list(ok = TRUE, signature_hashkey, signature_name).
build_signature_from_upload <- function(auth, uploaded, visibility = FALSE, difexp_dir) {
  if (!auth$user_role %in% c("editor", "admin")) {
    return(base::list(ok = FALSE, reason = "forbidden"))
  }

  norm <- normalize_upload(uploaded)
  if (!norm$ok) {
    return(norm)
  }
  shape_error <- validate_upload_shape(norm)
  if (!base::is.null(shape_error)) {
    return(shape_error)
  }

  metadata <- norm$metadata
  feature_tbl <- norm$feature_tbl
  difexp_tbl <- norm$difexp_tbl

  assay_type <- meta_str(metadata, "assay_type")
  ref_table <- enrichment_reference_table(assay_type)
  if (base::is.null(ref_table)) {
    return(base::list(
      ok = FALSE, reason = "unsupported_assay_type",
      message = base::sprintf("Signature upload supports assay_type in {transcriptomics, proteomics}; got '%s'.", assay_type)
    ))
  }

  signature_name <- meta_str(metadata, "signature_name")
  hashkey <- collection_hash(signature_name, auth$user_name)

  conn <- NULL
  base::tryCatch({
    conn <- db_connect_local()

    existing <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hashkey)
    if (!base::is.null(existing)) {
      return(base::list(ok = FALSE, reason = "duplicate", message = "You already have a signature with this name."))
    }

    organism_id <- lookup_id(conn, "organisms", "organism_id", "organism", meta_str(metadata, "organism"))
    if (base::is.null(organism_id)) {
      return(base::list(ok = FALSE, reason = "invalid_upload", message = base::sprintf("Unknown organism: '%s'.", meta_str(metadata, "organism"))))
    }
    platform_value <- meta_str(metadata, norm$platform_field) %||% "unknown"
    platform_id <- lookup_id(conn, "platforms", "platform_id", "platform_name", platform_value)
    if (base::is.null(platform_id)) {
      return(base::list(ok = FALSE, reason = "invalid_upload", message = base::sprintf("Unknown platform: '%s'.", platform_value)))
    }
    sample_type_value <- meta_str(metadata, "sample_type") %||% "unknown"
    sample_type_id <- lookup_id(conn, "sample_types", "sample_type_id", "sample_type", sample_type_value)
    if (base::is.null(sample_type_id)) {
      return(base::list(ok = FALSE, reason = "invalid_upload", message = base::sprintf("Unknown sample_type: '%s'.", sample_type_value)))
    }

    phenotype_id <- lookup_id(conn, "phenotypes", "phenotype_id", "phenotype", meta_str(metadata, "phenotype"))
    if (base::is.null(phenotype_id)) {
      DBI::dbExecute(conn, base::sprintf("INSERT INTO phenotypes (phenotype) VALUES (%s)", DBI::dbQuoteLiteral(conn, meta_str(metadata, "phenotype"))))
      phenotype_id <- lookup_id(conn, "phenotypes", "phenotype_id", "phenotype", meta_str(metadata, "phenotype"))
    }

    # Every uploaded feature must already resolve in the target reference
    # table -- no auto-creating new features from an upload.
    resolved <- resolve_feature_ids(conn, feature_tbl, ref_table, organism_id, norm$feature_key)
    if (!resolved$ok) {
      return(resolved)
    }
    feature_ids <- resolved$feature_ids

    has_difexp <- base::is.data.frame(difexp_tbl) && base::nrow(difexp_tbl) > 0
    scores <- if ("score" %in% base::colnames(feature_tbl)) base::suppressWarnings(base::as.numeric(feature_tbl$score)) else base::rep(NA_real_, base::nrow(feature_tbl))
    num_up <- base::sum(scores > 0, na.rm = TRUE)
    num_down <- base::sum(scores < 0, na.rm = TRUE)

    insert_cols <- c(
      "signature_name", "organism_id", "direction_type", "assay_type", "phenotype_id", "platform_id", "sample_type_id",
      "covariates", "description", "score_cutoff", "logfc_cutoff", "p_value_cutoff", "adj_p_cutoff", "cutoff_description",
      "keywords", "PMID", "year", "others", "has_difexp", "num_of_difexp", "num_up_regulated", "num_down_regulated",
      "user_name", "visibility", "signature_hashkey"
    )
    insert_vals <- c(
      sql_value(conn, signature_name),
      base::as.character(organism_id),
      sql_value(conn, meta_str(metadata, "direction_type")),
      sql_value(conn, assay_type),
      base::as.character(phenotype_id),
      base::as.character(platform_id),
      base::as.character(sample_type_id),
      sql_value(conn, metadata$covariates),
      sql_value(conn, metadata$description),
      sql_value(conn, metadata$score_cutoff),
      sql_value(conn, metadata$logfc_cutoff),
      sql_value(conn, metadata$p_value_cutoff),
      sql_value(conn, metadata$adj_p_cutoff),
      sql_value(conn, metadata$cutoff_description),
      sql_value(conn, metadata$keywords),
      sql_value(conn, metadata$PMID),
      sql_value(conn, metadata$year),
      sql_value(conn, metadata$others),
      base::as.character(base::as.integer(has_difexp)),
      base::as.character(if (has_difexp) base::nrow(difexp_tbl) else 0L),
      base::as.character(num_up),
      base::as.character(num_down),
      sql_value(conn, auth$user_name),
      base::as.character(if (base::isTRUE(visibility)) 1L else 0L),
      sql_value(conn, hashkey)
    )
    DBI::dbExecute(conn, base::sprintf(
      "INSERT INTO signatures (%s) VALUES (%s)",
      base::paste(insert_cols, collapse = ", "), base::paste(insert_vals, collapse = ", ")
    ))

    signature_id <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hashkey)

    rollback <- function() {
      DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", signature_id))
      DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_access WHERE signature_id = %d", signature_id))
      DBI::dbExecute(conn, base::sprintf("DELETE FROM signatures WHERE signature_id = %d", signature_id))
    }

    access_hash <- collection_hash(signature_id, auth$user_name)
    has_group_label <- "group_label" %in% base::colnames(feature_tbl)
    write_result <- base::tryCatch({
      DBI::dbExecute(conn, base::sprintf(
        "INSERT INTO signature_access (signature_id, user_name, access_type, access_sig_hashkey) VALUES (%d, %s, 'owner', %s)",
        signature_id, DBI::dbQuoteLiteral(conn, auth$user_name), DBI::dbQuoteLiteral(conn, access_hash)
      ))

      if (has_difexp) {
        save_difexp_rds(difexp_dir, hashkey, difexp_tbl)
      }

      feature_rows <- base::lapply(base::seq_len(base::nrow(feature_tbl)), function(i) {
        row <- feature_tbl[i, , drop = FALSE]
        group_label <- if (has_group_label) base::trimws(base::as.character(row$group_label[1])) else NA_character_
        if (base::is.na(group_label) || !base::nzchar(group_label)) group_label <- "All Features"
        feature_id <- feature_ids[i]
        probe_id <- base::as.character(row$probe_id[1])
        score <- scores[i]
        sig_feature_hash <- collection_hash(signature_id, feature_id, assay_type, probe_id)
        base::sprintf(
          "(%d, %d, %s, %s, %s, %s, %s)",
          signature_id, feature_id, DBI::dbQuoteLiteral(conn, probe_id),
          if (base::is.na(score)) "NULL" else base::as.character(score),
          DBI::dbQuoteLiteral(conn, group_label), DBI::dbQuoteLiteral(conn, assay_type),
          DBI::dbQuoteLiteral(conn, sig_feature_hash)
        )
      })
      DBI::dbExecute(conn, base::sprintf(
        "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey) VALUES %s",
        base::paste(feature_rows, collapse = ", ")
      ))

      base::list(ok = TRUE)
    }, error = function(err) {
      base::list(ok = FALSE, reason = "write_failed", message = err$message)
    })

    if (!write_result$ok) {
      rollback()
      return(write_result)
    }

    base::list(ok = TRUE, signature_hashkey = hashkey, signature_name = signature_name)
  }, finally = {
    if (!base::is.null(conn)) base::suppressWarnings(DBI::dbDisconnect(conn))
  })
}
