# Signature context retrieval and similarity/grouping logic backing the
# /read/signature_context and /read/group_signatures endpoints.
# Depends on api/lib/common.R (db_connect_local, compact_table).

fetch_signature_context <- function(signature_hashkey, include_features = TRUE, max_features = 50, auth = NULL) {
  conn <- NULL

  base::tryCatch({
    conn <- db_connect_local()

    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = "*",
      filter_coln_var = "signature_hashkey",
      filter_coln_val = base::list("signature_hashkey" = signature_hashkey),
      check_db_table = TRUE
    )

    if (base::nrow(signature_tbl) == 0) {
      return(NULL)
    }

    if (
      !is.null(auth) &&
        !identical(auth$user_role, "admin") &&
        "visibility" %in% base::colnames(signature_tbl) &&
        !base::isTRUE(base::as.logical(signature_tbl$visibility[1]))
    ) {
      access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access",
        return_var = c("signature_id", "user_name", "access_type"),
        filter_coln_var = c("signature_id", "user_name", "access_type"),
        filter_coln_val = base::list(
          "signature_id" = signature_tbl$signature_id[1],
          "user_name" = auth$user_name,
          "access_type" = c("owner", "editor", "viewer")
        ),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      )

      if (base::nrow(access_tbl) == 0) {
        return(NULL)
      }
    }

    organism_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "organisms",
      return_var = c("organism_id", "organism"),
      filter_coln_var = "organism_id",
      filter_coln_val = base::list("organism_id" = signature_tbl$organism_id),
      check_db_table = TRUE
    )

    phenotype_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "phenotypes",
      return_var = c("phenotype_id", "phenotype"),
      filter_coln_var = "phenotype_id",
      filter_coln_val = base::list("phenotype_id" = signature_tbl$phenotype_id),
      check_db_table = TRUE
    )

    sample_type_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "sample_types",
      return_var = c("sample_type_id", "sample_type"),
      filter_coln_var = "sample_type_id",
      filter_coln_val = base::list("sample_type_id" = signature_tbl$sample_type_id),
      check_db_table = TRUE
    )

    platform_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "platforms",
      return_var = c("platform_id", "platform_name"),
      filter_coln_var = "platform_id",
      filter_coln_val = base::list("platform_id" = signature_tbl$platform_id),
      check_db_table = TRUE
    )

    signature_tbl <- signature_tbl |>
      dplyr::left_join(organism_tbl, by = "organism_id") |>
      dplyr::left_join(phenotype_tbl, by = "phenotype_id") |>
      dplyr::left_join(sample_type_tbl, by = "sample_type_id") |>
      dplyr::left_join(platform_tbl, by = "platform_id")

    feature_tbl <- base::data.frame()
    if (include_features) {
      feature_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_feature_set",
        return_var = "*",
        filter_coln_var = "signature_id",
        filter_coln_val = base::list("signature_id" = signature_tbl$signature_id[1]),
        check_db_table = TRUE
      )

      if (base::nrow(feature_tbl) > 0 && "score" %in% base::colnames(feature_tbl)) {
        feature_tbl <- feature_tbl |>
          dplyr::arrange(dplyr::desc(base::abs(.data$score)))
      }
    }

    base::list(
      signature = base::as.list(signature_tbl[1, , drop = FALSE]),
      feature_count = base::nrow(feature_tbl),
      features = compact_table(feature_tbl, max_rows = max_features)
    )
  }, finally = {
    if (!is.null(conn)) {
      base::suppressWarnings(DBI::dbDisconnect(conn))
    }
  })
}

fetch_signature_contexts <- function(signature_hashkeys, include_features = TRUE, max_features = 200, auth = NULL) {
  contexts <- base::lapply(signature_hashkeys, function(hashkey) {
    fetch_signature_context(
      signature_hashkey = hashkey,
      include_features = include_features,
      max_features = max_features,
      auth = auth
    )
  })

  base::names(contexts) <- signature_hashkeys
  contexts[!base::vapply(contexts, is.null, logical(1))]
}

feature_ids_from_context <- function(context) {
  if (is.null(context$features) || base::length(context$features) == 0) {
    return(base::character())
  }

  ids <- base::vapply(context$features, function(row) {
    if (!is.null(row$probe_id) && !is.na(row$probe_id)) {
      return(base::as.character(row$probe_id))
    }

    if (!is.null(row$feature_id) && !is.na(row$feature_id)) {
      return(base::as.character(row$feature_id))
    }

    ""
  }, character(1))

  base::unique(ids[ids != ""])
}

signature_similarity_summary <- function(contexts) {
  hashkeys <- base::names(contexts)

  if (base::length(hashkeys) < 2) {
    return(base::data.frame())
  }

  feature_sets <- base::lapply(contexts, feature_ids_from_context)
  pairs <- utils::combn(hashkeys, 2, simplify = FALSE)

  do.call(base::rbind, base::lapply(pairs, function(pair) {
    left <- feature_sets[[pair[1]]]
    right <- feature_sets[[pair[2]]]
    intersection_n <- base::length(base::intersect(left, right))
    union_n <- base::length(base::union(left, right))

    base::data.frame(
      signature_hashkey_1 = pair[1],
      signature_hashkey_2 = pair[2],
      features_1 = base::length(left),
      features_2 = base::length(right),
      shared_features = intersection_n,
      jaccard_similarity = if (union_n == 0) 0 else intersection_n / union_n,
      stringsAsFactors = FALSE
    )
  }))
}

draft_signature_groups <- function(similarity_tbl, threshold = 0.10) {
  if (is.null(similarity_tbl) || base::nrow(similarity_tbl) == 0) {
    return(base::list())
  }

  hashkeys <- base::unique(c(similarity_tbl$signature_hashkey_1, similarity_tbl$signature_hashkey_2))
  groups <- base::as.list(hashkeys)
  base::names(groups) <- hashkeys

  for (i in base::seq_len(base::nrow(similarity_tbl))) {
    if (similarity_tbl$jaccard_similarity[i] >= threshold) {
      left <- similarity_tbl$signature_hashkey_1[i]
      right <- similarity_tbl$signature_hashkey_2[i]
      merged <- base::unique(c(groups[[left]], groups[[right]]))
      for (hashkey in merged) {
        groups[[hashkey]] <- merged
      }
    }
  }

  unique_groups <- base::unique(base::lapply(groups, sort))
  base::names(unique_groups) <- base::sprintf("group_%s", base::seq_along(unique_groups))
  unique_groups
}
