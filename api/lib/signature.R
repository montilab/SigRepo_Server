# Signature context retrieval and similarity/grouping logic backing the
# /read/signature_context and /read/group_signatures endpoints.
# Depends on api/lib/common.R (db_connect_local, compact_table).

# Compact search/list results for the Signatures page. Deliberately does not
# consult the per-signature signature_access ACL the way fetch_signature_context()
# does for a single record -- that's an expensive join across every candidate
# row. A non-admin user with explicit access to a hidden signature can still
# retrieve it directly via /read/signature_context if they already know its
# hashkey; it just won't surface via search. Under-returning is the safe default.
# Returns a paginated page of results as list(rows = <data.frame>,
# total = <integer count of all matching rows>). `total` backs server-side
# pagination on the Signatures page (DT-style server = TRUE): the client asks
# for one `limit`-sized page at `offset` and renders pager controls from
# `total`, instead of pulling every row up front.
# Columns the Signatures list may be ordered by, mapped to the SQL expression
# that actually sorts them.
#
# An allowlist rather than interpolating the caller's string: this value reaches
# ORDER BY, where quoting cannot protect it the way dbQuoteLiteral protects a
# WHERE value. Anything not in this map falls back to signature_name.
#
# The joined names sort by the human-readable label rather than the underlying
# *_id -- sorting by foreign key would look arbitrary to a reader.
.signature_sort_columns <- base::list(
  signature_name = "s.signature_name",
  organism       = "o.organism",
  assay_type     = "s.assay_type",
  direction_type = "s.direction_type",
  phenotype      = "p.phenotype",
  sample_type    = "st.sample_type",
  platform_name  = "pl.platform_name",
  year           = "s.year",
  user_name      = "s.user_name",
  visibility     = "s.visibility",
  signature_source = "s.signature_source"
)

search_signatures <- function(conn, organism = NULL, phenotype = NULL, assay_type = NULL,
                               keyword = NULL, limit = 20, offset = 0, is_admin = FALSE,
                               sort_by = NULL, sort_dir = "asc", signature_source = NULL) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 20
  }
  limit <- base::min(limit, 100)

  offset <- base::suppressWarnings(base::as.integer(offset[1]))
  if (base::is.na(offset) || offset < 0) {
    offset <- 0
  }

  # One FROM + WHERE, shared by the COUNT(*) and the page query so the total
  # and the page always agree on which rows match.
  from_where <- "
    FROM signatures s
    LEFT JOIN organisms o ON s.organism_id = o.organism_id
    LEFT JOIN phenotypes p ON s.phenotype_id = p.phenotype_id
    LEFT JOIN sample_types st ON s.sample_type_id = st.sample_type_id
    LEFT JOIN platforms pl ON s.platform_id = pl.platform_id
    WHERE 1=1
  "

  if (!is_admin) {
    from_where <- base::paste(from_where, "AND s.visibility = 1")
  }
  if (!is.null(organism) && base::nzchar(base::trimws(organism[1]))) {
    from_where <- base::paste(from_where, "AND o.organism =", DBI::dbQuoteLiteral(conn, base::trimws(organism[1])))
  }
  if (!is.null(phenotype) && base::nzchar(base::trimws(phenotype[1]))) {
    from_where <- base::paste(from_where, "AND p.phenotype =", DBI::dbQuoteLiteral(conn, base::trimws(phenotype[1])))
  }
  if (!is.null(assay_type) && base::nzchar(base::trimws(assay_type[1]))) {
    from_where <- base::paste(from_where, "AND s.assay_type =", DBI::dbQuoteLiteral(conn, base::trimws(assay_type[1])))
  }
  # Exact match, not LIKE: the values are a controlled set written by the code
  # that creates each signature, so a caller asking for "rummagene" wants
  # exactly that and not some future "rummagene-preprints".
  if (!is.null(signature_source) && base::nzchar(base::trimws(signature_source[1]))) {
    from_where <- base::paste(from_where, "AND s.signature_source =",
                              DBI::dbQuoteLiteral(conn, base::trimws(signature_source[1])))
  }
  if (!is.null(keyword) && base::nzchar(base::trimws(keyword[1]))) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", base::trimws(keyword[1])))
    from_where <- base::paste(from_where, base::sprintf(
      "AND (s.signature_name LIKE %s OR s.description LIKE %s OR s.keywords LIKE %s)",
      like, like, like
    ))
  }

  total <- DBI::dbGetQuery(conn, base::paste("SELECT COUNT(*) AS n", from_where))$n[1]

  # s.* plus the human-readable joined names instead of raw *_id foreign keys.
  # feature_count is intentionally NOT selected here: the Signatures list no
  # longer shows it, and its per-row correlated subquery was the main cost of
  # this query. The detail view computes its own count when a row is opened.
  # Sorting has to happen in SQL, not in the browser: the client holds one page,
  # so a client-side sort would silently reorder within the page and look like
  # it had sorted the whole repository.
  sort_key <- base::trimws(base::as.character(sort_by %||% ""))
  sort_expr <- if (base::nzchar(sort_key)) .signature_sort_columns[[sort_key]] else NULL
  if (base::is.null(sort_expr)) {
    sort_expr <- "s.signature_name"
  }
  sort_dir_sql <- if (base::identical(base::tolower(base::trimws(base::as.character(sort_dir %||% "asc"))), "desc")) "DESC" else "ASC"

  # signature_name breaks ties so paging stays stable: without a unique
  # tiebreaker, rows with equal sort values can show up on two pages or none.
  order_clause <- if (base::identical(sort_expr, "s.signature_name")) {
    base::paste("ORDER BY s.signature_name", sort_dir_sql)
  } else {
    base::paste0("ORDER BY ", sort_expr, " ", sort_dir_sql, ", s.signature_name ASC")
  }

  query <- base::paste(
    "SELECT s.*, o.organism, p.phenotype, st.sample_type, pl.platform_name",
    from_where,
    order_clause, "LIMIT", limit, "OFFSET", offset
  )

  base::list(rows = DBI::dbGetQuery(conn, query), total = total)
}

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

      # Add feature_name / gene_symbol so a reader sees the gene rather than
      # the positional probe id. Purely additive: annotate.R and rummagene.R
      # read probe_id and feature_id off these rows by name, and export.R
      # bind_rows()es them, so extra columns are safe for every consumer.
      feature_tbl <- attach_feature_labels(
        conn, feature_tbl,
        assay_type = base::as.character(signature_tbl$assay_type[1])
      )
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

# Delete a signature and its child rows, authorizing against the *calling*
# user (auth$user_name/auth$user_role, resolved from their api_key by
# validate_api_key()) rather than SigRepo::deleteSignature()'s own
# checkPermissions(), which authorizes against the DB connection's own
# login. The REST API always connects as one shared service account
# (see conn_handler in api.R), so checkPermissions() would authorize every
# request as that shared account instead of the real api_key holder -- it
# only does the right thing in Shiny, where each session's DB connection
# genuinely is logged in as that person (see shiny/app_src/app_server.R).
# Depends on api/lib/common.R (db_connect_local) and api/lib/difexp.R
# (delete_difexp_rds) and the `difexp_dir` global defined in api.R.
#
# Returns list(ok = TRUE, signature_name = ...) on success, or
# list(ok = FALSE, reason = "not_found" | "forbidden") otherwise.
delete_signature <- function(auth, signature_hashkey) {
  conn <- NULL

  base::tryCatch({
    conn <- db_connect_local()

    signature_tbl <- SigRepo::lookup_table_sql(
      conn = conn,
      db_table_name = "signatures",
      return_var = c("signature_id", "signature_name", "user_name", "has_difexp"),
      filter_coln_var = "signature_hashkey",
      filter_coln_val = base::list("signature_hashkey" = signature_hashkey),
      check_db_table = TRUE
    )

    if (base::nrow(signature_tbl) == 0) {
      return(base::list(ok = FALSE, reason = "not_found"))
    }

    signature_id <- signature_tbl$signature_id[1]
    owner <- signature_tbl$user_name[1]
    signature_name <- signature_tbl$signature_name[1]
    has_difexp <- base::isTRUE(base::as.logical(signature_tbl$has_difexp[1]))

    if (!auth$user_role %in% c("editor", "admin")) {
      return(base::list(ok = FALSE, reason = "forbidden"))
    }

    if (!identical(auth$user_role, "admin") && !identical(auth$user_name, owner)) {
      access_tbl <- SigRepo::lookup_table_sql(
        conn = conn,
        db_table_name = "signature_access",
        return_var = c("signature_id", "user_name", "access_type"),
        filter_coln_var = c("signature_id", "user_name", "access_type"),
        filter_coln_val = base::list(
          "signature_id" = signature_id,
          "user_name" = auth$user_name,
          "access_type" = c("owner", "editor")
        ),
        filter_var_by = c("AND", "AND"),
        check_db_table = TRUE
      )

      if (base::nrow(access_tbl) == 0) {
        return(base::list(ok = FALSE, reason = "forbidden"))
      }
    }

    # Children before parent so this works without disabling FK checks.
    DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", signature_id))
    DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_access WHERE signature_id = %d", signature_id))
    DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_collection_access WHERE signature_id = %d", signature_id))
    DBI::dbExecute(conn, base::sprintf("DELETE FROM signatures WHERE signature_id = %d", signature_id))

    if (has_difexp) {
      delete_difexp_rds(difexp_dir, signature_hashkey)
    }

    base::list(ok = TRUE, signature_name = signature_name)
  }, finally = {
    if (!is.null(conn)) {
      base::suppressWarnings(DBI::dbDisconnect(conn))
    }
  })
}

# Gene-content search: given gene symbols, which signatures contain them?
#
# The reverse of the Rummagene lookup, and the direction that lets someone
# arrive with a gene list and discover signatures without knowing any of them by
# name. Needs the indexes from scripts/migrate_gene_search_indexes.R to be fast;
# it is correct without them, just slow.
#
# The join is keyed on BOTH feature_id and assay_type, which is load-bearing:
# feature_id is a separate AUTO_INCREMENT per feature table, so the same id
# means different genes in transcriptomics_features and proteomics_features and
# their ranges overlap. Joining on feature_id alone silently matches proteomics
# features against transcriptomics ones.
.gene_search_feature_tables <- base::list(
  transcriptomics = "transcriptomics_features",
  proteomics = "proteomics_features"
)

# Returns a data frame ordered by overlap: signature_hashkey, signature_name,
# organism, phenotype, assay_type, n_overlap, n_signature_genes, jaccard,
# and matched_genes (comma-separated, capped).
search_signatures_by_genes <- function(conn, genes, limit = 20, min_overlap = 1,
                                       exclude_hashkey = NULL, is_admin = FALSE) {
  genes <- base::unique(base::toupper(base::trimws(base::as.character(genes))))
  genes <- genes[!base::is.na(genes) & base::nzchar(genes)]
  if (base::length(genes) == 0) {
    return(base::data.frame())
  }

  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) limit <- 20
  limit <- base::min(limit, 100)

  min_overlap <- base::suppressWarnings(base::as.integer(min_overlap[1]))
  if (base::is.na(min_overlap) || min_overlap < 1) min_overlap <- 1

  gene_list <- base::paste(
    base::vapply(genes, function(g) DBI::dbQuoteLiteral(conn, g), character(1)),
    collapse = ", "
  )

  # One branch per feature table, unioned. Each carries its own assay_type so
  # the join back to signature_feature_set cannot cross tables.
  branches <- base::vapply(
    base::names(.gene_search_feature_tables),
    function(assay) base::sprintf(
      "SELECT feature_id, UPPER(gene_symbol) AS gene_symbol, %s AS assay_type FROM %s WHERE UPPER(gene_symbol) IN (%s)",
      DBI::dbQuoteLiteral(conn, assay), .gene_search_feature_tables[[assay]], gene_list
    ),
    character(1)
  )
  matched_features <- base::paste(branches, collapse = " UNION ALL ")

  where_visibility <- if (is_admin) "" else " AND s.visibility = 1"
  where_exclude <- if (!base::is.null(exclude_hashkey) && base::nzchar(exclude_hashkey)) {
    base::paste(" AND s.signature_hashkey <>", DBI::dbQuoteLiteral(conn, exclude_hashkey))
  } else {
    ""
  }

  query <- base::sprintf("
    SELECT s.signature_hashkey, s.signature_name, s.assay_type,
           o.organism, p.phenotype,
           COUNT(DISTINCT g.gene_symbol) AS n_overlap,
           GROUP_CONCAT(DISTINCT g.gene_symbol ORDER BY g.gene_symbol SEPARATOR ',') AS matched_genes
    FROM (%s) g
    JOIN signature_feature_set sfs
      ON sfs.feature_id = g.feature_id AND sfs.assay_type = g.assay_type
    JOIN signatures s ON s.signature_id = sfs.signature_id
    LEFT JOIN organisms o ON s.organism_id = o.organism_id
    LEFT JOIN phenotypes p ON s.phenotype_id = p.phenotype_id
    WHERE 1=1%s%s
    GROUP BY s.signature_id
    HAVING n_overlap >= %d
    ORDER BY n_overlap DESC, s.signature_name ASC
    LIMIT %d",
    matched_features, where_visibility, where_exclude, min_overlap, limit
  )

  hits <- DBI::dbGetQuery(conn, query)
  if (base::nrow(hits) == 0) {
    return(base::data.frame())
  }

  # Jaccard needs each hit's own gene count. Done as a second query over just
  # the returned signatures rather than a correlated subquery in the one above,
  # which would compute it for every candidate before the LIMIT.
  hit_keys <- base::paste(
    base::vapply(hits$signature_hashkey, function(k) DBI::dbQuoteLiteral(conn, k), character(1)),
    collapse = ", "
  )
  size_branches <- base::vapply(
    base::names(.gene_search_feature_tables),
    function(assay) base::sprintf(
      "SELECT s.signature_hashkey, UPPER(f.gene_symbol) AS gene_symbol
         FROM signatures s
         JOIN signature_feature_set sfs ON sfs.signature_id = s.signature_id
         JOIN %s f ON f.feature_id = sfs.feature_id
        WHERE sfs.assay_type = %s AND s.signature_hashkey IN (%s)
          AND f.gene_symbol IS NOT NULL AND f.gene_symbol <> ''",
      .gene_search_feature_tables[[assay]], DBI::dbQuoteLiteral(conn, assay), hit_keys
    ),
    character(1)
  )
  sizes <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT signature_hashkey, COUNT(DISTINCT gene_symbol) AS n_signature_genes FROM (%s) t GROUP BY signature_hashkey",
    base::paste(size_branches, collapse = " UNION ALL ")
  ))

  hits$n_signature_genes <- sizes$n_signature_genes[base::match(hits$signature_hashkey, sizes$signature_hashkey)]
  hits$n_signature_genes[base::is.na(hits$n_signature_genes)] <- 0L
  hits$n_query_genes <- base::length(genes)

  union_size <- hits$n_query_genes + hits$n_signature_genes - hits$n_overlap
  hits$jaccard <- base::ifelse(union_size > 0, base::round(hits$n_overlap / union_size, 5), 0)

  hits
}

# Add human-readable labels to a signature_feature_set table.
#
# signature_feature_set stores probe_id and feature_id but no name, so a
# signature that arrived without its own probe ids renders as "feature_1",
# "feature_10", ... -- OmicSignature's positional filler, useless to a reader.
# This joins the reference table for the signature's assay type to add
# `feature_name` (the stored identifier) and `gene_symbol` (the readable one).
#
# Always returns BOTH columns, even when nothing can populate them: the client
# reads gene_symbol off every row, so the shape has to be stable. Rows are
# never dropped -- a feature whose reference row is missing keeps its place
# with NA labels, because losing a member would misreport the signature's size.
#
# Reuses upload_reference_table() / upload_reference_id_column() from
# api/lib/create_signature.R rather than adding a parallel mapping; those
# already cover all four assay types and know that metabolite_reference keys on
# metabolite_id rather than feature_id.
attach_feature_labels <- function(conn, feature_tbl, assay_type) {
  # Assigning a scalar into a zero-row data frame is an error, so the empty
  # case is handled before anything else -- it still has to come back carrying
  # both columns.
  if (base::nrow(feature_tbl) == 0) {
    feature_tbl$feature_name <- base::character(0)
    feature_tbl$gene_symbol <- base::character(0)
    return(feature_tbl)
  }

  feature_tbl$feature_name <- NA_character_
  feature_tbl$gene_symbol <- NA_character_
  if (!"feature_id" %in% base::colnames(feature_tbl)) {
    return(feature_tbl)
  }

  ref_table <- upload_reference_table(assay_type)
  if (base::is.null(ref_table)) {
    return(feature_tbl)
  }
  id_col <- upload_reference_id_column(ref_table)

  # Which columns this particular reference table actually carries. Only
  # transcriptomics and proteomics have gene_symbol; genetic variants have a
  # feature_name but no symbol; metabolites have neither and are labelled by
  # their RefMet name instead.
  available <- base::tryCatch(
    DBI::dbListFields(conn, ref_table), error = function(e) base::character(0)
  )
  label_col <- base::intersect(c("feature_name", "refmet_name"), available)[1]
  symbol_col <- base::intersect("gene_symbol", available)[1]
  if (base::is.na(label_col) && base::is.na(symbol_col)) {
    return(feature_tbl)
  }

  ids <- base::unique(base::as.integer(feature_tbl$feature_id))
  ids <- ids[!base::is.na(ids)]
  if (base::length(ids) == 0) {
    return(feature_tbl)
  }

  wanted <- c(id_col, label_col, symbol_col)
  wanted <- wanted[!base::is.na(wanted)]
  ref <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT %s FROM %s WHERE %s IN (%s)",
    base::paste(wanted, collapse = ", "), ref_table, id_col,
    base::paste(ids, collapse = ",")
  ))
  if (base::nrow(ref) == 0) {
    return(feature_tbl)
  }

  pos <- base::match(base::as.integer(feature_tbl$feature_id), ref[[id_col]])
  if (!base::is.na(label_col)) {
    feature_tbl$feature_name <- ref[[label_col]][pos]
  }
  if (!base::is.na(symbol_col)) {
    feature_tbl$gene_symbol <- ref[[symbol_col]][pos]
  }
  feature_tbl
}
