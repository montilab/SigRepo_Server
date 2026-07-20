# New read-only query logic backing the MCP tools in mcp/lib/tools.R.
# Depends on api/lib/common.R (db_connect_local) and, for compare_two_signatures,
# api/lib/signature.R (fetch_signature_contexts, signature_similarity_summary).

list_vocabulary <- function(conn) {
  organism <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT o.organism FROM organisms o
    INNER JOIN signatures s ON s.organism_id = o.organism_id
    ORDER BY o.organism
  ")$organism

  phenotype <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT p.phenotype FROM phenotypes p
    INNER JOIN signatures s ON s.phenotype_id = p.phenotype_id
    ORDER BY p.phenotype
  ")$phenotype

  sample_type <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT st.sample_type FROM sample_types st
    INNER JOIN signatures s ON s.sample_type_id = st.sample_type_id
    ORDER BY st.sample_type
  ")$sample_type

  platform <- DBI::dbGetQuery(conn, "
    SELECT DISTINCT pl.platform_name FROM platforms pl
    INNER JOIN signatures s ON s.platform_id = pl.platform_id
    ORDER BY pl.platform_name
  ")$platform_name

  assay_type <- DBI::dbGetQuery(conn, "SELECT DISTINCT assay_type FROM signatures ORDER BY assay_type")$assay_type

  base::list(
    organism = organism,
    phenotype = phenotype,
    sample_type = sample_type,
    platform = platform,
    assay_type = assay_type
  )
}

# is_admin controls whether hidden (visibility = 0) signatures are included.
# Search deliberately does NOT consult the per-signature signature_access ACL
# the way fetch_signature_context() does for a single record -- that's an
# expensive join across every candidate row. A non-admin user with explicit
# access to a hidden signature can still retrieve it directly via
# get_signature_context if they already know its hashkey; it just won't
# surface via search. Under-returning is the safe default here.
search_signatures <- function(conn, organism = NULL, phenotype = NULL, assay_type = NULL,
                               keyword = NULL, limit = 20, is_admin = FALSE) {
  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) {
    limit <- 20
  }
  limit <- base::min(limit, 100)

  query <- "
    SELECT s.signature_hashkey, s.signature_name, o.organism, p.phenotype,
           s.assay_type, s.description,
           (SELECT COUNT(*) FROM signature_feature_set sfs WHERE sfs.signature_id = s.signature_id) AS feature_count
    FROM signatures s
    LEFT JOIN organisms o ON s.organism_id = o.organism_id
    LEFT JOIN phenotypes p ON s.phenotype_id = p.phenotype_id
    WHERE 1=1
  "

  if (!is_admin) {
    query <- base::paste(query, "AND s.visibility = 1")
  }
  if (!is.null(organism) && base::nzchar(base::trimws(organism[1]))) {
    query <- base::paste(query, "AND o.organism =", DBI::dbQuoteLiteral(conn, base::trimws(organism[1])))
  }
  if (!is.null(phenotype) && base::nzchar(base::trimws(phenotype[1]))) {
    query <- base::paste(query, "AND p.phenotype =", DBI::dbQuoteLiteral(conn, base::trimws(phenotype[1])))
  }
  if (!is.null(assay_type) && base::nzchar(base::trimws(assay_type[1]))) {
    query <- base::paste(query, "AND s.assay_type =", DBI::dbQuoteLiteral(conn, base::trimws(assay_type[1])))
  }
  if (!is.null(keyword) && base::nzchar(base::trimws(keyword[1]))) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", base::trimws(keyword[1])))
    query <- base::paste(query, base::sprintf(
      "AND (s.signature_name LIKE %s OR s.description LIKE %s OR s.keywords LIKE %s)",
      like, like, like
    ))
  }

  query <- base::paste(query, "ORDER BY s.signature_name ASC LIMIT", limit)

  DBI::dbGetQuery(conn, query)
}

compare_two_signatures <- function(signature_hashkey_1, signature_hashkey_2, auth = NULL, max_features = 200) {
  if (identical(signature_hashkey_1, signature_hashkey_2)) {
    stop("signature_hashkey_1 and signature_hashkey_2 must be different signatures.")
  }

  contexts <- fetch_signature_contexts(
    signature_hashkeys = c(signature_hashkey_1, signature_hashkey_2),
    include_features = TRUE,
    max_features = max_features,
    auth = auth
  )

  if (base::length(contexts) < 2) {
    missing <- base::setdiff(c(signature_hashkey_1, signature_hashkey_2), base::names(contexts))
    stop(base::sprintf("Could not find signature(s): %s", base::paste(missing, collapse = ", ")))
  }

  similarity_tbl <- signature_similarity_summary(contexts)

  base::list(
    signature_hashkey_1 = signature_hashkey_1,
    signature_hashkey_2 = signature_hashkey_2,
    signature_name_1 = contexts[[signature_hashkey_1]]$signature$signature_name,
    signature_name_2 = contexts[[signature_hashkey_2]]$signature$signature_name,
    features_1 = similarity_tbl$features_1[1],
    features_2 = similarity_tbl$features_2[1],
    shared_features = similarity_tbl$shared_features[1],
    jaccard_similarity = similarity_tbl$jaccard_similarity[1]
  )
}
