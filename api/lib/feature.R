# Reference feature search, backing the Browse page.
#
# WHY THIS EXISTS AT ALL: the Browse page rendered five hardcoded genes from
# web/src/data/mock.ts. It was not merely fake -- it MISREPRESENTED the schema.
# It showed a "chromosome" column for transcriptomics features, which that table
# does not have, and it put the gene SYMBOL in feature_name and the Ensembl id in
# a "gene_id" column, which is backwards: transcriptomics_features stores the
# Ensembl id in feature_name and the symbol in gene_symbol. Anyone reading that
# page came away with the wrong model of the database.
#
# WHY NOT REUSE mcp/lib/queries.R's search_features(): that one matches
# feature_name EXACTLY (`feature_name = 'TP53'`), which is right for an agent
# that already knows the identifier it wants and useless for a person browsing.
# It also does not select gene_symbol, and returns no total, so it cannot drive
# a paged table. The two have different jobs; this is not duplication for its
# own sake.
#
# Depends on api/lib/common.R.

# Which table holds the reference features for each assay type, and which
# columns that table actually has. The column lists are deliberately explicit
# rather than SELECT * -- naming them is what stops this page from ever again
# promising a field the database does not hold.
FEATURE_SOURCES <- base::list(
  transcriptomics = base::list(
    table = "transcriptomics_features",
    columns = base::c("feature_name", "gene_symbol")
  ),
  proteomics = base::list(
    table = "proteomics_features",
    columns = base::c("feature_name", "gene_symbol")
  ),
  snps = base::list(
    table = "genetic_variants_features",
    columns = base::c("feature_name", "chromosome", "position", "annotation")
  )
)

# Assay types this search can serve, for a caller that wants to build a filter.
feature_assay_types <- function() base::names(FEATURE_SOURCES)

# Search the reference feature catalog for one assay type.
#
# `q` is matched as a SUBSTRING against every text column the assay has, so a
# person can find a feature by either its identifier or its symbol without
# knowing which they hold. Returns list(rows, total, columns) -- `columns` so the
# caller can render exactly the fields this assay type has rather than guessing.
search_features <- function(conn, assay_type, q = NULL, organism = NULL,
                            limit = 25, offset = 0) {
  assay_type <- base::trimws(base::tolower(base::as.character(assay_type %||% "")[1]))
  source <- FEATURE_SOURCES[[assay_type]]
  if (base::is.null(source)) {
    base::stop(base::sprintf(
      "Unsupported assay_type '%s'. Use one of: %s.",
      assay_type, base::paste(feature_assay_types(), collapse = ", ")
    ))
  }

  limit <- base::suppressWarnings(base::as.integer(limit[1]))
  if (base::is.na(limit) || limit < 1) limit <- 25
  limit <- base::min(limit, 100)
  offset <- base::suppressWarnings(base::as.integer(offset[1]))
  if (base::is.na(offset) || offset < 0) offset <- 0

  # is_current = 1 throughout: the feature tables are versioned, and a browse
  # view that showed superseded rows would double-count every re-annotated gene.
  from_where <- base::sprintf("
    FROM %s f
    LEFT JOIN organisms o ON f.organism_id = o.organism_id
    WHERE f.is_current = 1
  ", source$table)

  if (!base::is.null(organism) && base::nzchar(base::trimws(base::as.character(organism)[1]))) {
    from_where <- base::paste(from_where, "AND o.organism =",
                              DBI::dbQuoteLiteral(conn, base::trimws(base::as.character(organism)[1])))
  }

  query_text <- base::trimws(base::as.character(q %||% "")[1])
  if (base::nzchar(query_text)) {
    like <- DBI::dbQuoteLiteral(conn, base::sprintf("%%%s%%", query_text))
    # Every text column this assay has, so "TP53" finds the row whether the
    # caller is thinking in symbols or in Ensembl ids.
    text_cols <- base::intersect(source$columns,
                                 base::c("feature_name", "gene_symbol", "annotation", "chromosome"))
    clauses <- base::paste(base::sprintf("f.%s LIKE %s", text_cols, like), collapse = " OR ")
    from_where <- base::paste(from_where, base::sprintf("AND (%s)", clauses))
  }

  total <- DBI::dbGetQuery(conn, base::paste("SELECT COUNT(*) AS n", from_where))$n[1]

  select_list <- base::paste(base::sprintf("f.%s", source$columns), collapse = ", ")
  rows <- DBI::dbGetQuery(conn, base::paste(
    "SELECT", select_list, ", o.organism, f.version",
    from_where,
    "ORDER BY f.feature_name ASC LIMIT", limit, "OFFSET", offset
  ))

  base::list(rows = rows, total = total, columns = base::c(source$columns, "organism", "version"))
}
