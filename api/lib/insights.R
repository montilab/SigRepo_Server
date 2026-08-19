# Repository-wide summary stats backing the merged Dashboard/Insights page
# (GET /insights, api.R): totals, signature counts grouped by organism/
# assay/contributor, and the most recently created signatures. Not exposed
# as an MCP tool -- nothing there needs a dashboard summary today.
repository_insights <- function(conn, is_admin = FALSE, recent_limit = 5) {
  visibility_clause <- if (is_admin) "" else "WHERE s.visibility = 1"

  totals <- DBI::dbGetQuery(conn, base::sprintf("
    SELECT
      COUNT(*) AS total_signatures,
      COUNT(DISTINCT s.user_name) AS total_users,
      COUNT(DISTINCT s.organism_id) AS total_organisms,
      COUNT(DISTINCT s.assay_type) AS total_assays
    FROM signatures s
    %s
  ", visibility_clause))

  by_organism <- DBI::dbGetQuery(conn, base::sprintf("
    SELECT o.organism AS name, COUNT(*) AS value
    FROM signatures s
    LEFT JOIN organisms o ON s.organism_id = o.organism_id
    %s
    GROUP BY o.organism
    ORDER BY value DESC
  ", visibility_clause))

  by_assay <- DBI::dbGetQuery(conn, base::sprintf("
    SELECT s.assay_type AS name, COUNT(*) AS value
    FROM signatures s
    %s
    GROUP BY s.assay_type
    ORDER BY value DESC
  ", visibility_clause))

  top_contributors <- DBI::dbGetQuery(conn, base::sprintf("
    SELECT s.user_name AS name, COUNT(*) AS value
    FROM signatures s
    %s
    GROUP BY s.user_name
    ORDER BY value DESC
    LIMIT 5
  ", visibility_clause))

  recent_limit <- base::suppressWarnings(base::as.integer(recent_limit[1]))
  if (base::is.na(recent_limit) || recent_limit < 1) {
    recent_limit <- 5
  }

  recent_signatures <- DBI::dbGetQuery(conn, base::sprintf("
    SELECT s.signature_hashkey, s.signature_name, s.assay_type, s.user_name, s.date_created
    FROM signatures s
    %s
    ORDER BY s.date_created DESC
    LIMIT %d
  ", visibility_clause, recent_limit))

  base::list(
    total_signatures = totals$total_signatures[1],
    total_users = totals$total_users[1],
    total_organisms = totals$total_organisms[1],
    total_assays = totals$total_assays[1],
    by_organism = by_organism,
    by_assay = by_assay,
    top_contributors = top_contributors,
    recent_signatures = recent_signatures
  )
}
