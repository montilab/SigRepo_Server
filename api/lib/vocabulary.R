# Distinct vocabulary values in use by signatures, for populating the UI's
# search/filter dropdowns. Mirrors the query logic behind the MCP
# `list_vocabulary` tool so the REST and MCP surfaces stay consistent.
# Depends on api/lib/common.R (db_connect_local).

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
