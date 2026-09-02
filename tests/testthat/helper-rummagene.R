# Shared by test-rummagene-catalog.R and test-rummagene-catalog-build.R.
#
# A connection to the local dev database, and a way to put known rows into
# transcriptomics_features without disturbing what is already there. Only the
# rows seed_features() actually creates -- as opposed to ones already present,
# e.g. from a real basket import of reference data -- are reported back so the
# caller's on.exit can remove exactly those again via unseed_features(). That
# is what lets these tests run against the real local dev database without
# depending on, or leaving a permanent mark on, whatever is already loaded
# there.
test_conn <- function() {
  testthat::skip_if_not(base::nzchar(base::Sys.getenv("DB_NAME")), "no database configured")
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = base::Sys.getenv("DB_HOST"), port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), password = base::Sys.getenv("DB_PASSWORD"),
    dbname = base::Sys.getenv("DB_NAME")
  )
}

# INSERT IGNOREs one row per feature_name for organism_id (matching the
# feature_name/organism_id unique key transcriptomics_features already
# enforces), and returns -- invisibly -- the feature_hashkeys that did not
# already exist before this call. Those are the rows this call is actually
# responsible for; anything that was already present is left untouched and
# never reported for deletion.
#
# Pass the return value to unseed_features() in the caller's on.exit.
seed_features <- function(conn, organism_id, feature_names) {
  feature_names <- base::unique(base::as.character(feature_names))
  hashkeys <- base::vapply(
    feature_names, function(fn) collection_hash(fn, organism_id), base::character(1)
  )
  pre_existing <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT feature_hashkey FROM transcriptomics_features WHERE feature_hashkey IN (%s)",
    base::paste(DBI::dbQuoteLiteral(conn, base::unname(hashkeys)), collapse = ",")
  ))$feature_hashkey

  for (i in base::seq_along(feature_names)) {
    DBI::dbExecute(conn, base::sprintf(
      "INSERT IGNORE INTO transcriptomics_features
         (feature_name, organism_id, gene_symbol, is_current, version, feature_hashkey)
       VALUES (%s, %d, NULL, 1, 1, %s)",
      DBI::dbQuoteLiteral(conn, feature_names[i]), organism_id,
      DBI::dbQuoteLiteral(conn, hashkeys[i])
    ))
  }
  base::invisible(base::unname(hashkeys[!(hashkeys %in% pre_existing)]))
}

# Deletes exactly the feature_hashkeys seed_features() reported as newly
# created -- never a broad DELETE or TRUNCATE, and never a row this test run
# did not itself add (e.g. a real row from a basket import that happened to
# match one of the fixture symbols).
unseed_features <- function(conn, hashkeys) {
  if (base::length(hashkeys) == 0) {
    return(base::invisible(NULL))
  }
  DBI::dbExecute(conn, base::sprintf(
    "DELETE FROM transcriptomics_features WHERE feature_hashkey IN (%s)",
    base::paste(DBI::dbQuoteLiteral(conn, hashkeys), collapse = ",")
  ))
}

# Temporarily remove one reference feature so a test can exercise the
# "feature_absent" branch, restoring it afterwards.
#
# WHY THIS IS NEEDED: feature_absent is a property of how COMPLETE this
# database's reference table is, not of the gene set. These tests originally
# relied on the local transcriptomics_features being nearly empty, so naming
# any real gene made it "absent". Once the table was populated with a real
# Ensembl build (86,411 human rows on 2026-09-01, matching what production
# holds), every mappable symbol resolved and the branch became unreachable --
# the tests broke without any code changing. Absence has to be constructed by
# the test rather than inherited from an incomplete fixture.
#
# Returns a zero-argument restore function; call it in on.exit. Restores the
# exact row, so a test cannot leave the reference table short a gene.
suppress_feature <- function(conn, feature_name, organism_id) {
  hk <- collection_hash(feature_name, organism_id)
  row <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT feature_name, organism_id, gene_symbol, is_current, version, feature_hashkey
     FROM transcriptomics_features WHERE feature_hashkey = %s",
    DBI::dbQuoteLiteral(conn, hk)
  ))
  if (base::nrow(row) == 0) {
    return(function() invisible(NULL))   # already absent; nothing to restore
  }

  DBI::dbExecute(conn, base::sprintf(
    "DELETE FROM transcriptomics_features WHERE feature_hashkey = %s",
    DBI::dbQuoteLiteral(conn, hk)
  ))

  function() {
    DBI::dbExecute(conn, base::sprintf(
      "INSERT IGNORE INTO transcriptomics_features
         (feature_name, organism_id, gene_symbol, is_current, version, feature_hashkey)
       VALUES (%s, %d, %s, %s, %s, %s)",
      DBI::dbQuoteLiteral(conn, row$feature_name[1]),
      base::as.integer(row$organism_id[1]),
      if (base::is.na(row$gene_symbol[1])) "NULL" else DBI::dbQuoteLiteral(conn, row$gene_symbol[1]),
      base::as.integer(row$is_current[1]),
      DBI::dbQuoteLiteral(conn, base::as.character(row$version[1])),
      DBI::dbQuoteLiteral(conn, row$feature_hashkey[1])
    ))
    invisible(NULL)
  }
}
