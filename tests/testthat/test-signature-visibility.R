source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

# Who can see a signature in search.
#
# THE BUG THESE WERE WRITTEN FOR: search_signatures() filtered non-admins to
# `s.visibility = 1` and nothing else, so a person could not find their OWN
# private signatures. Reported as "I added signatures to a collection but they
# do not appear on the Signatures tab" -- the collection view was right all
# along; the Signatures tab was hiding them.
#
# It looked like a collections bug and was not. The R client showed the same
# signatures fine, which made it look like a web-only display fault; in fact
# SigRepo::searchSignature() applies NO visibility filter at all and connects as
# a privileged database user, so it was never subject to this rule.
#
# search_collections() already had the correct three-part rule (public, OR mine,
# OR granted to me). These assert search_signatures() matches it.

vis_conn <- function() {
  testthat::skip_if_not(base::nzchar(base::Sys.getenv("DB_NAME")), "no database configured")
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = base::Sys.getenv("DB_HOST"), port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), password = base::Sys.getenv("DB_PASSWORD"),
    dbname = base::Sys.getenv("DB_NAME")
  )
}

vis_seed_user <- function(conn, user_name) {
  # Only the NOT NULL columns without defaults, so this does not break the next
  # time an optional column is added. api_key here is a throwaway fixture value,
  # never a real credential.
  DBI::dbExecute(conn, base::sprintf(
    "INSERT IGNORE INTO users
       (user_name, user_password_hashkey, user_email, user_role, api_key, user_hashkey)
     VALUES (%s, %s, %s, 'editor', %s, %s)",
    DBI::dbQuoteLiteral(conn, user_name),
    DBI::dbQuoteLiteral(conn, base::sprintf("hash_%s", user_name)),
    DBI::dbQuoteLiteral(conn, base::sprintf("%s@example.org", user_name)),
    DBI::dbQuoteLiteral(conn, base::sprintf("fixturekey_%s", user_name)),
    DBI::dbQuoteLiteral(conn, collection_hash(user_name, "vis_fixture"))
  ))
}

# A signature owned by `owner`, with the owner grant the real upload writes.
vis_seed_signature <- function(conn, owner, name, visibility) {
  vis_seed_user(conn, owner)
  hk <- collection_hash(name, owner)
  vis_remove_signature(conn, hk)
  # Whatever vocabulary this database happens to hold, creating a row only if a
  # table is empty. These tests are about VISIBILITY and do not care which
  # organism or platform a signature carries.
  #
  # They previously asked for "Homo sapiens" and "unknown" by name. That passes
  # against a development database and fails in CI, whose seed fixture uses
  # "CI Test Organism" -- lookup_id() returned NULL, sprintf("%d", NULL) produced
  # no output, and the resulting malformed SQL surfaced as the opaque
  # "CHAR() can only be applied to a 'CHARSXP', not a 'NULL'".
  vocab_id <- function(table, id_col, name_col, fallback) {
    row <- DBI::dbGetQuery(conn, base::sprintf("SELECT %s FROM %s LIMIT 1", id_col, table))
    if (base::nrow(row) > 0) {
      return(base::as.integer(row[[id_col]][1]))
    }
    DBI::dbExecute(conn, base::sprintf("INSERT INTO %s (%s) VALUES (%s)",
                                       table, name_col, DBI::dbQuoteLiteral(conn, fallback)))
    base::as.integer(DBI::dbGetQuery(conn, base::sprintf(
      "SELECT %s FROM %s ORDER BY %s DESC LIMIT 1", id_col, table, id_col))[[id_col]][1])
  }
  organism_id <- vocab_id("organisms", "organism_id", "organism", "vis test organism")
  phenotype_id <- vocab_id("phenotypes", "phenotype_id", "phenotype", "unknown")
  sample_type_id <- vocab_id("sample_types", "sample_type_id", "sample_type", "unknown")
  platform_id <- vocab_id("platforms", "platform_id", "platform_name", "vis test platform")

  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO signatures
       (signature_name, organism_id, direction_type, assay_type, phenotype_id,
        platform_id, sample_type_id, description, user_name, visibility, signature_hashkey)
     VALUES (%s, %d, 'uni-directional', 'transcriptomics', %d, %d, %d, 'vis', %s, %d, %s)",
    DBI::dbQuoteLiteral(conn, name), organism_id, phenotype_id, platform_id,
    sample_type_id, DBI::dbQuoteLiteral(conn, owner), base::as.integer(visibility),
    DBI::dbQuoteLiteral(conn, hk)))
  sid <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hk)
  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO signature_access (signature_id, user_name, access_type, access_sig_hashkey)
     VALUES (%d, %s, 'owner', %s)",
    base::as.integer(sid), DBI::dbQuoteLiteral(conn, owner),
    DBI::dbQuoteLiteral(conn, collection_hash(hk, owner))))
  hk
}

vis_grant <- function(conn, hashkey, user_name, access_type = "viewer") {
  vis_seed_user(conn, user_name)
  sid <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hashkey)
  DBI::dbExecute(conn, base::sprintf(
    "INSERT IGNORE INTO signature_access (signature_id, user_name, access_type, access_sig_hashkey)
     VALUES (%d, %s, %s, %s)",
    base::as.integer(sid), DBI::dbQuoteLiteral(conn, user_name),
    DBI::dbQuoteLiteral(conn, access_type),
    DBI::dbQuoteLiteral(conn, collection_hash(hashkey, user_name))))
}

vis_remove_signature <- function(conn, hashkey) {
  sid <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hashkey)
  if (base::is.null(sid)) return(base::invisible(NULL))
  for (stmt in base::c("DELETE FROM signature_feature_set WHERE signature_id = %d",
                       "DELETE FROM signature_access WHERE signature_id = %d",
                       "DELETE FROM signature_collection_access WHERE signature_id = %d",
                       "DELETE FROM signatures WHERE signature_id = %d")) {
    DBI::dbExecute(conn, base::sprintf(stmt, base::as.integer(sid)))
  }
  base::invisible(NULL)
}

vis_hashkeys <- function(result) result$rows$signature_hashkey

test_that("a person finds their own private signature", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_private_sig", visibility = 0L)
  auth <- base::list(user_name = "vis_owner", user_role = "editor")

  # The reported bug in one line. Without the owner clause this returns FALSE,
  # and the signature is unreachable from the Signatures tab by the only person
  # guaranteed to be allowed to see it.
  res <- search_signatures(conn, auth = auth, limit = 200, is_admin = FALSE)
  testthat::expect_true(hk %in% vis_hashkeys(res))
})

test_that("someone else's private signature stays hidden", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_private_sig2", visibility = 0L)
  stranger <- base::list(user_name = "vis_stranger", user_role = "editor")

  # The fix must widen visibility for the OWNER without widening it for
  # everyone -- otherwise it trades an annoyance for a data leak.
  res <- search_signatures(conn, auth = stranger, limit = 200, is_admin = FALSE)
  testthat::expect_false(hk %in% vis_hashkeys(res))
})

test_that("an explicit grant makes a private signature findable", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_shared_sig", visibility = 0L)
  vis_grant(conn, hk, "vis_friend", "viewer")
  friend <- base::list(user_name = "vis_friend", user_role = "editor")

  # Selective sharing is meaningless if the shared-with person cannot find the
  # signature. This is the same rule search_collections() already applies.
  res <- search_signatures(conn, auth = friend, limit = 200, is_admin = FALSE)
  testthat::expect_true(hk %in% vis_hashkeys(res))
})

test_that("public signatures stay visible to everyone", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_public_sig", visibility = 1L)
  stranger <- base::list(user_name = "vis_stranger", user_role = "editor")
  res <- search_signatures(conn, auth = stranger, limit = 200, is_admin = FALSE)
  testthat::expect_true(hk %in% vis_hashkeys(res))
})

test_that("an admin still sees everything", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_admin_sees", visibility = 0L)
  admin <- base::list(user_name = "vis_stranger", user_role = "admin")
  res <- search_signatures(conn, auth = admin, limit = 200, is_admin = TRUE)
  testthat::expect_true(hk %in% vis_hashkeys(res))
})

test_that("the total matches the rows a caller may actually see", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_count_sig", visibility = 0L)
  owner <- base::list(user_name = "vis_owner", user_role = "editor")
  stranger <- base::list(user_name = "vis_stranger", user_role = "editor")

  as_owner <- search_signatures(conn, auth = owner, limit = 200, is_admin = FALSE)
  as_stranger <- search_signatures(conn, auth = stranger, limit = 200, is_admin = FALSE)

  # COUNT(*) and the page share one FROM/WHERE, so a visibility rule applied to
  # only one of them would give a paginated view that offers empty pages.
  testthat::expect_equal(base::as.integer(as_owner$total), base::nrow(as_owner$rows))
  testthat::expect_equal(base::as.integer(as_stranger$total), base::nrow(as_stranger$rows))
  testthat::expect_gt(base::as.integer(as_owner$total), base::as.integer(as_stranger$total))
})

test_that("a caller with no auth sees only public signatures", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_noauth_sig", visibility = 0L)
  # auth is optional for backwards compatibility with existing callers. Missing
  # auth must fall back to public-only rather than to everything -- a NULL
  # user_name interpolated into the SQL would otherwise match nothing or, worse,
  # break the clause open.
  res <- search_signatures(conn, limit = 200, is_admin = FALSE)
  testthat::expect_false(hk %in% vis_hashkeys(res))
})

test_that("gene search applies the same rule", {
  conn <- vis_conn()
  hk <- NULL
  base::on.exit({
    if (!base::is.null(hk)) vis_remove_signature(conn, hk)
    base::suppressWarnings(DBI::dbDisconnect(conn))
  }, add = TRUE)

  hk <- vis_seed_signature(conn, "vis_owner", "vis_gene_sig", visibility = 0L)
  owner <- base::list(user_name = "vis_owner", user_role = "editor")
  stranger <- base::list(user_name = "vis_stranger", user_role = "editor")

  # search_signatures_by_genes() carried its own copy of the visibility rule and
  # the same defect. It backs gene search and the related-signatures panel, so a
  # fix to search_signatures() alone would have left half the app still hiding a
  # person's own work. Both now share signature_visibility_clause().
  #
  # The seeded signature has no features, so neither call returns it -- what is
  # asserted here is that the function ACCEPTS auth and still filters, not that
  # it matches. The clause itself is covered by the tests above.
  testthat::expect_no_error(
    search_signatures_by_genes(conn, genes = c("TP53"), limit = 5, is_admin = FALSE, auth = owner))
  testthat::expect_no_error(
    search_signatures_by_genes(conn, genes = c("TP53"), limit = 5, is_admin = FALSE, auth = stranger))
  testthat::expect_no_error(
    search_signatures_by_genes(conn, genes = c("TP53"), limit = 5, is_admin = FALSE))
})

test_that("the visibility rule lives in one place", {
  src <- base::paste(base::readLines(
    testthat::test_path("../../api/lib/signature.R"), warn = FALSE), collapse = "\n")
  # Three call sites drifting apart is how this bug survived. Every signature
  # query must go through the shared helper rather than inlining the filter.
  testthat::expect_match(src, "signature_visibility_clause <- function", fixed = TRUE)
  inlined <- base::length(base::grep(
    'AND s\\.visibility = 1"', base::strsplit(src, "\n")[[1]]))
  testthat::expect_equal(inlined, 0)
})
