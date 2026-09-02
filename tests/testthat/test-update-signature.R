# Editing a signature's metadata in place.
#
# Deliberately NOT SigRepo::updateSignature(): that function authorises through
# SigRepo::checkPermissions(), which identifies the caller as
# DBI::dbGetInfo(conn)$user -- the DATABASE CONNECTION's login. This API
# connects as a single privileged account, so every api_key holder would be
# authorised as that account and could edit anyone's signature. Same reasoning
# as api/lib/create_signature.R's header. Authorisation here is resolved
# against the api_key caller via user_has_owner_or_editor_access().
#
# It is also a metadata PATCH, not a replace: updateSignature() takes a whole
# OmicSignature and rebuilds the feature set from it, which cannot fix a typo
# in a description without regenerating the object.
source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/update_signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

# signatures.user_name is a foreign key to users, so a throwaway owner has to
# exist before a throwaway signature can. Mirrors the pattern in
# test-collection.R: a fabricated account, removed again in on.exit. No real
# credential is read or written -- the api_key here is a literal, and nothing
# authenticates with it.
seed_user <- function(conn, user_name, user_role = "editor") {
  # Upsert rather than delete-then-insert. A DELETE fails outright if any
  # signature still references this user -- which is exactly the state an
  # earlier failed run leaves behind, so delete-first turns one broken run into
  # every subsequent run being broken too.
  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO users (user_name, user_password_hashkey, user_email, user_role, api_key, user_hashkey, active)
     VALUES (%s, 'x', %s, %s, %s, %s, 1)
     ON DUPLICATE KEY UPDATE user_role = VALUES(user_role), active = 1",
    DBI::dbQuoteLiteral(conn, user_name),
    # users has a CHECK on user_email requiring a 2-4 character TLD, so
    # .invalid (7) is rejected. .test is a reserved TLD and fits.
    DBI::dbQuoteLiteral(conn, base::paste0(user_name, "@example.test")),
    DBI::dbQuoteLiteral(conn, user_role),
    DBI::dbQuoteLiteral(conn, base::paste0(user_name, "_not_a_real_key")),
    DBI::dbQuoteLiteral(conn, base::paste0("hk_", user_name))
  ))
  invisible(user_name)
}

remove_user <- function(conn, user_name) {
  # Tolerate a user still referenced by a signature: the caller's on.exit
  # removes the signature first, but if that failed we would rather leave the
  # account than abort cleanup and lose the rest of it.
  base::try(
    DBI::dbExecute(conn, base::sprintf("DELETE FROM users WHERE user_name = %s",
                                       DBI::dbQuoteLiteral(conn, user_name))),
    silent = TRUE
  )
  invisible(NULL)
}

# A signature owned by `owner`, plus the access grant the real upload writes.
# Returns its hashkey; the caller removes it in on.exit.
seed_signature <- function(conn, owner, name, visibility = 0L) {
  seed_user(conn, owner)
  hk <- collection_hash(name, owner)
  # Seed idempotently. on.exit cleanup is registered by the CALLER, after this
  # function returns -- so if this throws part-way, nothing is registered and
  # the partial row survives, and UNIQUE(signature_name, user_name) then breaks
  # every subsequent run. Clearing first makes one bad run a one-run problem.
  remove_signature(conn, hk)
  organism_id <- lookup_id(conn, "organisms", "organism_id", "organism", "Homo sapiens")
  phenotype_id <- lookup_id(conn, "phenotypes", "phenotype_id", "phenotype", "unknown")
  if (base::is.null(phenotype_id)) {
    DBI::dbExecute(conn, "INSERT INTO phenotypes (phenotype) VALUES ('unknown')")
    phenotype_id <- lookup_id(conn, "phenotypes", "phenotype_id", "phenotype", "unknown")
  }
  sample_type_id <- lookup_id(conn, "sample_types", "sample_type_id", "sample_type", "unknown")
  platform_id <- DBI::dbGetQuery(conn, "SELECT platform_id FROM platforms LIMIT 1")$platform_id[1]

  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO signatures
       (signature_name, organism_id, direction_type, assay_type, phenotype_id,
        platform_id, sample_type_id, description, user_name, visibility, signature_hashkey)
     VALUES (%s, %d, 'uni-directional', 'transcriptomics', %d, %d, %d, 'before', %s, %d, %s)",
    DBI::dbQuoteLiteral(conn, name), organism_id, phenotype_id, platform_id,
    sample_type_id, DBI::dbQuoteLiteral(conn, owner), base::as.integer(visibility),
    DBI::dbQuoteLiteral(conn, hk)
  ))
  sid <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hk)
  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO signature_access (signature_id, user_name, access_type, access_sig_hashkey)
     VALUES (%d, %s, 'owner', %s)",
    base::as.integer(sid), DBI::dbQuoteLiteral(conn, owner),
    DBI::dbQuoteLiteral(conn, collection_hash(hk, owner))
  ))
  hk
}

remove_signature <- function(conn, hashkey) {
  sid <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hashkey)
  if (base::is.null(sid)) return(invisible(NULL))
  DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", base::as.integer(sid)))
  DBI::dbExecute(conn, base::sprintf("DELETE FROM signature_access WHERE signature_id = %d", base::as.integer(sid)))
  DBI::dbExecute(conn, base::sprintf("DELETE FROM signatures WHERE signature_id = %d", base::as.integer(sid)))
  invisible(NULL)
}

update_conn <- function() {
  testthat::skip_if_not(base::nzchar(base::Sys.getenv("DB_NAME")), "no database configured")
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = base::Sys.getenv("DB_HOST"), port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), password = base::Sys.getenv("DB_PASSWORD"),
    dbname = base::Sys.getenv("DB_NAME")
  )
}

test_that("the owner can update their own signature's description", {
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_desc")
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)

  out <- update_signature_metadata(
    conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
    signature_hashkey = hk, fields = base::list(description = "after")
  )

  expect_true(out$ok)
  got <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT description FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))
  expect_equal(got$description[1], "after")
})

test_that("a user who neither owns the signature nor is admin is refused", {
  # The whole reason this does not delegate to SigRepo::updateSignature: its
  # checkPermissions() would see the DB connection's login, not this caller.
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_forbidden")
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)

  out <- update_signature_metadata(
    conn, auth = base::list(user_name = "someone_else", user_role = "editor"),
    signature_hashkey = hk, fields = base::list(description = "should not land")
  )

  expect_false(out$ok)
  expect_equal(out$reason, "forbidden")
  got <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT description FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))
  expect_equal(got$description[1], "before")
})

test_that("an admin can update a signature they do not own", {
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_admin")
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)

  out <- update_signature_metadata(
    conn, auth = base::list(user_name = "some_admin", user_role = "admin"),
    signature_hashkey = hk, fields = base::list(description = "by admin")
  )

  expect_true(out$ok)
  got <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT description FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))
  expect_equal(got$description[1], "by admin")
})

test_that("visibility round-trips, which is what makes a signature public", {
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_visibility", visibility = 0L)
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)

  update_signature_metadata(conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
                            signature_hashkey = hk, fields = base::list(visibility = TRUE))
  expect_equal(DBI::dbGetQuery(conn, base::sprintf(
    "SELECT visibility FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))$visibility[1], 1)

  update_signature_metadata(conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
                            signature_hashkey = hk, fields = base::list(visibility = FALSE))
  expect_equal(DBI::dbGetQuery(conn, base::sprintf(
    "SELECT visibility FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))$visibility[1], 0)
})

test_that("a field outside the whitelist is ignored rather than written", {
  # These values are interpolated into an UPDATE. An unrecognised key must not
  # reach the SQL at all -- not merely be quoted -- or a caller chooses which
  # column to write.
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_whitelist")
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)

  out <- update_signature_metadata(
    conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
    signature_hashkey = hk,
    fields = base::list(description = "kept", user_name = "hijacked", signature_hashkey = "x")
  )

  expect_true(out$ok)
  got <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT description, user_name, signature_hashkey FROM signatures WHERE signature_hashkey = %s",
    DBI::dbQuoteLiteral(conn, hk)))
  expect_equal(got$description[1], "kept")
  expect_equal(got$user_name[1], "upd_owner")     # ownership cannot be reassigned
  expect_equal(got$signature_hashkey[1], hk)      # nor the identity key rewritten
})

test_that("an omitted field is left alone rather than blanked", {
  # The UI sends a partial patch; a field it does not include must survive.
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_partial")
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)

  update_signature_metadata(conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
                            signature_hashkey = hk, fields = base::list(year = 2024L))

  got <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT description, year FROM signatures WHERE signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))
  expect_equal(got$description[1], "before")
  expect_equal(got$year[1], 2024)
})

test_that("phenotype resolves through the vocabulary and creates the row when new", {
  # phenotype is an FK, not a free string. Upload creates a missing phenotype
  # (create_signature.R:422) rather than refusing, and editing must behave the
  # same or a correction could not introduce a phenotype the repo lacks.
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_phenotype")
  novel <- base::paste0("upd_pheno_", base::as.integer(base::Sys.time()))
  on.exit({
    remove_signature(conn, hk)
    remove_user(conn, "upd_owner")
    DBI::dbExecute(conn, base::sprintf("DELETE FROM phenotypes WHERE phenotype = %s",
                                       DBI::dbQuoteLiteral(conn, novel)))
    DBI::dbDisconnect(conn)
  }, add = TRUE)

  out <- update_signature_metadata(
    conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
    signature_hashkey = hk, fields = base::list(phenotype = novel)
  )

  expect_true(out$ok)
  got <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT p.phenotype FROM signatures s JOIN phenotypes p USING (phenotype_id)
     WHERE s.signature_hashkey = %s", DBI::dbQuoteLiteral(conn, hk)))
  expect_equal(got$phenotype[1], novel)
})

test_that("an unknown signature is reported as not found, not as forbidden", {
  # Distinguishable failures: "no such signature" and "not yours" are different
  # answers and map to different statuses.
  conn <- update_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)

  out <- update_signature_metadata(
    conn, auth = base::list(user_name = "upd_owner", user_role = "admin"),
    signature_hashkey = "no_such_hashkey_000000000000000", fields = base::list(description = "x")
  )

  expect_false(out$ok)
  expect_equal(out$reason, "not_found")
})

test_that("updating metadata does not disturb the feature set", {
  conn <- update_conn()
  hk <- seed_signature(conn, "upd_owner", "upd_test_features")
  on.exit({ remove_signature(conn, hk); remove_user(conn, "upd_owner"); DBI::dbDisconnect(conn) }, add = TRUE)
  sid <- lookup_id(conn, "signatures", "signature_id", "signature_hashkey", hk)
  DBI::dbExecute(conn, base::sprintf(
    "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, group_label, assay_type, sig_feature_hashkey)
     SELECT %d, feature_id, 'p1', 'All Features', 'transcriptomics', %s
     FROM transcriptomics_features LIMIT 1",
    base::as.integer(sid), DBI::dbQuoteLiteral(conn, collection_hash(hk, "p1"))))
  before <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) n FROM signature_feature_set WHERE signature_id = %d", base::as.integer(sid)))$n

  update_signature_metadata(conn, auth = base::list(user_name = "upd_owner", user_role = "editor"),
                            signature_hashkey = hk, fields = base::list(description = "metadata only"))

  after <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) n FROM signature_feature_set WHERE signature_id = %d", base::as.integer(sid)))$n
  expect_equal(after, before)
})
