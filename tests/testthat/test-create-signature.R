source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/difexp.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

# The real `OmicSignature` package (montilab/OmicSignature, installed per
# .github/workflows/test.yml) is currently R6 -- NOT the unmerged S4
# migration some in-repo branches carry -- so this is what an actual
# uploaded .rds file (built by a real SigRepo user, e.g. via Shiny's
# "Upload Signature") looks like on the wire today.
omic_signature_available <- requireNamespace("OmicSignature", quietly = TRUE)

local_tempdir <- function() {
  dir <- tempfile("sigrepo-create-signature-test-")
  dir.create(dir)
  dir
}

# A real OmicSignature R6 object -- the shape build_signature_from_upload()
# needs to support since that's what Shiny's "Upload Signature" feature (and
# any real SigRepo user) actually produces. feature_name values must match
# ones seeded into transcriptomics_features by the tests below.
omic_signature_upload <- function(signature_name, feature_names = c("CI_UPLOAD_TEST_GENE_1", "CI_UPLOAD_TEST_GENE_2"),
                                   scores = c(2.5, -1.2), difexp = NULL) {
  suppressWarnings(suppressMessages(
    OmicSignature::OmicSignature$new(
      metadata = list(
        signature_name = signature_name,
        direction_type = "bi-directional",
        assay_type = "transcriptomics",
        organism = "CI Test Organism",
        phenotype = "CI Test Phenotype",
        sample_type = "CI Test Sample Type",
        platform = "CI Test Platform",
        description = "A test signature",
        keywords = "test,upload"
      ),
      signature = data.frame(
        probe_id = paste0("probe_", seq_along(feature_names)),
        feature_name = feature_names,
        score = scores,
        group_label = factor(rep("All Features", length(feature_names))),
        stringsAsFactors = FALSE
      ),
      difexp = difexp
    )
  ))
}

# This API's own /signatures/export shape (list(metadata, signature,
# difexp)), whose feature rows already carry a resolved feature_id --
# Export -> Upload must keep round-tripping without a feature_name lookup.
export_shape_upload <- function(signature_name, extra_features = data.frame()) {
  list(
    metadata = list(
      signature_name = signature_name,
      direction_type = "bi-directional",
      assay_type = "transcriptomics",
      organism = "CI Test Organism",
      phenotype = "CI Test Phenotype",
      sample_type = "CI Test Sample Type",
      platform_name = "CI Test Platform",
      description = "A test signature",
      keywords = "test,upload"
    ),
    signature = rbind(
      data.frame(probe_id = "1020", feature_id = 1, score = 2.5, group_label = "All Features", stringsAsFactors = FALSE),
      data.frame(probe_id = "1023", feature_id = 2, score = -1.2, group_label = "All Features", stringsAsFactors = FALSE),
      extra_features
    ),
    difexp = NULL
  )
}

seed_transcriptomics_features <- function(exec_sql) {
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_name IN ('CI_UPLOAD_TEST_GENE_1', 'CI_UPLOAD_TEST_GENE_2')")
  # feature_hashkey computed the same way collection_hash()/
  # SigRepo::createHashKey() do: md5(tolower(paste0(feature_name, organism_id))).
  exec_sql("
    INSERT INTO transcriptomics_features (feature_id, feature_name, organism_id, gene_symbol, version, feature_hashkey)
    SELECT 1, 'ENSG_TEST_1', organism_id, 'TP53', 1, MD5(LOWER(CONCAT('ENSG_TEST_1', organism_id))) FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT 2, 'ENSG_TEST_2', organism_id, 'BRCA1', 1, MD5(LOWER(CONCAT('ENSG_TEST_2', organism_id))) FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT NULL, 'CI_UPLOAD_TEST_GENE_1', organism_id, 'TP53', 1, MD5(LOWER(CONCAT('CI_UPLOAD_TEST_GENE_1', organism_id))) FROM organisms WHERE organism = 'CI Test Organism'
    UNION ALL
    SELECT NULL, 'CI_UPLOAD_TEST_GENE_2', organism_id, 'BRCA1', 1, MD5(LOWER(CONCAT('CI_UPLOAD_TEST_GENE_2', organism_id))) FROM organisms WHERE organism = 'CI Test Organism'
  ")
}

cleanup_transcriptomics_features <- function(exec_sql) {
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_id IN (1, 2)")
  exec_sql("DELETE FROM transcriptomics_features WHERE feature_name IN ('CI_UPLOAD_TEST_GENE_1', 'CI_UPLOAD_TEST_GENE_2')")
}

test_that("normalize_upload recognizes a real OmicSignature object vs. the export-list shape vs. neither", {
  skip_if_not(omic_signature_available, "OmicSignature package not installed")

  omic_norm <- normalize_upload(omic_signature_upload("Norm Test"))
  expect_true(omic_norm$ok)
  expect_equal(omic_norm$feature_key, "feature_name")
  expect_equal(omic_norm$platform_field, "platform")

  export_norm <- normalize_upload(export_shape_upload("Norm Test"))
  expect_true(export_norm$ok)
  expect_equal(export_norm$feature_key, "feature_id")
  expect_equal(export_norm$platform_field, "platform_name")

  bad_norm <- normalize_upload(list(foo = "bar"))
  expect_false(bad_norm$ok)
  expect_equal(bad_norm$reason, "invalid_upload")
})

test_that("validate_upload_shape rejects missing metadata fields and missing feature columns", {
  bad_meta <- normalize_upload(list(metadata = list(signature_name = "x"), signature = data.frame(probe_id = 1, feature_id = 1)))
  expect_null(bad_meta$reason)
  err <- validate_upload_shape(bad_meta)
  expect_false(is.null(err))
  expect_equal(err$reason, "invalid_upload")
  expect_match(err$message, "direction_type")

  bad_features <- normalize_upload(list(
    metadata = as.list(setNames(rep("x", length(REQUIRED_UPLOAD_METADATA_FIELDS)), REQUIRED_UPLOAD_METADATA_FIELDS)),
    signature = data.frame(probe_id = 1)
  ))
  err2 <- validate_upload_shape(bad_features)
  expect_false(is.null(err2))
  expect_equal(err2$reason, "invalid_upload")

  expect_null(validate_upload_shape(normalize_upload(export_shape_upload("ok"))))
})

test_that("build_signature_from_upload rejects viewers and unsupported assay types", {
  skip_if_no_test_db()
  viewer_auth <- list(user_name = "ci_viewer", user_role = "viewer")
  editor_auth <- list(user_name = "ci_admin", user_role = "admin")

  forbidden <- build_signature_from_upload(viewer_auth, export_shape_upload("Forbidden Test"), difexp_dir = tempdir())
  expect_false(forbidden$ok)
  expect_equal(forbidden$reason, "forbidden")

  # methylomics, not metabolomics: metabolomics uploads now, and so do
  # genetic_variants. methylomics is the one assay type still unsupported, and
  # SigRepo::addSignature() rejects it too (showAssayTypeErrorMessage).
  upload <- export_shape_upload("Bad Assay Test")
  upload$metadata$assay_type <- "methylomics"
  bad_assay <- build_signature_from_upload(editor_auth, upload, difexp_dir = tempdir())
  expect_false(bad_assay$ok)
  expect_equal(bad_assay$reason, "unsupported_assay_type")
  expect_match(bad_assay$message, "metabolomics", fixed = TRUE)
})

test_that("build_signature_from_upload rejects unknown organism/platform/sample_type and unknown features (export shape)", {
  skip_if_no_test_db()
  auth <- list(user_name = "ci_admin", user_role = "admin")

  bad_org <- export_shape_upload("Bad Org Test")
  bad_org$metadata$organism <- "Not A Real Organism"
  result <- build_signature_from_upload(auth, bad_org, difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "invalid_upload")
  expect_match(result$message, "organism")

  bad_features <- export_shape_upload("Bad Features Test")
  bad_features$signature <- data.frame(probe_id = "999999", feature_id = 999999, score = 1, group_label = "All Features", stringsAsFactors = FALSE)
  result2 <- build_signature_from_upload(auth, bad_features, difexp_dir = tempdir())
  expect_false(result2$ok)
  expect_equal(result2$reason, "unknown_features")
  expect_match(result2$message, "999999")
})

test_that("build_signature_from_upload writes a real signature end-to-end from the export shape and computes up/down counts", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  query_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    DBI::dbGetQuery(conn, stmt)
  }

  seed_transcriptomics_features(exec_sql)
  on.exit(cleanup_transcriptomics_features(exec_sql), add = TRUE)

  auth <- list(user_name = "ci_admin", user_role = "admin")
  upload <- export_shape_upload("Upload Roundtrip Test")
  upload$difexp <- data.frame(probe_id = c("1020", "1023"), score = c(2.5, -1.2), stringsAsFactors = FALSE)

  difexp_dir <- local_tempdir()
  result <- build_signature_from_upload(auth, upload, visibility = TRUE, difexp_dir = difexp_dir)
  expect_true(result$ok)
  expect_equal(result$signature_name, "Upload Roundtrip Test")

  on.exit({
    row <- query_sql(sprintf("SELECT signature_id FROM signatures WHERE signature_hashkey = '%s'", result$signature_hashkey))
    if (nrow(row) > 0) {
      sid <- row$signature_id[1]
      exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", sid))
      exec_sql(sprintf("DELETE FROM signature_access WHERE signature_id = %d", sid))
      exec_sql(sprintf("DELETE FROM signatures WHERE signature_id = %d", sid))
    }
  }, add = TRUE)

  sig_row <- query_sql(sprintf("SELECT * FROM signatures WHERE signature_hashkey = '%s'", result$signature_hashkey))
  expect_equal(nrow(sig_row), 1)
  expect_equal(sig_row$user_name[1], "ci_admin")
  expect_equal(as.integer(sig_row$num_up_regulated[1]), 1)
  expect_equal(as.integer(sig_row$num_down_regulated[1]), 1)
  expect_equal(as.integer(sig_row$has_difexp[1]), 1)

  access_row <- query_sql(sprintf("SELECT * FROM signature_access WHERE signature_id = %d", sig_row$signature_id[1]))
  expect_equal(nrow(access_row), 1)
  expect_equal(access_row$access_type[1], "owner")

  feature_rows <- query_sql(sprintf("SELECT * FROM signature_feature_set WHERE signature_id = %d", sig_row$signature_id[1]))
  expect_equal(nrow(feature_rows), 2)

  loaded_difexp <- load_difexp_rds(difexp_dir, result$signature_hashkey)
  expect_equal(nrow(loaded_difexp), 2)

  # Duplicate name+user is rejected.
  dup <- build_signature_from_upload(auth, export_shape_upload("Upload Roundtrip Test"), difexp_dir = tempdir())
  expect_false(dup$ok)
  expect_equal(dup$reason, "duplicate")
})

test_that("build_signature_from_upload writes a real signature end-to-end from a genuine OmicSignature object", {
  skip_if_no_test_db()
  skip_if_not(omic_signature_available, "OmicSignature package not installed")

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  query_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    DBI::dbGetQuery(conn, stmt)
  }

  seed_transcriptomics_features(exec_sql)
  on.exit(cleanup_transcriptomics_features(exec_sql), add = TRUE)

  auth <- list(user_name = "ci_admin", user_role = "admin")
  upload <- omic_signature_upload("Omic Upload Test")

  result <- build_signature_from_upload(auth, upload, visibility = FALSE, difexp_dir = tempdir())
  expect_true(result$ok)
  expect_equal(result$signature_name, "Omic Upload Test")

  on.exit({
    row <- query_sql(sprintf("SELECT signature_id FROM signatures WHERE signature_hashkey = '%s'", result$signature_hashkey))
    if (nrow(row) > 0) {
      sid <- row$signature_id[1]
      exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id = %d", sid))
      exec_sql(sprintf("DELETE FROM signature_access WHERE signature_id = %d", sid))
      exec_sql(sprintf("DELETE FROM signatures WHERE signature_id = %d", sid))
    }
  }, add = TRUE)

  sig_row <- query_sql(sprintf("SELECT * FROM signatures WHERE signature_hashkey = '%s'", result$signature_hashkey))
  expect_equal(nrow(sig_row), 1)
  expect_equal(sig_row$user_name[1], "ci_admin")
  expect_equal(as.integer(sig_row$num_up_regulated[1]), 1)
  expect_equal(as.integer(sig_row$num_down_regulated[1]), 1)
  expect_equal(as.integer(sig_row$visibility[1]), 0)

  feature_rows <- query_sql(sprintf("SELECT * FROM signature_feature_set WHERE signature_id = %d", sig_row$signature_id[1]))
  expect_equal(nrow(feature_rows), 2)
  expect_true(all(c("probe_1", "probe_2") %in% feature_rows$probe_id))

  # A feature_name that doesn't exist in transcriptomics_features is rejected.
  bad <- omic_signature_upload("Omic Unknown Feature Test", feature_names = c("NOT_A_REAL_GENE", "CI_UPLOAD_TEST_GENE_2"))
  bad_result <- build_signature_from_upload(auth, bad, difexp_dir = tempdir())
  expect_false(bad_result$ok)
  expect_equal(bad_result$reason, "unknown_features")
  expect_match(bad_result$message, "NOT_A_REAL_GENE")
})

test_that("build_signature_from_upload rolls back the signature row if feature insertion fails", {
  skip_if_no_test_db()

  exec_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    suppressWarnings(DBI::dbExecute(conn, stmt))
  }
  query_sql <- function(stmt) {
    conn <- db_connect_local()
    on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)
    DBI::dbGetQuery(conn, stmt)
  }

  seed_transcriptomics_features(exec_sql)
  on.exit(cleanup_transcriptomics_features(exec_sql), add = TRUE)

  auth <- list(user_name = "ci_admin", user_role = "admin")
  # Two rows with the same (group_label, probe_id) violate
  # signature_feature_set's primary key, forcing the insert to fail after
  # the signatures row already exists.
  upload <- export_shape_upload(
    "Rollback Test",
    extra_features = data.frame(probe_id = "1020", feature_id = 1, score = 3.0, group_label = "All Features", stringsAsFactors = FALSE)
  )

  result <- build_signature_from_upload(auth, upload, difexp_dir = tempdir())
  expect_false(result$ok)
  expect_equal(result$reason, "write_failed")

  leftover <- query_sql("SELECT * FROM signatures WHERE signature_name = 'Rollback Test'")
  expect_equal(nrow(leftover), 0)
})

# --- assay types beyond transcriptomics/proteomics ---------------------------
# Upload was silently limited to the two assay types annotate can map to gene
# symbols, because it reused enrichment_reference_table(). The repository holds
# metabolomics and genetic-variants signatures, and the GEM annotate methods
# exist to serve metabolomics, so neither could be uploaded through the API.

test_that("upload_reference_table covers every assay type SigRepo::addSignature dispatches on", {
  expect_equal(upload_reference_table("transcriptomics"), "transcriptomics_features")
  expect_equal(upload_reference_table("proteomics"), "proteomics_features")
  expect_equal(upload_reference_table("metabolomics"), "metabolite_reference")
  expect_equal(upload_reference_table("genetic_variants"), "genetic_variants_features")
  # Unsupported in the client too (showAssayTypeErrorMessage).
  expect_null(upload_reference_table("methylomics"))
  expect_null(upload_reference_table("nonsense"))
})

test_that("upload_reference_id_column knows metabolite_reference's key is not called feature_id", {
  expect_equal(upload_reference_id_column("metabolite_reference"), "metabolite_id")
  expect_equal(upload_reference_id_column("transcriptomics_features"), "feature_id")
  expect_equal(upload_reference_id_column("genetic_variants_features"), "feature_id")
})

test_that("resolve_metabolomics_nomenclature reads either shape and defaults to refmet", {
  # OmicSignature: metadata$others, the same key SigRepo::addSignature takes.
  expect_equal(
    resolve_metabolomics_nomenclature(list(others = list(metabolomics_nomenclature = "hmdb")), data.frame()),
    "hmdb"
  )
  # /signatures/export: recorded per feature instead.
  expect_equal(
    resolve_metabolomics_nomenclature(list(), data.frame(nomenclature_type = c("inchikey", "inchikey"), stringsAsFactors = FALSE)),
    "inchikey"
  )
  # metadata wins when both are present.
  expect_equal(
    resolve_metabolomics_nomenclature(
      list(others = list(metabolomics_nomenclature = "smiles")),
      data.frame(nomenclature_type = "refmet", stringsAsFactors = FALSE)
    ),
    "smiles"
  )
  expect_equal(resolve_metabolomics_nomenclature(list(), data.frame()), "refmet")
  # resolveMetabolomicsFeatureConfig() treats these as one dictionary.
  expect_equal(
    resolve_metabolomics_nomenclature(list(others = list(metabolomics_nomenclature = "refmet_name")), data.frame()),
    "refmet"
  )
})

test_that("upload_others_value records the metabolite dictionary so the signature can be read back", {
  # createOmicSignature() stops without it, so a metabolomics signature stored
  # without one could never be exported, compared, or GEM-enriched again.
  expect_equal(upload_others_value(list(), "refmet"), "metabolomics_nomenclature: refmet")
  expect_equal(
    upload_others_value(list(others = "note: something"), "hmdb"),
    "note: something; metabolomics_nomenclature: hmdb"
  )
  # An OmicSignature holds `others` as a list.
  expect_equal(
    upload_others_value(list(others = list(note = "something")), "refmet"),
    "note: something; metabolomics_nomenclature: refmet"
  )
  # Don't duplicate one the uploader already set.
  expect_equal(
    upload_others_value(list(others = "metabolomics_nomenclature: smiles"), "refmet"),
    "metabolomics_nomenclature: smiles"
  )
  # Non-metabolomics uploads are left alone.
  expect_null(upload_others_value(list(), NULL))
  expect_equal(upload_others_value(list(others = "note: x"), NULL), "note: x")
})

test_that("resolve_feature_ids resolves metabolites by refmet_name and reports unknown ones", {
  skip_if_no_test_db()

  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  hashkey <- "upload_met_test_hashkey_000001"
  DBI::dbExecute(conn, sprintf("DELETE FROM metabolite_reference WHERE metabolite_hashkey = '%s'", hashkey))
  on.exit(
    suppressWarnings(DBI::dbExecute(conn, sprintf("DELETE FROM metabolite_reference WHERE metabolite_hashkey = '%s'", hashkey))),
    add = TRUE
  )
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO metabolite_reference (refmet_name, is_current, version, metabolite_hashkey) VALUES ('Upload Test Metabolite', 1, 1, '%s')",
    hashkey
  ))
  expected_id <- DBI::dbGetQuery(conn, sprintf(
    "SELECT metabolite_id FROM metabolite_reference WHERE metabolite_hashkey = '%s'", hashkey
  ))$metabolite_id[1]

  ok <- resolve_feature_ids(
    conn,
    data.frame(feature_name = "Upload Test Metabolite", probe_id = "Upload Test Metabolite", stringsAsFactors = FALSE),
    "metabolite_reference", organism_id = NA, feature_key = "feature_name",
    metabolomics_nomenclature = "refmet"
  )
  expect_true(ok$ok)
  expect_equal(ok$feature_ids, as.integer(expected_id))

  missing <- resolve_feature_ids(
    conn,
    data.frame(feature_name = "Not A Real Metabolite At All", probe_id = "x", stringsAsFactors = FALSE),
    "metabolite_reference", organism_id = NA, feature_key = "feature_name",
    metabolomics_nomenclature = "refmet"
  )
  expect_false(missing$ok)
  expect_equal(missing$reason, "unknown_features")
  expect_match(missing$message, "Not A Real Metabolite At All", fixed = TRUE)

  bad_dict <- resolve_feature_ids(
    conn, data.frame(feature_name = "x", probe_id = "x", stringsAsFactors = FALSE),
    "metabolite_reference", organism_id = NA, feature_key = "feature_name",
    metabolomics_nomenclature = "not_a_dictionary"
  )
  expect_false(bad_dict$ok)
  expect_equal(bad_dict$reason, "invalid_upload")
})

test_that("resolve_feature_ids resolves metabolites case-insensitively, matching the SQL collation", {
  # metabolite_reference.refmet_name collates utf8_unicode_ci (case-
  # insensitive), so the SQL lookup already matches 'upload test metabolite'
  # (or any other case) against a stored 'Upload Test Metabolite'. The R-side
  # id_by_name[names_in] subscript that follows is an exact, case-sensitive
  # match, and used to be keyed on the stored casing -- so a differently-cased
  # upload was found by SQL and then dropped right back out in R, reported as
  # "not in metabolite_reference" for a name that demonstrably is. RefMet
  # names are mixed case (Cholic acid, Glucose), so this rejected real
  # uploads. Deliberately uses a DIFFERENT case than the stored fixture row --
  # the test above uses the exact stored case and cannot catch this.
  skip_if_no_test_db()

  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  hashkey <- "upload_met_test_hashkey_000002"
  DBI::dbExecute(conn, sprintf("DELETE FROM metabolite_reference WHERE metabolite_hashkey = '%s'", hashkey))
  on.exit(
    suppressWarnings(DBI::dbExecute(conn, sprintf("DELETE FROM metabolite_reference WHERE metabolite_hashkey = '%s'", hashkey))),
    add = TRUE
  )
  DBI::dbExecute(conn, sprintf(
    "INSERT INTO metabolite_reference (refmet_name, is_current, version, metabolite_hashkey) VALUES ('Upload Case Test Metabolite', 1, 1, '%s')",
    hashkey
  ))
  expected_id <- DBI::dbGetQuery(conn, sprintf(
    "SELECT metabolite_id FROM metabolite_reference WHERE metabolite_hashkey = '%s'", hashkey
  ))$metabolite_id[1]

  lower_case <- resolve_feature_ids(
    conn,
    data.frame(feature_name = "upload case test metabolite", probe_id = "upload case test metabolite", stringsAsFactors = FALSE),
    "metabolite_reference", organism_id = NA, feature_key = "feature_name",
    metabolomics_nomenclature = "refmet"
  )
  expect_true(lower_case$ok)
  expect_equal(lower_case$feature_ids, as.integer(expected_id))

  upper_case <- resolve_feature_ids(
    conn,
    data.frame(feature_name = "UPLOAD CASE TEST METABOLITE", probe_id = "UPLOAD CASE TEST METABOLITE", stringsAsFactors = FALSE),
    "metabolite_reference", organism_id = NA, feature_key = "feature_name",
    metabolomics_nomenclature = "refmet"
  )
  expect_true(upper_case$ok)
  expect_equal(upper_case$feature_ids, as.integer(expected_id))
})
