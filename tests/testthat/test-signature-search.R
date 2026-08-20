source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/annotate.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature_search.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

if (db_test_available()) {
  sigrepo_dir <- Sys.getenv("SIGREPO_DIR", unset = testthat::test_path("../../../SigRepo"))
  pkgload::load_all(sigrepo_dir, quiet = TRUE, export_all = FALSE, helpers = FALSE)
}

test_that("availability is reported as a reason, not just a boolean", {
  # The UI explains why the feature is off rather than showing a button that
  # always fails, so the reason has to distinguish the two ways it can be off.
  withr::with_envvar(c(LINCS_REFDB = ""), {
    reason <- lincs_unavailable_reason()
    expect_false(is.null(reason))
    expect_true(grepl("not installed|LINCS_REFDB", reason))
  })

  withr::with_envvar(c(LINCS_REFDB = "/definitely/not/here.h5"), {
    reason <- lincs_unavailable_reason()
    expect_false(is.null(reason))
  })

  expect_false(lincs_available())
})

test_that("a search refuses to run when no reference database is configured", {
  withr::with_envvar(c(LINCS_REFDB = ""), {
    res <- lincs_search(list(user_role = "admin"), "whatever")
    expect_false(isTRUE(res$ok))
    expect_equal(res$reason, "unavailable")
  })
})

test_that("query building splits on the sign of score, gates organism and assay type", {
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

  seeded <- query_sql("SELECT COUNT(*) n FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'")$n[1]
  skip_if(seeded == 0, "seed.sql fixture signature not present")

  human <- query_sql("SELECT organism_id FROM organisms WHERE organism = 'Homo sapiens'")$organism_id[1]
  skip_if(is.na(human), "no Homo sapiens organism row")

  # 12 up and 12 down, enough to clear LINCS_MIN_GENES_PER_SIDE, plus one gene
  # that appears in both directions and must be discarded.
  n_side <- 12L
  feats <- data.frame(
    name = c(sprintf("ss_up_%02d", 1:n_side), sprintf("ss_dn_%02d", 1:n_side), "ss_both_a", "ss_both_b"),
    sym  = c(sprintf("SSUP%02d", 1:n_side),  sprintf("SSDN%02d", 1:n_side),  "SSBOTH", "SSBOTH"),
    scr  = c(rep(2.5, n_side), rep(-2.5, n_side), 1.5, -1.5),
    stringsAsFactors = FALSE
  )

  cleanup_names <- paste(sprintf("'%s'", feats$name), collapse = ",")
  exec_sql(sprintf("DELETE FROM transcriptomics_features WHERE feature_name IN (%s)", cleanup_names))
  for (i in seq_len(nrow(feats))) {
    exec_sql(sprintf(
      "INSERT INTO transcriptomics_features (feature_name, organism_id, gene_symbol, version, feature_hashkey)
       VALUES ('%s', %d, '%s', 1, '%s')",
      feats$name[i], human, feats$sym[i], paste0("ss_fh_", i)
    ))
  }
  fids <- query_sql(sprintf("SELECT feature_id, feature_name FROM transcriptomics_features WHERE feature_name IN (%s)", cleanup_names))

  make_sig <- function(hashkey, name, organism_id, assay) {
    exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id IN
                      (SELECT signature_id FROM signatures WHERE signature_hashkey='%s')", hashkey))
    exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey='%s'", hashkey))
    exec_sql(sprintf(
      "INSERT INTO signatures (signature_name, organism_id, direction_type, assay_type, phenotype_id,
                               platform_id, sample_type_id, user_name, visibility, signature_hashkey)
       SELECT '%s', %d, direction_type, '%s', phenotype_id, platform_id, sample_type_id, user_name, 1, '%s'
       FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'",
      name, organism_id, assay, hashkey))
    sid <- query_sql(sprintf("SELECT signature_id FROM signatures WHERE signature_hashkey='%s'", hashkey))$signature_id[1]
    for (i in seq_len(nrow(feats))) {
      fid <- fids$feature_id[fids$feature_name == feats$name[i]]
      exec_sql(sprintf(
        "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
         VALUES (%d, %d, '%s', %f, 'All Features', '%s', '%s')",
        sid, fid, paste0(hashkey, "_", feats$name[i]), feats$scr[i], assay,
        substr(paste0(hashkey, feats$name[i], "0000000000000000000000000000000000"), 1, 32)))
    }
    sid
  }

  hk_human <- "ss_human_hashkey_000000000000001"
  make_sig(hk_human, "SS Human", human, "transcriptomics")
  on.exit({
    for (hk in c(hk_human, "ss_mouse_hashkey_000000000000001", "ss_prot_hashkey_0000000000000001")) {
      exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id IN
                        (SELECT signature_id FROM signatures WHERE signature_hashkey='%s')", hk))
      exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey='%s'", hk))
    }
    exec_sql(sprintf("DELETE FROM transcriptomics_features WHERE feature_name IN (%s)", cleanup_names))
  }, add = TRUE)

  auth <- list(user_name = "ci_admin", user_role = "admin")

  q <- lincs_query_from_signature(auth, hk_human)
  expect_true(isTRUE(q$ok))
  expect_equal(q$n_up, n_side)
  expect_equal(q$n_down, n_side)
  expect_true(all(grepl("^SSUP", q$upset)))
  expect_true(all(grepl("^SSDN", q$downset)))

  # A gene scored in both directions is contradictory: dropped from both sides
  # rather than allowed to push the score two ways at once.
  expect_equal(q$n_ambiguous, 1)
  expect_false("SSBOTH" %in% q$upset)
  expect_false("SSBOTH" %in% q$downset)

  # Non-human is refused with an explanation, not an empty result.
  mouse <- query_sql("SELECT organism_id FROM organisms WHERE organism = 'Mus musculus'")$organism_id[1]
  if (!is.na(mouse)) {
    hk_mouse <- "ss_mouse_hashkey_000000000000001"
    make_sig(hk_mouse, "SS Mouse", mouse, "transcriptomics")
    qm <- lincs_query_from_signature(auth, hk_mouse)
    expect_false(isTRUE(qm$ok))
    expect_equal(qm$reason, "unsupported_organism")
    expect_true(grepl("human cell lines", qm$message))
  }

  # LINCS profiles are transcriptional; a proteomics signature is out of scope.
  hk_prot <- "ss_prot_hashkey_0000000000000001"
  make_sig(hk_prot, "SS Proteomics", human, "proteomics")
  qp <- lincs_query_from_signature(auth, hk_prot)
  expect_false(isTRUE(qp$ok))
  expect_equal(qp$reason, "unsupported_assay_type")

  # An unknown signature is not found, rather than erroring.
  qn <- lincs_query_from_signature(auth, "no_such_signature_hashkey_00000")
  expect_false(isTRUE(qn$ok))
  expect_equal(qn$reason, "not_found")
})
