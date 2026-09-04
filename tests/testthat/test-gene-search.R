source(testthat::test_path("../../api/lib/common.R"), local = FALSE)
source(testthat::test_path("../../api/lib/signature.R"), local = FALSE)
source(testthat::test_path("helper-db.R"), local = FALSE)

test_that("search_signatures_by_genes returns nothing for an empty query", {
  # No database needed: it short-circuits before building any SQL.
  expect_equal(nrow(search_signatures_by_genes(conn = NULL, genes = character())), 0)
  expect_equal(nrow(search_signatures_by_genes(conn = NULL, genes = c("", NA))), 0)
})

test_that("search_signatures_by_genes ranks by overlap, scores Jaccard, and honours visibility", {
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

  organism_id <- query_sql("SELECT organism_id FROM organisms LIMIT 1")$organism_id[1]
  skip_if(is.na(organism_id), "no organism rows to build features against")

  # The fixtures clone this seeded signature's metadata rather than inventing
  # valid organism/phenotype/platform ids of their own. CI loads it from
  # tests/testthat/fixtures/seed.sql; a developer database may not have it.
  seeded <- query_sql("SELECT COUNT(*) AS n FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'")$n[1]
  skip_if(seeded == 0, "seed.sql fixture signature not present")

  # Three genes, and three signatures with a known overlap each:
  #   BIG    -> all 3   PART -> 2   HIDDEN -> 3 but visibility = 0
  feature_names <- c("gs_feat_a", "gs_feat_b", "gs_feat_c")
  gene_symbols <- c("GSTESTA", "GSTESTB", "GSTESTC")
  for (i in seq_along(feature_names)) {
    exec_sql(sprintf("DELETE FROM transcriptomics_features WHERE feature_name = '%s'", feature_names[i]))
    exec_sql(sprintf(
      "INSERT INTO transcriptomics_features (feature_name, organism_id, gene_symbol, version, feature_hashkey)
       VALUES ('%s', %d, '%s', 1, '%s')",
      feature_names[i], organism_id, gene_symbols[i], paste0("gs_fh_", i)
    ))
  }
  feature_ids <- query_sql(sprintf(
    "SELECT feature_id, feature_name FROM transcriptomics_features WHERE feature_name IN ('%s')",
    paste(feature_names, collapse = "','")
  ))

  make_sig <- function(hashkey, name, visibility, which_features) {
    exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id IN
                      (SELECT signature_id FROM signatures WHERE signature_hashkey = '%s')", hashkey))
    exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey = '%s'", hashkey))
    exec_sql(sprintf(
      "INSERT INTO signatures (signature_name, organism_id, direction_type, assay_type, phenotype_id,
                               platform_id, sample_type_id, user_name, visibility, signature_hashkey)
       SELECT '%s', organism_id, direction_type, assay_type, phenotype_id, platform_id, sample_type_id,
              user_name, %d, '%s'
       FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'",
      name, visibility, hashkey
    ))
    sid <- query_sql(sprintf("SELECT signature_id FROM signatures WHERE signature_hashkey = '%s'", hashkey))$signature_id[1]
    for (fn in which_features) {
      fid <- feature_ids$feature_id[feature_ids$feature_name == fn]
      exec_sql(sprintf(
        "INSERT INTO signature_feature_set (signature_id, feature_id, probe_id, group_label, assay_type, sig_feature_hashkey)
         VALUES (%d, %d, '%s', 'All Features', 'transcriptomics', '%s')",
        sid, fid, paste0(hashkey, "_", fn), substr(paste0(hashkey, fn, "hash000000000000000000000000000000"), 1, 32)
      ))
    }
    sid
  }

  make_sig("gs_big_hashkey_00000000000000001", "GS Big", 1, feature_names)
  make_sig("gs_part_hashkey_0000000000000001", "GS Part", 1, feature_names[1:2])
  make_sig("gs_hidden_hashkey_00000000000001", "GS Hidden", 0, feature_names)

  on.exit({
    for (hk in c("gs_big_hashkey_00000000000000001", "gs_part_hashkey_0000000000000001",
                 "gs_hidden_hashkey_00000000000001")) {
      exec_sql(sprintf("DELETE FROM signature_feature_set WHERE signature_id IN
                        (SELECT signature_id FROM signatures WHERE signature_hashkey = '%s')", hk))
      exec_sql(sprintf("DELETE FROM signatures WHERE signature_hashkey = '%s'", hk))
    }
    exec_sql(sprintf("DELETE FROM transcriptomics_features WHERE feature_name IN ('%s')",
                     paste(feature_names, collapse = "','")))
  }, add = TRUE)

  conn <- db_connect_local()
  on.exit(suppressWarnings(DBI::dbDisconnect(conn)), add = TRUE)

  hits <- search_signatures_by_genes(conn, genes = gene_symbols, is_admin = TRUE)
  ours <- hits[hits$signature_hashkey %in% c("gs_big_hashkey_00000000000000001",
                                             "gs_part_hashkey_0000000000000001",
                                             "gs_hidden_hashkey_00000000000001"), , drop = FALSE]
  expect_equal(nrow(ours), 3)

  # Ordered by overlap: the 3-gene signatures outrank the 2-gene one.
  expect_equal(ours$n_overlap[ours$signature_hashkey == "gs_big_hashkey_00000000000000001"], 3)
  expect_equal(ours$n_overlap[ours$signature_hashkey == "gs_part_hashkey_0000000000000001"], 2)
  expect_true(which(ours$signature_hashkey == "gs_big_hashkey_00000000000000001") <
                which(ours$signature_hashkey == "gs_part_hashkey_0000000000000001"))

  # Jaccard: |overlap| / (|query| + |signature genes| - |overlap|).
  # Big  -> 3 / (3 + 3 - 3) = 1
  # Part -> 2 / (3 + 2 - 2) = 0.66667
  expect_equal(ours$jaccard[ours$signature_hashkey == "gs_big_hashkey_00000000000000001"], 1)
  expect_equal(ours$jaccard[ours$signature_hashkey == "gs_part_hashkey_0000000000000001"], 0.66667)

  # The matched symbols come back so the UI can show what actually hit.
  expect_equal(
    sort(strsplit(ours$matched_genes[ours$signature_hashkey == "gs_part_hashkey_0000000000000001"], ",")[[1]]),
    c("GSTESTA", "GSTESTB")
  )

  # visibility = 0 is invisible to a non-admin, and visible to an admin.
  as_viewer <- search_signatures_by_genes(conn, genes = gene_symbols, is_admin = FALSE)
  expect_false("gs_hidden_hashkey_00000000000001" %in% as_viewer$signature_hashkey)
  expect_true("gs_big_hashkey_00000000000000001" %in% as_viewer$signature_hashkey)

  # A signature is never a hit for its own genes.
  excluded <- search_signatures_by_genes(conn, genes = gene_symbols, is_admin = TRUE,
                                         exclude_hashkey = "gs_big_hashkey_00000000000000001")
  expect_false("gs_big_hashkey_00000000000000001" %in% excluded$signature_hashkey)
  expect_true("gs_part_hashkey_0000000000000001" %in% excluded$signature_hashkey)

  # min_overlap drops the partial match.
  strict <- search_signatures_by_genes(conn, genes = gene_symbols, is_admin = TRUE, min_overlap = 3)
  expect_false("gs_part_hashkey_0000000000000001" %in% strict$signature_hashkey)
  expect_true("gs_big_hashkey_00000000000000001" %in% strict$signature_hashkey)

  # Gene matching is case-insensitive on both sides.
  lowered <- search_signatures_by_genes(conn, genes = tolower(gene_symbols), is_admin = TRUE)
  expect_true("gs_big_hashkey_00000000000000001" %in% lowered$signature_hashkey)
})

test_that("the feature join is keyed on assay_type, not feature_id alone", {
  skip_if_no_test_db()

  # feature_id is a separate AUTO_INCREMENT per feature table, and the ranges
  # overlap in practice (on production, transcriptomics reaches 135,856 while
  # proteomics reaches 205,003). Joining on feature_id alone would match a
  # proteomics feature against a transcriptomics one and invent overlap.
  sql <- deparse(search_signatures_by_genes)
  joined <- paste(sql, collapse = " ")
  expect_true(grepl("sfs.assay_type = g.assay_type", joined, fixed = TRUE))
})
