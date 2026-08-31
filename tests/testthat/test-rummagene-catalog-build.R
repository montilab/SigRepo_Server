# The build job, exercised end to end against a tiny local GMT with MeSH
# injected rather than fetched -- so this test touches no network and is
# deterministic. The network path itself is covered by the parser tests in
# test-rummagene-ingest.R, following the same convention as test-rummagene.R.
#
# test_conn() and seed_features() come from tests/testthat/helper-rummagene.R,
# which testthat sources before every test file -- do NOT redefine them here.
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog_build.R"), local = FALSE)

test_that("build_rummagene_catalog keeps only the sets that pass every gate", {
  conn <- test_conn()
  new_hashkeys <- base::character(0)
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-build'")
            unseed_features(conn, new_hashkeys)
            DBI::dbDisconnect(conn) }, add = TRUE)
  # Scoped to exactly the rows this call creates (see helper-rummagene.R) --
  # without capturing and unseeding new_hashkeys, TP53/MYC's Ensembl ids would
  # leak into transcriptomics_features permanently and falsely satisfy the
  # "feature_absent" gate in every later test run (own test file and others).
  new_hashkeys <- seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510", "ensg00000136997"))

  gmt <- base::tempfile(fileext = ".gmt")
  base::writeLines(c(
    # keeps: human, transcriptomics, both symbols resolve
    "PMC1-t.xlsx-keep\tDEGs\tTP53\tMYC",
    # drops: chicken
    "PMC2-t.xlsx-chicken\tDEGs\tTP53\tMYC",
    # drops: no assay descriptor
    "PMC3-t.xlsx-noassay\tDEGs\tTP53\tMYC",
    # drops: a symbol that maps to an Ensembl id absent from this database
    "PMC4-t.xlsx-absent\tDEGs\tTP53\tEGFR"
  ), gmt)
  on.exit(base::unlink(gmt), add = TRUE)

  articles <- base::list(
    PMC1 = base::list(pmid = "1", mesh = c("Humans", "Transcriptome"),
                      title = "A keeper", year = 2020L, doi = "10.1/a"),
    PMC2 = base::list(pmid = "2", mesh = c("Animals", "Chickens", "Transcriptome"),
                      title = "A chicken paper", year = 2022L, doi = "10.1/b"),
    PMC3 = base::list(pmid = "3", mesh = c("Humans", "Liver"),
                      title = "No assay descriptor", year = 2021L, doi = "10.1/c"),
    PMC4 = base::list(pmid = "4", mesh = c("Humans", "Transcriptome"),
                      title = "Gene absent here", year = 2019L, doi = "10.1/d")
  )

  out <- build_rummagene_catalog(conn, gmt_path = gmt, gmt_version = "test-build",
                                 articles_by_pmcid = articles, progress = FALSE)

  expect_equal(out$examined, 4)
  expect_equal(out$qualified, 1)
  expect_equal(out$rejected$organism, 1)
  expect_equal(out$rejected$assay_type, 1)
  expect_equal(out$rejected$feature_absent, 1)

  got <- DBI::dbGetQuery(conn, "SELECT term, title, year, doi, pmid FROM rummagene_catalog WHERE gmt_version = 'test-build'")
  expect_equal(got$term, "PMC1-t.xlsx-keep")
  # Citation metadata must actually land -- the browse page shows a paper column,
  # and writing NA here would leave it permanently blank.
  expect_equal(got$title[1], "A keeper")
  expect_equal(got$year[1], 2020)
  expect_equal(got$doi[1], "10.1/a")
  expect_equal(got$pmid[1], "1")
})

test_that("build_rummagene_catalog does not read the whole GMT into memory", {
  # The droplet has ~1GB free and latest.gmt is ~700MB. The job must stream.
  # readLines() with no `n` would materialize the file, so assert the source
  # opens a connection and reads in chunks instead.
  src <- base::paste(base::readLines(
    testthat::test_path("../../api/lib/rummagene_catalog_build.R")
  ), collapse = "\n")

  expect_match(src, "base::file(", fixed = TRUE)
  expect_match(src, "n = chunk_size", fixed = TRUE)
  expect_false(base::grepl("readLines(gmt_path)", src, fixed = TRUE))
})

test_that(".rummagene_article_lookup finds a key in either an environment or a plain list, and returns NULL -- not an error -- for a missing one", {
  # Production hands this an environment (rummagene_fetch_articles_by_pmcid(),
  # for O(1) lookup at ~188k papers); tests and any hand-built/cached fixture
  # hand it a plain named list. Both shapes must behave identically here.
  rec <- base::list(pmid = "1", mesh = c("Humans"), title = "t", year = 2020L, doi = "d")

  env_fixture <- base::new.env(parent = base::emptyenv())
  base::assign("PMC1", rec, envir = env_fixture)
  expect_equal(.rummagene_article_lookup(env_fixture, "PMC1"), rec)
  expect_null(.rummagene_article_lookup(env_fixture, "PMC404"))

  list_fixture <- base::list(PMC1 = rec)
  expect_equal(.rummagene_article_lookup(list_fixture, "PMC1"), rec)
  expect_null(.rummagene_article_lookup(list_fixture, "PMC404"))
})

test_that("build_rummagene_catalog counts lines it could not parse at all, rather than dropping them uncounted", {
  # A blank line, a term with no PMC id, and a line with too few fields never
  # become a candidate -- rummagene_parse_gmt_line() returns NULL for each.
  # They must land in `unparsed`, not vanish between `examined` and every
  # `rejected` bucket, or a truncated download would under-report silently.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-unparsed'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  gmt <- base::tempfile(fileext = ".gmt")
  base::writeLines(c(
    "",                                    # blank
    "no-pmc-id-here\tdesc\tTP53\tMYC",     # no PMC id in the term
    "PMC1-t.xlsx-x\tdesc"                  # fewer than 3 fields (no genes)
  ), gmt)
  on.exit(base::unlink(gmt), add = TRUE)

  out <- NULL
  # Nothing qualifies under 'test-unparsed', so rummagene_catalog_prune()'s
  # own failed-build guard (Task 5) fires as designed -- expected here, not a
  # bug, since this fixture is deliberately all-unparseable.
  expect_warning(
    out <- build_rummagene_catalog(conn, gmt_path = gmt, gmt_version = "test-unparsed",
                                   articles_by_pmcid = base::list(), progress = FALSE),
    "refusing to delete"
  )

  expect_equal(out$unparsed, 3)
  expect_equal(out$examined, 0)
  expect_equal(out$qualified, 0)
})
