# Rummagene Catalog Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build a browsable catalog of Rummagene gene sets whose metadata is fully attested by PubMed MeSH, from which a person can pull individual sets into SigRepo's `signatures` table as ordinary `OmicSignature`s.

**Architecture:** A weekly build job streams Rummagene's 700MB `latest.gmt`, resolves each paper's organism and assay type from PubMed MeSH, maps gene symbols to Ensembl IDs, and keeps only sets where *every* gene resolves to a row in `transcriptomics_features`. Survivors land in a new `rummagene_catalog` table. A React page browses that table with server-side paging; a pull builds an `OmicSignature` and hands it to the existing `upload_signature()` path.

**Tech Stack:** R Plumber, MySQL, `org.Hs.eg.db` / `AnnotationDbi`, `httr`, `jsonlite`, `xml2`, React + TypeScript, testthat.

**Spec:** [specs/2026-08-31-rummagene-catalog-design.md](../specs/2026-08-31-rummagene-catalog-design.md)

## Global Constraints

- **Nothing is invented.** `phenotype` is always the literal `"unknown"`; `direction_type` is always the literal `"uni-directional"`. `organism` and `assay_type` come only from MeSH, never inferred from a term string, title, or filename.
- **Scope is human transcriptomics only.** Reject any other organism or assay type at catalog-build time. Do not add `org.Mm.eg.db`, proteomics, genetic_variants, or methylomics handling.
- **The gate is all-or-nothing.** A set enters the catalog only if 100% of its symbols resolve to a `feature_id` in `transcriptomics_features` for that organism. Never drop a gene to make a set fit.
- **The gate checks the live table**, not `org.Hs.eg.db`. Ensembl version drift must move a set out of the catalog, never produce a catalog row that fails on pull.
- **Stream the GMT.** The droplet has 3GB RAM, ~1GB free, no swap. `latest.gmt` is ~700MB and must never be read into memory whole.
- **`api.R` is the only file whose route annotations are parsed.** Helpers go in `api/lib/*.R`, which `api.R` sources alphabetically.
- **All `base::`-prefix calls** in `api/lib/*.R`, matching the surrounding code.
- **Never commit** `.Renviron*`, `.env*`, `.local-data/`, `.superpowers/`. Before any push run: `git ls-tree -r --name-only HEAD | grep -cE '^\.Renviron|^\.env|^\.local-data/|^\.superpowers/'` and require `0`.
- **Test suite is stateful.** Always run with `testthat::set_max_fails(Inf)` so a run cannot abort partway and corrupt the shared DB for the next run. Do **not** set `SIGREPO_ALLOW_DESTRUCTIVE_TESTS=true` against a database whose name lacks "test" — that wipes it.

## Preconditions

Before Task 1, get the existing untracked work onto a proper branch. `api/lib/rummagene_ingest.R` and `tests/testthat/test-rummagene-ingest.R` were written 2026-08-31 and are untracked; the current branch is `feat/lincs-connectivity-search`, which is unrelated.

```bash
cd SigRepo_Server
git checkout dev && git pull
git checkout -b feat/rummagene-catalog
git add api/lib/rummagene_ingest.R tests/testthat/test-rummagene-ingest.R \
        specs/2026-08-31-rummagene-catalog-design.md plans/2026-08-31-rummagene-catalog.md
git commit -m "feat: Rummagene MeSH qualification filter, catalog spec and plan"
```

---

### Task 1: Truncate long Rummagene terms into `signature_name`

Rummagene terms exceed `signatures.signature_name`'s `VARCHAR(255)`. Truncation must stay unique because of `UNIQUE(signature_name, user_name)` — two long terms from the same paper share a long prefix and would collide.

**Files:**
- Modify: `api/lib/rummagene_ingest.R` (append)
- Test: `tests/testthat/test-rummagene-ingest.R` (append)

**Interfaces:**
- Consumes: nothing
- Produces: `rummagene_signature_name(term)` → `character(1)`, at most 255 characters

- [ ] **Step 1: Write the failing tests**

```r
test_that("rummagene_signature_name leaves a short term untouched", {
  expect_equal(rummagene_signature_name("PMC123-t1.xlsx-up"), "PMC123-t1.xlsx-up")
})

test_that("rummagene_signature_name caps a long term at 255 characters", {
  long <- base::paste0("PMC7202592-", base::paste(base::rep("x", 400), collapse = ""))
  out <- rummagene_signature_name(long)
  expect_lte(base::nchar(out), 255)
})

test_that("rummagene_signature_name keeps two long terms sharing a prefix distinct", {
  # The collision that UNIQUE(signature_name, user_name) would otherwise reject:
  # sibling tables from one paper differ only in a suffix past character 255.
  prefix <- base::paste0("PMC7202592-", base::paste(base::rep("x", 300), collapse = ""))
  a <- rummagene_signature_name(base::paste0(prefix, "-cluster_1"))
  b <- rummagene_signature_name(base::paste0(prefix, "-cluster_2"))
  expect_false(base::identical(a, b))
  expect_lte(base::nchar(a), 255)
  expect_lte(base::nchar(b), 255)
})

test_that("rummagene_signature_name is deterministic", {
  long <- base::paste0("PMC1-", base::paste(base::rep("y", 400), collapse = ""))
  expect_equal(rummagene_signature_name(long), rummagene_signature_name(long))
})
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-ingest.R")'
```

Expected: FAIL with `could not find function "rummagene_signature_name"`.

- [ ] **Step 3: Implement**

Append to `api/lib/rummagene_ingest.R`:

```r
# signatures.signature_name is VARCHAR(255) and Rummagene terms routinely run
# longer. Truncating alone is not safe: UNIQUE(signature_name, user_name) means
# two sibling tables from one paper -- identical for their first 300 characters
# and differing only in a trailing "-cluster_1" / "-cluster_2" -- would collide
# and the second pull would be rejected as a duplicate. So a term that needs
# truncating carries a short digest of its FULL text, which restores
# uniqueness. The untruncated term is always preserved in
# rummagene_catalog.term and in the signature's `others` provenance string.
RUMMAGENE_SIGNATURE_NAME_MAX <- 255L

rummagene_signature_name <- function(term) {
  term <- base::as.character(term)[1]
  if (base::nchar(term) <= RUMMAGENE_SIGNATURE_NAME_MAX) {
    return(term)
  }
  digest <- base::substr(digest::digest(term, algo = "md5"), 1, 8)
  suffix <- base::paste0("~", digest)
  base::paste0(
    base::substr(term, 1, RUMMAGENE_SIGNATURE_NAME_MAX - base::nchar(suffix)),
    suffix
  )
}
```

- [ ] **Step 4: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-ingest.R")'
```

Expected: PASS, 62 total.

- [ ] **Step 5: Commit**

```bash
git add api/lib/rummagene_ingest.R tests/testthat/test-rummagene-ingest.R
git commit -m "feat: unique truncation of Rummagene terms into signature_name"
```

---

### Task 2: `rummagene_catalog` table

**Files:**
- Create: `mysql/schema/rummagene_catalog.sql`
- Modify: `api/lib/database_admin.R:99` (add one `run_schema_file` call)
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: nothing
- Produces: the `rummagene_catalog` table

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-rummagene-catalog.R`:

```r
# The Rummagene catalog: build-time gate, storage, and query.
# See specs/2026-08-31-rummagene-catalog-design.md.
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog.R"), local = FALSE)

test_that("the rummagene_catalog schema file declares every column the build job writes", {
  # Guards against the schema drifting from the writer, which is exactly how
  # metabolite_reference and signatures.assay_type went wrong before.
  sql <- base::paste(base::readLines(
    testthat::test_path("../../mysql/schema/rummagene_catalog.sql")
  ), collapse = "\n")

  for (col in c("term", "pmcid", "pmid", "title", "year", "doi", "description",
                "organism", "assay_type", "mesh_evidence", "n_genes",
                "gene_symbols", "feature_names", "gmt_version", "built_at",
                "term_hashkey")) {
    expect_match(sql, base::sprintf("`%s`", col), fixed = TRUE,
                 info = base::sprintf("column %s missing from schema", col))
  }
  # term must outgrow signature_name's 255, and uniqueness must be on the
  # hashkey -- a 512-char unique index exceeds InnoDB's key length under utf8.
  expect_match(sql, "`term` VARCHAR(512)", fixed = TRUE)
  expect_match(sql, "UNIQUE (`term_hashkey`)", fixed = TRUE)
})

test_that("database_admin.R creates the rummagene_catalog table", {
  admin <- base::paste(base::readLines(
    testthat::test_path("../../api/lib/database_admin.R")
  ), collapse = "\n")
  expect_match(admin, "rummagene_catalog.sql", fixed = TRUE)
})
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL — `cannot open file '../../api/lib/rummagene_catalog.R'`.

- [ ] **Step 3: Create the schema file**

Create `mysql/schema/rummagene_catalog.sql`:

```sql
--
-- Table structure for `rummagene_catalog`
--
-- Catalog of Rummagene gene sets that passed the full ingest gate: organism and
-- assay_type attested by PubMed MeSH, and every gene symbol resolving to a
-- feature_id in transcriptomics_features. A row is an OFFER, not a signature --
-- it grants no access and creates nothing until someone pulls it.
--
-- `organism` and `assay_type` are deliberately free text rather than foreign
-- keys: a catalog row must be storable before anything validates it against
-- SigRepo's controlled vocabularies. The pull path does that validation, via
-- the same create_signature.R lookups every other upload uses.
--
CREATE TABLE `rummagene_catalog` (
  `rummagene_catalog_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  -- Rummagene terms exceed signatures.signature_name's VARCHAR(255); the full
  -- term lives here and is truncated only on pull.
  `term`            VARCHAR(512) NOT NULL,
  `pmcid`           VARCHAR(32)  NOT NULL,
  `pmid`            VARCHAR(32)  DEFAULT NULL,
  `title`           TEXT         DEFAULT NULL,
  `year`            INT          DEFAULT NULL,
  `doi`             VARCHAR(255) DEFAULT NULL,
  `description`     TEXT         DEFAULT NULL,
  `organism`        VARCHAR(128) NOT NULL,
  `assay_type`      VARCHAR(64)  NOT NULL,
  -- The MeSH descriptors that attested organism and assay_type, so a reader can
  -- re-check the claim against the PubMed record.
  `mesh_evidence`   TEXT         NOT NULL,
  `n_genes`         INT UNSIGNED NOT NULL,
  -- What the paper published, verbatim -- shown in the UI.
  `gene_symbols`    MEDIUMTEXT   NOT NULL,
  -- The mapped, lowercased Ensembl IDs, so pull needs no mapping at request
  -- time and resolves straight through create_signature.R's feature lookup.
  `feature_names`   MEDIUMTEXT   NOT NULL,
  `gmt_version`     VARCHAR(64)  NOT NULL,
  `built_at`        DATETIME     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  -- md5(tolower(term)). Uniqueness lives here because a 512-char unique index
  -- exceeds InnoDB's key length under utf8.
  `term_hashkey`    VARCHAR(32)  NOT NULL,
  PRIMARY KEY (`rummagene_catalog_id`),
  UNIQUE (`term_hashkey`),
  KEY (`organism`, `assay_type`),
  KEY (`year`),
  KEY (`n_genes`),
  KEY (`pmcid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
```

- [ ] **Step 4: Register it**

In `api/lib/database_admin.R`, after the `genetic_variants_features` line (currently line 99), add:

```r
  run_schema_file("mysql/schema/rummagene_catalog.sql", "rummagene_catalog")
```

- [ ] **Step 5: Create a stub `api/lib/rummagene_catalog.R` so the test file can source it**

```r
# Rummagene catalog: build-time gate, storage, and query.
#
# The build job (api/lib/rummagene_catalog_build.R) streams Rummagene's
# latest.gmt, qualifies each set through api/lib/rummagene_ingest.R, and stores
# the survivors here. See specs/2026-08-31-rummagene-catalog-design.md.
#
# Depends on api/lib/rummagene_ingest.R and the `conn_handler` global in api.R.

if (!base::exists("%||%")) {
  `%||%` <- function(a, b) if (base::is.null(a)) b else a
}
```

- [ ] **Step 6: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 2 tests.

- [ ] **Step 7: Verify the table actually creates**

```bash
docker exec sigrepo-local-mysql sh -c 'mysql -uroot -p"$MYSQL_ROOT_PASSWORD" "$MYSQL_DATABASE" < /dev/stdin' < mysql/schema/rummagene_catalog.sql
docker exec sigrepo-local-mysql sh -c 'mysql -uroot -p"$MYSQL_ROOT_PASSWORD" "$MYSQL_DATABASE" -e "DESCRIBE rummagene_catalog;"'
```

Expected: 17 columns, no error.

- [ ] **Step 8: Commit**

```bash
git add mysql/schema/rummagene_catalog.sql api/lib/database_admin.R \
        api/lib/rummagene_catalog.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: rummagene_catalog table"
```

---

### Task 3: Parse a GMT line

**Files:**
- Modify: `api/lib/rummagene_catalog.R`
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: nothing
- Produces: `rummagene_parse_gmt_line(line)` → `list(term, description, genes, pmcid)` or `NULL` for an unusable line

- [ ] **Step 1: Write the failing tests**

```r
test_that("rummagene_parse_gmt_line splits term, description and genes", {
  line <- "PMC7202592-Table_1.xlsx-liver\tDEGs FDR<0.05\tTP53\tMYC\tEGFR"
  out <- rummagene_parse_gmt_line(line)

  expect_equal(out$term, "PMC7202592-Table_1.xlsx-liver")
  expect_equal(out$description, "DEGs FDR<0.05")
  expect_equal(out$genes, c("TP53", "MYC", "EGFR"))
  expect_equal(out$pmcid, "PMC7202592")
})

test_that("rummagene_parse_gmt_line tolerates an empty description", {
  out <- rummagene_parse_gmt_line("PMC1-t.xlsx-x\t\tTP53\tMYC")
  expect_equal(out$description, "")
  expect_equal(out$genes, c("TP53", "MYC"))
})

test_that("rummagene_parse_gmt_line drops blank and duplicate gene fields", {
  # Real GMT lines carry trailing tabs and repeat symbols.
  out <- rummagene_parse_gmt_line("PMC1-t.xlsx-x\tdesc\tTP53\t\tMYC\tTP53\t")
  expect_equal(out$genes, c("TP53", "MYC"))
})

test_that("rummagene_parse_gmt_line returns NULL for a line with no genes", {
  expect_null(rummagene_parse_gmt_line("PMC1-t.xlsx-x\tdesc"))
  expect_null(rummagene_parse_gmt_line("PMC1-t.xlsx-x\tdesc\t\t"))
})

test_that("rummagene_parse_gmt_line returns NULL when the term carries no PMC id", {
  # Without a PMC id there is no way to reach MeSH, so the set can never
  # qualify. Dropping it at parse time avoids carrying it through the pipeline.
  expect_null(rummagene_parse_gmt_line("some-other-source-table\tdesc\tTP53\tMYC"))
})

test_that("rummagene_parse_gmt_line returns NULL for a blank line", {
  expect_null(rummagene_parse_gmt_line(""))
  expect_null(rummagene_parse_gmt_line("   "))
})
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL with `could not find function "rummagene_parse_gmt_line"`.

- [ ] **Step 3: Implement**

Append to `api/lib/rummagene_catalog.R`:

```r
# One GMT line -> list(term, description, genes, pmcid), or NULL when the line
# cannot yield a candidate.
#
# GMT is `term \t description \t gene \t gene \t ...`. Rummagene's terms start
# with the source article's PMC id, which is the only route to MeSH -- a term
# without one can never qualify, so it is dropped here rather than carried
# through the whole pipeline.
rummagene_parse_gmt_line <- function(line) {
  line <- base::as.character(line)[1]
  if (base::is.na(line) || !base::nzchar(base::trimws(line))) {
    return(NULL)
  }

  fields <- base::strsplit(line, "\t", fixed = TRUE)[[1]]
  if (base::length(fields) < 3) {
    return(NULL)
  }

  term <- base::trimws(fields[1])
  pmcid <- rummagene_pmcid_from_term(term)
  if (base::length(pmcid) == 0 || base::is.na(pmcid)) {
    return(NULL)
  }

  genes <- base::trimws(fields[-c(1, 2)])
  genes <- base::unique(genes[base::nzchar(genes)])
  if (base::length(genes) == 0) {
    return(NULL)
  }

  base::list(
    term = term,
    description = base::trimws(fields[2]),
    genes = genes,
    pmcid = pmcid
  )
}
```

Note this reuses `rummagene_pmcid_from_term()`, already defined in `api/lib/rummagene.R`. Add `source()` of that file at the top of the test file if it is not already loaded — `api.R` loads it in production because it sources `api/lib/*.R` alphabetically and `rummagene.R` precedes `rummagene_catalog.R`.

- [ ] **Step 4: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 8 tests.

- [ ] **Step 5: Commit**

```bash
git add api/lib/rummagene_catalog.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: GMT line parsing for the Rummagene catalog"
```

---

### Task 4: Symbol → feature_id resolution and the all-or-nothing gate

**Files:**
- Modify: `api/lib/rummagene_catalog.R`
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: `rummagene_parse_gmt_line()` from Task 3
- Produces:
  - `rummagene_map_symbols(symbols, organism)` → named `character` vector, symbol → lowercased Ensembl ID, `NA` where unmapped
  - `rummagene_resolve_features(conn, feature_names, organism_id)` → `character` vector of the `feature_name`s present in `transcriptomics_features`
  - `rummagene_gate(conn, parsed, organism, organism_id)` → `list(ok, feature_names)` or `list(ok = FALSE, reason)`

- [ ] **Step 1: Write the failing tests**

```r
test_that("rummagene_map_symbols returns lowercased Ensembl ids for human symbols", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  out <- rummagene_map_symbols(c("TP53", "MYC"), "Homo sapiens")
  # feature_name in transcriptomics_features is stored lowercased
  # (updateTranscriptomicsFeatureSet does trimws(tolower(ensembl_gene_id))),
  # so the mapping must match that form or nothing will ever resolve.
  expect_equal(out[["TP53"]], "ensg00000141510")
  expect_equal(out[["MYC"]], "ensg00000136997")
})

test_that("rummagene_map_symbols returns NA for a symbol with no Ensembl id", {
  testthat::skip_if_not(requireNamespace("org.Hs.eg.db", quietly = TRUE), "org.Hs.eg.db not installed")

  # A retired alias. Measured 2026-08-31: LGTN, TRA, SOGA1 and CCDC153 are the
  # symbols that fail across the sampled corpus.
  out <- rummagene_map_symbols(c("TP53", "LGTN"), "Homo sapiens")
  expect_false(base::is.na(out[["TP53"]]))
  expect_true(base::is.na(out[["LGTN"]]))
})

test_that("rummagene_map_symbols refuses an organism outside scope", {
  expect_error(rummagene_map_symbols("TP53", "Mus musculus"), "only Homo sapiens")
})

test_that("rummagene_gate accepts a set whose every symbol resolves", {
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510", "ensg00000136997"))

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("TP53", "MYC"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_true(out$ok)
  expect_setequal(out$feature_names, c("ensg00000141510", "ensg00000136997"))
})

test_that("rummagene_gate rejects a set with one unmappable symbol", {
  # The whole point of the 100%-mappable rule: a single dead alias disqualifies
  # the set rather than being silently dropped, so a stored signature always
  # matches the published gene list exactly.
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510"))

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("TP53", "LGTN"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_false(out$ok)
  expect_equal(out$reason, "unmapped_symbol")
})

test_that("rummagene_gate rejects a set whose Ensembl id is absent from the reference table", {
  # Distinct from unmapped: the symbol maps fine, but that Ensembl id is not in
  # THIS database. Checking the live table rather than org.Hs.eg.db is what
  # makes Ensembl version drift move a set out of the catalog instead of
  # producing a row that fails on pull.
  conn <- test_conn()
  on.exit(DBI::dbDisconnect(conn), add = TRUE)
  seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510"))

  parsed <- base::list(term = "PMC1-t.xlsx-x", description = "d",
                       genes = c("TP53", "MYC"), pmcid = "PMC1")
  out <- rummagene_gate(conn, parsed, organism = "Homo sapiens", organism_id = 2L)

  expect_false(out$ok)
  expect_equal(out$reason, "feature_absent")
})
```

Add these helpers at the top of `tests/testthat/test-rummagene-catalog.R`, below the `source()` calls:

```r
# A connection to the local dev database, and a way to put known rows into
# transcriptomics_features without disturbing what is already there. Every
# seeded row is removed again by the caller's on.exit, so these tests do not
# depend on -- or damage -- the basket import.
test_conn <- function() {
  testthat::skip_if_not(base::nzchar(base::Sys.getenv("DB_NAME")), "no database configured")
  DBI::dbConnect(
    RMySQL::MySQL(),
    host = base::Sys.getenv("DB_HOST"), port = base::as.integer(base::Sys.getenv("DB_PORT")),
    user = base::Sys.getenv("DB_USER"), password = base::Sys.getenv("DB_PASSWORD"),
    dbname = base::Sys.getenv("DB_NAME")
  )
}

seed_features <- function(conn, organism_id, feature_names) {
  for (fn in feature_names) {
    hk <- collection_hash(fn, organism_id)
    DBI::dbExecute(conn, base::sprintf(
      "INSERT IGNORE INTO transcriptomics_features
         (feature_name, organism_id, gene_symbol, is_current, version, feature_hashkey)
       VALUES (%s, %d, NULL, 1, 1, %s)",
      DBI::dbQuoteLiteral(conn, fn), organism_id, DBI::dbQuoteLiteral(conn, hk)
    ))
  }
}
```

`collection_hash()` lives in `api/lib/collection.R`; add `source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)` to the test file's header.

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL with `could not find function "rummagene_map_symbols"`.

- [ ] **Step 3: Implement**

Append to `api/lib/rummagene_catalog.R`:

```r
# Scope is human transcriptomics (see the spec). Mouse needs org.Mm.eg.db, and
# separately needs updateTranscriptomicsFeatureSet()'s hgnc_symbol -> mgi_symbol
# bug fixed, so it is refused here rather than half-supported.
RUMMAGENE_CATALOG_ORGANISM <- "Homo sapiens"

# symbol -> lowercased Ensembl gene id, NA where the symbol has none.
#
# Lowercased because updateTranscriptomicsFeatureSet() stores
# feature_name = trimws(tolower(ensembl_gene_id)); matching any other case
# would resolve nothing.
rummagene_map_symbols <- function(symbols, organism) {
  if (!base::identical(base::as.character(organism)[1], RUMMAGENE_CATALOG_ORGANISM)) {
    base::stop(
      "The Rummagene catalog covers only Homo sapiens (asked for '",
      base::as.character(organism)[1], "')."
    )
  }
  symbols <- base::unique(base::as.character(symbols))

  mapped <- base::suppressMessages(AnnotationDbi::mapIds(
    org.Hs.eg.db::org.Hs.eg.db,
    keys = symbols, keytype = "SYMBOL", column = "ENSEMBL", multiVals = "first"
  ))
  out <- base::tolower(base::as.character(mapped))
  out[base::is.na(mapped)] <- NA_character_
  stats::setNames(out, symbols)
}

# Which of `feature_names` actually exist in transcriptomics_features for this
# organism. Resolved by feature_hashkey, the same way
# create_signature.R's resolve_feature_ids() does, so the gate and the eventual
# insert agree by construction.
rummagene_resolve_features <- function(conn, feature_names, organism_id) {
  feature_names <- base::unique(base::as.character(feature_names))
  if (base::length(feature_names) == 0) {
    return(base::character(0))
  }
  hashkeys <- base::vapply(
    feature_names, function(fn) collection_hash(fn, organism_id), base::character(1)
  )
  found <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT feature_hashkey FROM transcriptomics_features WHERE feature_hashkey IN (%s)",
    base::paste(DBI::dbQuoteLiteral(conn, base::unname(hashkeys)), collapse = ",")
  ))
  base::unname(feature_names[hashkeys %in% found$feature_hashkey])
}

# The all-or-nothing gate. Returns list(ok = TRUE, feature_names) only when
# EVERY symbol maps to an Ensembl id AND every one of those ids is present in
# this database's reference table.
#
# Two distinct rejections, kept distinct because they mean different things: a
# dead alias is a property of the source set, while a missing feature is a
# property of THIS database and may change when the reference table is next
# rebuilt from biomaRt.
rummagene_gate <- function(conn, parsed, organism, organism_id) {
  mapped <- rummagene_map_symbols(parsed$genes, organism)
  if (base::any(base::is.na(mapped))) {
    return(base::list(ok = FALSE, reason = "unmapped_symbol"))
  }

  present <- rummagene_resolve_features(conn, base::unname(mapped), organism_id)
  if (base::length(present) != base::length(base::unique(base::unname(mapped)))) {
    return(base::list(ok = FALSE, reason = "feature_absent"))
  }

  base::list(ok = TRUE, feature_names = base::unname(mapped))
}
```

- [ ] **Step 4: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 14 tests.

- [ ] **Step 5: Commit**

```bash
git add api/lib/rummagene_catalog.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: symbol to feature resolution and the 100%-mappable gate"
```

---

### Task 5: Catalog upsert

**Files:**
- Modify: `api/lib/rummagene_catalog.R`
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: `rummagene_gate()` from Task 4
- Produces:
  - `rummagene_catalog_upsert(conn, rows, gmt_version)` → `integer(1)` count written
  - `rummagene_catalog_prune(conn, gmt_version)` → `integer(1)` count deleted

- [ ] **Step 1: Write the failing tests**

```r
catalog_row_fixture <- function(term = "PMC1-t.xlsx-x", genes = c("TP53", "MYC")) {
  base::list(
    term = term, pmcid = "PMC1", pmid = "111", title = "A paper", year = 2020L,
    doi = "10.1/x", description = "d", organism = "Homo sapiens",
    assay_type = "transcriptomics", mesh_evidence = "Humans, Transcriptome",
    gene_symbols = genes, feature_names = c("ensg00000141510", "ensg00000136997")
  )
}

test_that("rummagene_catalog_upsert writes a row that reads back intact", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  n <- rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")
  expect_equal(n, 1)

  got <- DBI::dbGetQuery(conn, "SELECT * FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
  expect_equal(base::nrow(got), 1)
  expect_equal(got$term[1], "PMC1-t.xlsx-x")
  expect_equal(got$n_genes[1], 2)
  expect_equal(got$organism[1], "Homo sapiens")
  # Genes round-trip as a delimited list in both namespaces.
  expect_equal(base::strsplit(got$gene_symbols[1], ",", fixed = TRUE)[[1]], c("TP53", "MYC"))
  expect_equal(base::strsplit(got$feature_names[1], ",", fixed = TRUE)[[1]],
               c("ensg00000141510", "ensg00000136997"))
})

test_that("rummagene_catalog_upsert is idempotent", {
  # A second build over an unchanged GMT must be a no-op, not a duplicate-key
  # error and not a second row.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
            DBI::dbDisconnect(conn) }, add = TRUE)

  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")
  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")

  got <- DBI::dbGetQuery(conn, "SELECT COUNT(*) n FROM rummagene_catalog WHERE gmt_version = 'test-v1'")
  expect_equal(got$n[1], 1)
})

test_that("rummagene_catalog_upsert refreshes a term whose genes changed", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version IN ('test-v1','test-v2')")
            DBI::dbDisconnect(conn) }, add = TRUE)

  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture()), gmt_version = "test-v1")
  changed <- catalog_row_fixture(genes = c("TP53", "MYC", "EGFR"))
  rummagene_catalog_upsert(conn, base::list(changed), gmt_version = "test-v2")

  got <- DBI::dbGetQuery(conn, "SELECT n_genes, gmt_version FROM rummagene_catalog WHERE pmcid = 'PMC1'")
  expect_equal(base::nrow(got), 1)
  expect_equal(got$n_genes[1], 3)
  expect_equal(got$gmt_version[1], "test-v2")
})

test_that("rummagene_catalog_prune deletes rows from earlier builds only", {
  # A set withdrawn from Rummagene stops being offered. Any signature already
  # pulled from it is untouched -- it lives in `signatures` with its provenance,
  # and there is deliberately no FK between the two tables.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version IN ('test-v1','test-v2')")
            DBI::dbDisconnect(conn) }, add = TRUE)

  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture(term = "PMC1-old")), gmt_version = "test-v1")
  rummagene_catalog_upsert(conn, base::list(catalog_row_fixture(term = "PMC1-new")), gmt_version = "test-v2")

  deleted <- rummagene_catalog_prune(conn, gmt_version = "test-v2")
  expect_gte(deleted, 1)

  remaining <- DBI::dbGetQuery(conn, "SELECT term FROM rummagene_catalog WHERE gmt_version = 'test-v2'")
  expect_equal(remaining$term, "PMC1-new")
})
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL with `could not find function "rummagene_catalog_upsert"`.

- [ ] **Step 3: Implement**

Append to `api/lib/rummagene_catalog.R`:

```r
# Gene lists are stored as comma-delimited text. Gene symbols and Ensembl ids
# never contain a comma, so this needs no escaping, and it keeps the column
# greppable from SQL for debugging.
.rummagene_join_genes <- function(x) base::paste(base::as.character(x), collapse = ",")

# Write catalog rows. Keyed on term_hashkey = md5(tolower(term)) -- the same
# formula collection_hash() uses everywhere else in this codebase -- so a
# re-run over an unchanged GMT updates in place rather than duplicating.
rummagene_catalog_upsert <- function(conn, rows, gmt_version) {
  written <- 0L
  for (r in rows %||% base::list()) {
    hk <- collection_hash(r$term, "")
    DBI::dbExecute(conn, base::sprintf(
      "INSERT INTO rummagene_catalog
         (term, pmcid, pmid, title, year, doi, description, organism, assay_type,
          mesh_evidence, n_genes, gene_symbols, feature_names, gmt_version, term_hashkey)
       VALUES (%s, %s, %s, %s, %s, %s, %s, %s, %s, %s, %d, %s, %s, %s, %s)
       ON DUPLICATE KEY UPDATE
         pmcid = VALUES(pmcid), pmid = VALUES(pmid), title = VALUES(title),
         year = VALUES(year), doi = VALUES(doi), description = VALUES(description),
         organism = VALUES(organism), assay_type = VALUES(assay_type),
         mesh_evidence = VALUES(mesh_evidence), n_genes = VALUES(n_genes),
         gene_symbols = VALUES(gene_symbols), feature_names = VALUES(feature_names),
         gmt_version = VALUES(gmt_version), built_at = CURRENT_TIMESTAMP",
      DBI::dbQuoteLiteral(conn, r$term),
      DBI::dbQuoteLiteral(conn, r$pmcid),
      sql_value(conn, r$pmid),
      sql_value(conn, r$title),
      sql_value(conn, r$year),
      sql_value(conn, r$doi),
      sql_value(conn, r$description),
      DBI::dbQuoteLiteral(conn, r$organism),
      DBI::dbQuoteLiteral(conn, r$assay_type),
      DBI::dbQuoteLiteral(conn, r$mesh_evidence),
      base::length(r$gene_symbols),
      DBI::dbQuoteLiteral(conn, .rummagene_join_genes(r$gene_symbols)),
      DBI::dbQuoteLiteral(conn, .rummagene_join_genes(r$feature_names)),
      DBI::dbQuoteLiteral(conn, gmt_version),
      DBI::dbQuoteLiteral(conn, hk)
    ))
    written <- written + 1L
  }
  written
}

# Drop rows the current build did not touch -- i.e. sets Rummagene has withdrawn
# or that no longer pass the gate. A signature someone already pulled is
# unaffected: it lives in `signatures` and there is no FK back to here.
rummagene_catalog_prune <- function(conn, gmt_version) {
  DBI::dbExecute(conn, base::sprintf(
    "DELETE FROM rummagene_catalog WHERE gmt_version <> %s",
    DBI::dbQuoteLiteral(conn, gmt_version)
  ))
}
```

`sql_value()` is defined in `api/lib/create_signature.R`; add a `source()` of that file to the test header.

- [ ] **Step 4: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 18 tests.

- [ ] **Step 5: Commit**

```bash
git add api/lib/rummagene_catalog.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: idempotent Rummagene catalog upsert and prune"
```

---

### Task 6: Catalog search query

**Files:**
- Modify: `api/lib/rummagene_catalog.R`
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: `rummagene_catalog_upsert()` from Task 5
- Produces: `search_rummagene_catalog(conn, q, organism, assay_type, year_min, year_max, n_genes_min, n_genes_max, limit, offset, sort_by, sort_dir)` → `list(count, rows)`

- [ ] **Step 1: Write the failing tests**

```r
seed_catalog <- function(conn) {
  rows <- base::list(
    base::modifyList(catalog_row_fixture(term = "PMC1-liver-up"),  base::list(title = "Liver study", year = 2019L)),
    base::modifyList(catalog_row_fixture(term = "PMC2-tumor-down"), base::list(title = "Tumor study", year = 2023L, pmcid = "PMC2")),
    base::modifyList(catalog_row_fixture(term = "PMC3-liver-down"), base::list(title = "Another liver", year = 2021L, pmcid = "PMC3"))
  )
  rummagene_catalog_upsert(conn, rows, gmt_version = "test-search")
}

test_that("search_rummagene_catalog returns a page plus the total matching count", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, limit = 2, offset = 0)
  expect_equal(base::nrow(out$rows), 2)
  expect_gte(out$count, 3)
})

test_that("search_rummagene_catalog matches free text against term and title", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, q = "liver", limit = 50)
  expect_true(base::all(base::grepl("liver", base::tolower(
    base::paste(out$rows$term, out$rows$title)))))
  expect_gte(base::nrow(out$rows), 2)
})

test_that("search_rummagene_catalog filters by year range", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, year_min = 2021, limit = 50)
  expect_true(base::all(out$rows$year >= 2021))
})

test_that("search_rummagene_catalog sorts server-side on a whitelisted column", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, sort_by = "year", sort_dir = "desc", limit = 50)
  expect_equal(out$rows$year, base::sort(out$rows$year, decreasing = TRUE))
})

test_that("search_rummagene_catalog ignores an unknown sort column instead of interpolating it", {
  # sort_by lands in ORDER BY, where quoting cannot protect it -- the same
  # reasoning as .signature_sort_columns in api/lib/signature.R.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  expect_no_error(search_rummagene_catalog(conn, sort_by = "year; DROP TABLE rummagene_catalog--", limit = 5))
  expect_equal(base::nrow(DBI::dbGetQuery(conn, "SELECT 1 FROM rummagene_catalog LIMIT 1")), 1)
})

test_that("search_rummagene_catalog omits the large gene columns", {
  # The list endpoint must never ship gene_symbols/feature_names -- 135k rows of
  # 40 genes each is why they are fetched only on a detail view.
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-search'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_catalog(conn)

  out <- search_rummagene_catalog(conn, limit = 5)
  expect_false("gene_symbols" %in% base::colnames(out$rows))
  expect_false("feature_names" %in% base::colnames(out$rows))
})
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL with `could not find function "search_rummagene_catalog"`.

- [ ] **Step 3: Implement**

Append to `api/lib/rummagene_catalog.R`:

```r
# sort_by is interpolated into ORDER BY, where dbQuoteLiteral cannot protect it.
# Whitelist, exactly as .signature_sort_columns does in api/lib/signature.R.
.rummagene_catalog_sort_columns <- base::list(
  term       = "term",
  title      = "title",
  year       = "year",
  n_genes    = "n_genes",
  organism   = "organism",
  assay_type = "assay_type"
)

# One page of the catalog plus the TOTAL count of matching rows, so the client
# can render pager controls while holding only one page.
#
# gene_symbols and feature_names are deliberately NOT selected: at ~135k rows of
# ~40 genes each they dwarf everything else, and only a detail view needs them.
search_rummagene_catalog <- function(conn, q = NULL, organism = NULL, assay_type = NULL,
                                     year_min = NULL, year_max = NULL,
                                     n_genes_min = NULL, n_genes_max = NULL,
                                     limit = 25, offset = 0,
                                     sort_by = NULL, sort_dir = "asc") {
  where <- base::character(0)
  add <- function(clause) where <<- c(where, clause)

  q <- base::trimws(base::as.character(q %||% ""))
  if (base::nzchar(q)) {
    like <- DBI::dbQuoteLiteral(conn, base::paste0("%", q, "%"))
    add(base::sprintf("(term LIKE %s OR title LIKE %s OR description LIKE %s)", like, like, like))
  }
  if (base::length(organism) == 1 && base::nzchar(base::as.character(organism))) {
    add(base::sprintf("organism = %s", DBI::dbQuoteLiteral(conn, organism)))
  }
  if (base::length(assay_type) == 1 && base::nzchar(base::as.character(assay_type))) {
    add(base::sprintf("assay_type = %s", DBI::dbQuoteLiteral(conn, assay_type)))
  }
  for (bound in base::list(
    base::list(v = year_min,    col = "year",    op = ">="),
    base::list(v = year_max,    col = "year",    op = "<="),
    base::list(v = n_genes_min, col = "n_genes", op = ">="),
    base::list(v = n_genes_max, col = "n_genes", op = "<=")
  )) {
    n <- base::suppressWarnings(base::as.integer(bound$v %||% NA))
    if (!base::is.na(n)) {
      add(base::sprintf("%s %s %d", bound$col, bound$op, n))
    }
  }

  where_sql <- if (base::length(where) == 0) "" else base::paste("WHERE", base::paste(where, collapse = " AND "))

  count <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT COUNT(*) AS n FROM rummagene_catalog %s", where_sql
  ))$n[1]

  sort_key <- base::trimws(base::as.character(sort_by %||% ""))
  sort_expr <- if (base::nzchar(sort_key)) .rummagene_catalog_sort_columns[[sort_key]] else NULL
  if (base::is.null(sort_expr)) {
    sort_expr <- "year"
  }
  sort_dir_sql <- if (base::identical(base::tolower(base::trimws(base::as.character(sort_dir %||% "asc"))), "desc")) "DESC" else "ASC"

  rows <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT rummagene_catalog_id, term, pmcid, pmid, title, year, doi, description,
            organism, assay_type, mesh_evidence, n_genes, gmt_version, built_at, term_hashkey
     FROM rummagene_catalog %s
     ORDER BY %s %s, term ASC
     LIMIT %d OFFSET %d",
    where_sql, sort_expr, sort_dir_sql,
    base::max(1L, base::as.integer(limit)), base::max(0L, base::as.integer(offset))
  ))

  base::list(count = base::as.integer(count), rows = rows)
}

# One catalog row WITH its genes, for a detail view or a pull.
get_rummagene_catalog_entry <- function(conn, term) {
  hk <- collection_hash(term, "")
  row <- DBI::dbGetQuery(conn, base::sprintf(
    "SELECT * FROM rummagene_catalog WHERE term_hashkey = %s LIMIT 1",
    DBI::dbQuoteLiteral(conn, hk)
  ))
  if (base::nrow(row) == 0) {
    return(NULL)
  }
  base::list(
    term = row$term[1], pmcid = row$pmcid[1], pmid = row$pmid[1],
    title = row$title[1], year = row$year[1], doi = row$doi[1],
    description = row$description[1], organism = row$organism[1],
    assay_type = row$assay_type[1], mesh_evidence = row$mesh_evidence[1],
    gene_symbols  = base::strsplit(row$gene_symbols[1],  ",", fixed = TRUE)[[1]],
    feature_names = base::strsplit(row$feature_names[1], ",", fixed = TRUE)[[1]]
  )
}
```

- [ ] **Step 4: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 24 tests.

- [ ] **Step 5: Commit**

```bash
git add api/lib/rummagene_catalog.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: server-side paged, sorted, filtered catalog search"
```

---

### Task 7: The build job

**Files:**
- Create: `api/lib/rummagene_catalog_build.R`
- Create: `scripts/build_rummagene_catalog.R`
- Test: `tests/testthat/test-rummagene-catalog-build.R`

**Interfaces:**
- Consumes: `rummagene_parse_gmt_line()`, `rummagene_gate()`, `rummagene_catalog_upsert()`, `rummagene_catalog_prune()`, and `rummagene_mesh_organism()` / `rummagene_mesh_assay_type()` from `rummagene_ingest.R`
- Produces:
  - `rummagene_parse_article_xml(xml_text)` → named list, PMID → `list(mesh, title, year, doi)`
  - `rummagene_fetch_articles_by_pmcid(pmcids)` → named list, PMC id → `list(mesh, title, year, doi)`
  - `build_rummagene_catalog(conn, gmt_path, gmt_version, articles_by_pmcid = NULL, chunk_size = 5000, progress = TRUE)` → `list(examined, qualified, rejected)`

- [ ] **Step 0a: Write the failing test for article metadata**

The GMT carries no title, year, or DOI, and `rummagene_fetch_mesh_by_pmcid()` returns only MeSH descriptors — so without this the catalog's `title` / `year` / `doi` columns would be `NULL` and the browse page's "paper" column would always be blank. The same efetch response already contains all of it; it just is not being read.

Append to `tests/testthat/test-rummagene-ingest.R`:

```r
test_that("rummagene_parse_article_xml returns MeSH and citation metadata together", {
  # One efetch response carries both. Parsing it once avoids a second round trip
  # per paper across ~188k papers.
  xml <- '<PubmedArticleSet>
    <PubmedArticle><MedlineCitation><PMID>32341563</PMID>
      <Article>
        <Journal><JournalIssue><PubDate><Year>2020</Year></PubDate></JournalIssue></Journal>
        <ArticleTitle>A liver paper</ArticleTitle>
        <ELocationID EIdType="doi">10.1/abc</ELocationID>
      </Article>
      <MeshHeadingList>
        <MeshHeading><DescriptorName>Humans</DescriptorName></MeshHeading>
        <MeshHeading><DescriptorName>Transcriptome</DescriptorName></MeshHeading>
      </MeshHeadingList>
    </MedlineCitation></PubmedArticle>
  </PubmedArticleSet>'
  out <- rummagene_parse_article_xml(xml)

  expect_equal(out[["32341563"]]$mesh, c("Humans", "Transcriptome"))
  expect_equal(out[["32341563"]]$title, "A liver paper")
  expect_equal(out[["32341563"]]$year, 2020L)
  expect_equal(out[["32341563"]]$doi, "10.1/abc")
})

test_that("rummagene_parse_article_xml tolerates an article missing every optional field", {
  # Unindexed articles, and articles with no DOI or no structured year, are
  # common. They must come back with empty fields rather than erroring, so the
  # build treats them as no_mesh instead of dying mid-pass.
  xml <- '<PubmedArticleSet>
    <PubmedArticle><MedlineCitation><PMID>37223537</PMID></MedlineCitation></PubmedArticle>
  </PubmedArticleSet>'
  out <- rummagene_parse_article_xml(xml)

  expect_equal(out[["37223537"]]$mesh, character(0))
  expect_true(base::is.na(out[["37223537"]]$title))
  expect_true(base::is.na(out[["37223537"]]$year))
  expect_true(base::is.na(out[["37223537"]]$doi))
})
```

- [ ] **Step 0b: Run it and verify it fails**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-ingest.R")'
```

Expected: FAIL with `could not find function "rummagene_parse_article_xml"`.

- [ ] **Step 0c: Implement, appending to `api/lib/rummagene_ingest.R`**

```r
# PubMed efetch XML -> PMID -> list(mesh, title, year, doi).
#
# A superset of rummagene_parse_mesh_xml(), which stays as-is because
# /rummagene/enrich only needs the descriptors. The catalog build needs the
# citation fields too, and one efetch response already carries both -- reading
# them together avoids a second pass over ~188k papers.
rummagene_parse_article_xml <- function(xml_text) {
  doc <- xml2::read_xml(xml_text)
  out <- base::list()
  for (art in xml2::xml_find_all(doc, ".//PubmedArticle")) {
    pmid <- xml2::xml_text(xml2::xml_find_first(art, ".//MedlineCitation/PMID"))
    if (base::is.na(pmid) || !base::nzchar(pmid)) {
      next
    }
    year_txt <- xml2::xml_text(xml2::xml_find_first(art, ".//Article//PubDate/Year"))
    out[[pmid]] <- base::list(
      mesh = base::as.character(xml2::xml_text(
        xml2::xml_find_all(art, ".//MeshHeadingList/MeshHeading/DescriptorName")
      )),
      title = xml2::xml_text(xml2::xml_find_first(art, ".//Article/ArticleTitle")),
      year = if (base::is.na(year_txt)) NA_integer_ else base::suppressWarnings(base::as.integer(year_txt)),
      doi = xml2::xml_text(xml2::xml_find_first(art, ".//Article/ELocationID[@EIdType='doi']"))
    )
  }
  out
}

# pmcid -> list(mesh, title, year, doi). Same batching and rate-limit handling
# as rummagene_fetch_mesh_by_pmcid(), which this parallels.
rummagene_fetch_articles_by_pmcid <- function(pmcids, batch_size = 100, timeout = 90,
                                              pause = 0.4) {
  pmcids <- base::unique(base::as.character(pmcids))
  pmcids <- pmcids[!base::is.na(pmcids) & base::nzchar(pmcids)]
  if (base::length(pmcids) == 0) {
    return(base::list())
  }

  pmid_of <- base::character(0)
  for (i in base::seq(1, base::length(pmcids), by = batch_size)) {
    chunk <- pmcids[i:base::min(i + batch_size - 1, base::length(pmcids))]
    res <- httr::GET(NCBI_IDCONV_URL,
      query = base::list(format = "json", ids = base::paste(chunk, collapse = ",")),
      httr::timeout(timeout))
    if (httr::status_code(res) == 200) {
      pmid_of <- c(pmid_of, base::tryCatch(
        rummagene_parse_idconv(httr::content(res, as = "text", encoding = "UTF-8")),
        error = function(e) base::character(0)))
    }
    base::Sys.sleep(pause)
  }
  if (base::length(pmid_of) == 0) {
    return(base::list())
  }

  by_pmid <- base::list()
  pmids <- base::unname(pmid_of)
  for (i in base::seq(1, base::length(pmids), by = batch_size)) {
    chunk <- pmids[i:base::min(i + batch_size - 1, base::length(pmids))]
    res <- httr::GET(NCBI_EFETCH_URL,
      query = base::list(db = "pubmed", retmode = "xml", id = base::paste(chunk, collapse = ",")),
      httr::timeout(timeout))
    if (httr::status_code(res) == 200) {
      by_pmid <- c(by_pmid, base::tryCatch(
        rummagene_parse_article_xml(httr::content(res, as = "text", encoding = "UTF-8")),
        error = function(e) base::list()))
    }
    base::Sys.sleep(pause)
  }

  out <- base::list()
  for (pmcid in base::names(pmid_of)) {
    rec <- by_pmid[[pmid_of[[pmcid]]]]
    out[[pmcid]] <- base::list(
      pmid  = pmid_of[[pmcid]],
      mesh  = (rec$mesh %||% base::character(0)),
      title = (rec$title %||% NA_character_),
      year  = (rec$year %||% NA_integer_),
      doi   = (rec$doi %||% NA_character_)
    )
  }
  out
}
```

- [ ] **Step 0d: Run it and verify it passes**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-ingest.R")'
```

Expected: PASS, 64 total.

- [ ] **Step 1: Write the failing test for the build itself**

Create `tests/testthat/test-rummagene-catalog-build.R`:

```r
# The build job, exercised end to end against a tiny local GMT with MeSH
# injected rather than fetched -- so this test touches no network and is
# deterministic. The network path itself is covered by the parser tests in
# test-rummagene-ingest.R, following the same convention as test-rummagene.R.
source(testthat::test_path("../../api/lib/collection.R"), local = FALSE)
source(testthat::test_path("../../api/lib/create_signature.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_ingest.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog.R"), local = FALSE)
source(testthat::test_path("../../api/lib/rummagene_catalog_build.R"), local = FALSE)

test_that("build_rummagene_catalog keeps only the sets that pass every gate", {
  conn <- test_conn()
  on.exit({ DBI::dbExecute(conn, "DELETE FROM rummagene_catalog WHERE gmt_version = 'test-build'")
            DBI::dbDisconnect(conn) }, add = TRUE)
  seed_features(conn, organism_id = 2L, feature_names = c("ensg00000141510", "ensg00000136997"))

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
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog-build.R")'
```

Expected: FAIL — `cannot open file '../../api/lib/rummagene_catalog_build.R'`.

- [ ] **Step 3: Implement**

Create `api/lib/rummagene_catalog_build.R`:

```r
# Build the Rummagene catalog from Rummagene's published GMT.
#
# Pipeline, per gene set:
#   parse GMT line -> PMC id -> MeSH -> organism + assay_type -> symbols ->
#   Ensembl ids -> every id present in transcriptomics_features -> store
#
# A set that fails any step is counted and discarded. Nothing is inferred: see
# the governing rule in specs/2026-08-31-rummagene-catalog-design.md.
#
# Depends on api/lib/rummagene_ingest.R and api/lib/rummagene_catalog.R.

RUMMAGENE_GMT_URL <- base::Sys.getenv(
  "RUMMAGENE_GMT_URL",
  unset = "https://rummagene.com/latest.gmt"
)

# Download the GMT to `dest` if it is not already there. ~700MB.
download_rummagene_gmt <- function(dest, url = RUMMAGENE_GMT_URL, overwrite = FALSE) {
  if (base::file.exists(dest) && !overwrite) {
    return(dest)
  }
  utils::download.file(url, destfile = dest, mode = "wb", quiet = FALSE)
  dest
}

# Every distinct PMC id in the GMT, streamed. Needed up front so MeSH can be
# fetched in batches rather than one request per gene set.
rummagene_gmt_pmcids <- function(gmt_path, chunk_size = 5000) {
  con <- base::file(gmt_path, open = "r")
  on.exit(base::close(con), add = TRUE)

  seen <- base::new.env(parent = base::emptyenv())
  repeat {
    lines <- base::readLines(con, n = chunk_size, warn = FALSE)
    if (base::length(lines) == 0) break
    for (ln in lines) {
      parsed <- rummagene_parse_gmt_line(ln)
      if (!base::is.null(parsed)) {
        base::assign(parsed$pmcid, TRUE, envir = seen)
      }
    }
  }
  base::ls(seen)
}

# The build. `articles_by_pmcid` may be supplied (tests, or a cached pass); when
# NULL it is fetched from PubMed. Each record is
# list(pmid, mesh, title, year, doi).
build_rummagene_catalog <- function(conn, gmt_path, gmt_version,
                                    articles_by_pmcid = NULL, chunk_size = 5000,
                                    progress = TRUE) {
  if (base::is.null(articles_by_pmcid)) {
    if (progress) base::message("Collecting PMC ids from the GMT...")
    pmcids <- rummagene_gmt_pmcids(gmt_path, chunk_size = chunk_size)
    if (progress) base::message("  ", base::length(pmcids), " distinct papers; fetching from PubMed...")
    articles_by_pmcid <- rummagene_fetch_articles_by_pmcid(pmcids)
  }

  organism_id <- lookup_id(conn, "organisms", "organism_id", "organism", RUMMAGENE_CATALOG_ORGANISM)
  if (base::is.null(organism_id)) {
    base::stop(
      "Organism '", RUMMAGENE_CATALOG_ORGANISM, "' is not in the organisms table, ",
      "so no Rummagene set can be catalogued. Seed it from mysql/data/organisms.csv first."
    )
  }

  reasons <- c("no_mesh", "organism", "assay_type", "unmapped_symbol", "feature_absent")
  rejected <- stats::setNames(base::as.list(base::rep(0L, base::length(reasons))), reasons)
  examined <- 0L
  qualified <- 0L
  batch <- base::list()

  flush_batch <- function() {
    if (base::length(batch) > 0) {
      rummagene_catalog_upsert(conn, batch, gmt_version = gmt_version)
      batch <<- base::list()
    }
  }

  con <- base::file(gmt_path, open = "r")
  on.exit(base::close(con), add = TRUE)

  repeat {
    lines <- base::readLines(con, n = chunk_size, warn = FALSE)
    if (base::length(lines) == 0) break

    for (ln in lines) {
      parsed <- rummagene_parse_gmt_line(ln)
      if (base::is.null(parsed)) next
      examined <- examined + 1L

      article <- articles_by_pmcid[[parsed$pmcid]] %||% base::list()
      mesh <- article$mesh %||% base::character(0)
      if (base::length(mesh) == 0) { rejected$no_mesh <- rejected$no_mesh + 1L; next }

      organism <- rummagene_mesh_organism(mesh)
      if (base::is.null(organism) || !base::identical(organism, RUMMAGENE_CATALOG_ORGANISM)) {
        rejected$organism <- rejected$organism + 1L; next
      }
      assay_type <- rummagene_mesh_assay_type(mesh)
      if (base::is.null(assay_type) || !base::identical(assay_type, "transcriptomics")) {
        rejected$assay_type <- rejected$assay_type + 1L; next
      }

      gated <- rummagene_gate(conn, parsed, organism = organism, organism_id = organism_id)
      if (!base::isTRUE(gated$ok)) {
        rejected[[gated$reason]] <- rejected[[gated$reason]] + 1L; next
      }

      batch[[base::length(batch) + 1L]] <- base::list(
        term = parsed$term, pmcid = parsed$pmcid,
        pmid  = article$pmid  %||% NA_character_,
        title = article$title %||% NA_character_,
        year  = article$year  %||% NA_integer_,
        doi   = article$doi   %||% NA_character_,
        description = parsed$description, organism = organism, assay_type = assay_type,
        mesh_evidence = base::paste(base::intersect(mesh, c(
          base::names(RUMMAGENE_MESH_ORGANISM), base::names(RUMMAGENE_MESH_ASSAY)
        )), collapse = ", "),
        gene_symbols = parsed$genes, feature_names = gated$feature_names
      )
      qualified <- qualified + 1L

      if (base::length(batch) >= 500) flush_batch()
    }
    if (progress) base::message("  examined ", examined, ", qualified ", qualified)
  }
  flush_batch()

  rummagene_catalog_prune(conn, gmt_version = gmt_version)
  base::list(examined = examined, qualified = qualified, rejected = rejected)
}
```

- [ ] **Step 4: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog-build.R")'
```

Expected: PASS, 2 tests.

- [ ] **Step 5: Add the runner script**

Create `scripts/build_rummagene_catalog.R`:

```r
#!/usr/bin/env Rscript
# Build the Rummagene catalog. Intended to run weekly, and NOT on the droplet --
# the GMT is ~700MB and the NCBI pass takes about 20 minutes.
#
#   Rscript scripts/build_rummagene_catalog.R /path/to/latest.gmt
#
# With no argument the GMT is downloaded to a temporary file first.
base::setwd(base::Sys.getenv("SIGREPO_SERVER_DIR", unset = "/SigRepo_Server"))
for (f in base::sort(base::list.files("api/lib", pattern = "[.]R$", full.names = TRUE))) {
  base::source(f)
}

args <- base::commandArgs(trailingOnly = TRUE)
gmt <- if (base::length(args) > 0) args[1] else download_rummagene_gmt(base::tempfile(fileext = ".gmt"))
version <- base::paste0("latest.gmt ", base::format(base::file.mtime(gmt), "%Y-%m-%d"))

conn <- db_connect_local()
on.exit(DBI::dbDisconnect(conn), add = TRUE)

result <- build_rummagene_catalog(conn, gmt_path = gmt, gmt_version = version)

base::cat("\nexamined :", result$examined, "\n")
base::cat("qualified:", result$qualified, "\n")
for (r in base::names(result$rejected)) {
  base::cat(base::sprintf("  rejected %-16s %d\n", r, result$rejected[[r]]))
}
```

- [ ] **Step 6: Commit**

```bash
git add api/lib/rummagene_catalog_build.R scripts/build_rummagene_catalog.R \
        tests/testthat/test-rummagene-catalog-build.R
git commit -m "feat: streaming Rummagene catalog build job"
```

---

### Task 8: `GET /rummagene/catalog`

**Files:**
- Modify: `api/api.R` (append a route)
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: `search_rummagene_catalog()` from Task 6
- Produces: `GET /rummagene/catalog` returning `{ count, rows }`

- [ ] **Step 1: Write the failing test**

```r
test_that("api.R declares the rummagene catalog route with every documented parameter", {
  api <- base::paste(base::readLines(testthat::test_path("../../api/api.R")), collapse = "\n")
  expect_match(api, "@get /rummagene/catalog", fixed = TRUE)
  for (p in c("api_key", "q", "organism", "assay_type", "year_min", "year_max",
              "n_genes_min", "n_genes_max", "limit", "offset", "sort_by", "sort_dir")) {
    expect_match(api, base::sprintf("#* @param %s", p), fixed = TRUE,
                 info = base::sprintf("route parameter %s not documented", p))
  }
})
```

- [ ] **Step 2: Run the test and verify it fails**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL — route annotation not found.

- [ ] **Step 3: Implement**

Append to `api/api.R`, following the shape of `search_signatures_route`:

```r
#* Browse the Rummagene catalog: literature-mined gene sets whose organism and
#* assay type are attested by PubMed MeSH, and whose every gene resolves in this
#* repository's transcriptomics reference table. Server-side paged and sorted.
#* Gene lists are omitted here and fetched per row on demand.
#* @param api_key
#* @param q Free text matched against term, title and description
#* @param organism
#* @param assay_type
#* @param year_min
#* @param year_max
#* @param n_genes_min
#* @param n_genes_max
#* @param limit
#* @param offset
#* @param sort_by One of term, title, year, n_genes, organism, assay_type
#* @param sort_dir asc (default) or desc
#' @get /rummagene/catalog
rummagene_catalog_route <- function(res, api_key = "", q = "", organism = "", assay_type = "",
                                    year_min = "", year_max = "", n_genes_min = "", n_genes_max = "",
                                    limit = 25, offset = 0, sort_by = "", sort_dir = "asc"){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  conn <- db_connect_local()
  base::on.exit(DBI::dbDisconnect(conn), add = TRUE)

  result <- base::tryCatch(
    search_rummagene_catalog(
      conn,
      q = json_scalar(q), organism = json_scalar(organism), assay_type = json_scalar(assay_type),
      year_min = json_scalar(year_min), year_max = json_scalar(year_max),
      n_genes_min = json_scalar(n_genes_min), n_genes_max = json_scalar(n_genes_max),
      limit = base::as.integer(json_scalar(limit, "25")),
      offset = base::as.integer(json_scalar(offset, "0")),
      sort_by = json_scalar(sort_by), sort_dir = json_scalar(sort_dir, "asc")
    ),
    error = function(e) e
  )
  if (base::inherits(result, "error")) {
    return(json_error(res, 500, base::sprintf("Catalog search failed: %s", base::conditionMessage(result))))
  }

  json_response(res, 200, payload = base::list(count = result$count, rows = result$rows))
}
```

- [ ] **Step 4: Add the detail route**

The list route omits `gene_symbols` / `feature_names` because at ~135k rows they dwarf everything else — so the expandable row in Task 11 needs somewhere to fetch one entry's genes from.

First, the failing test — append to `tests/testthat/test-rummagene-catalog.R`:

```r
test_that("api.R declares a catalog entry route that can serve one entry's genes", {
  api <- base::paste(base::readLines(testthat::test_path("../../api/api.R")), collapse = "\n")
  expect_match(api, "@get /rummagene/catalog/entry", fixed = TRUE)
})
```

Then append to `api/api.R`:

```r
#* One Rummagene catalog entry, including its gene list. Separate from
#* /rummagene/catalog because the gene columns are large and only a detail view
#* needs them -- shipping them with every page of a 135k-row catalog would
#* dominate the response.
#* @param api_key
#* @param term The exact Rummagene term
#' @get /rummagene/catalog/entry
rummagene_catalog_entry_route <- function(res, api_key = "", term = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  term_value <- json_scalar(term)
  if (!base::nzchar(term_value)) {
    return(json_error(res, 400, "Provide the `term` of the catalog entry."))
  }

  conn <- db_connect_local()
  base::on.exit(DBI::dbDisconnect(conn), add = TRUE)

  entry <- get_rummagene_catalog_entry(conn, term_value)
  if (base::is.null(entry)) {
    return(json_error(res, 404, "No Rummagene catalog entry with that term."))
  }

  json_response(res, 200, payload = entry)
}
```

- [ ] **Step 5: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 26 tests.

- [ ] **Step 6: Verify the API still boots and both routes answer**

```bash
docker restart sigrepo-local-api
curl -s "http://localhost:8000/rummagene/catalog?api_key=$LOCAL_API_KEY&limit=2" | head -c 400
curl -s "http://localhost:8000/rummagene/catalog/entry?api_key=$LOCAL_API_KEY&term=PMC1-t.xlsx-keep" | head -c 400
```

Expected: the first returns JSON with `count` and `rows` and **no** gene columns; the second returns one entry **with** `gene_symbols` and `feature_names`. Neither is a 404 or a startup error.

- [ ] **Step 7: Commit**

```bash
git add api/api.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: GET /rummagene/catalog and /rummagene/catalog/entry"
```

---

### Task 9: `POST /rummagene/pull`

**Files:**
- Modify: `api/lib/rummagene_catalog.R`
- Modify: `api/api.R`
- Test: `tests/testthat/test-rummagene-catalog.R`

**Interfaces:**
- Consumes: `get_rummagene_catalog_entry()` from Task 6, `rummagene_signature_name()` from Task 1, `upload_signature()` from `create_signature.R`
- Produces:
  - `rummagene_catalog_omic_signature(entry)` → an `OmicSignature`
  - `POST /rummagene/pull` returning `{ signature_hashkey }`

- [ ] **Step 1: Write the failing tests**

```r
test_that("rummagene_catalog_omic_signature builds a signature keyed by Ensembl id", {
  # feature_name must be the Ensembl ids, because that is what
  # create_signature.R's resolve_feature_ids() hashes and looks up. Handing it
  # symbols is exactly the failure that makes 0% of Rummagene sets uploadable.
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  entry <- base::list(
    term = "PMC1-t.xlsx-x", pmcid = "PMC1", title = "A paper", year = 2020L,
    description = "d", organism = "Homo sapiens", assay_type = "transcriptomics",
    mesh_evidence = "Humans, Transcriptome",
    gene_symbols  = c("TP53", "MYC"),
    feature_names = c("ensg00000141510", "ensg00000136997")
  )
  os <- base::suppressWarnings(rummagene_catalog_omic_signature(entry))

  expect_s3_class(os, "OmicSignature")
  expect_setequal(os$signature$feature_name, c("ensg00000141510", "ensg00000136997"))
  expect_equal(os$metadata$phenotype, "unknown")
  expect_equal(os$metadata$direction_type, "uni-directional")
  expect_equal(os$metadata$organism, "Homo sapiens")
  expect_false("group_label" %in% base::colnames(os$signature))
})

test_that("rummagene_catalog_omic_signature records provenance including the untruncated term", {
  testthat::skip_if_not(requireNamespace("OmicSignature", quietly = TRUE), "OmicSignature not installed")

  long_term <- base::paste0("PMC1-", base::paste(base::rep("x", 400), collapse = ""))
  entry <- base::list(
    term = long_term, pmcid = "PMC1", title = "t", year = 2020L, description = "d",
    organism = "Homo sapiens", assay_type = "transcriptomics",
    mesh_evidence = "Humans, Transcriptome",
    gene_symbols = c("TP53", "MYC"), feature_names = c("ensg00000141510", "ensg00000136997")
  )
  os <- base::suppressWarnings(rummagene_catalog_omic_signature(entry))

  expect_lte(base::nchar(os$metadata$signature_name), 255)
  expect_match(os$metadata$others, long_term, fixed = TRUE)
  expect_match(os$metadata$others, "MeSH", fixed = TRUE)
})

test_that("api.R declares the rummagene pull route", {
  api <- base::paste(base::readLines(testthat::test_path("../../api/api.R")), collapse = "\n")
  expect_match(api, "@post /rummagene/pull", fixed = TRUE)
})
```

- [ ] **Step 2: Run the tests and verify they fail**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: FAIL with `could not find function "rummagene_catalog_omic_signature"`.

- [ ] **Step 3: Implement the builder**

Append to `api/lib/rummagene_catalog.R`:

```r
# A catalog entry -> an OmicSignature ready for upload_signature().
#
# feature_name carries the ENSEMBL IDS, not the symbols: create_signature.R's
# resolve_feature_ids() hashes feature_name and looks it up in
# transcriptomics_features, whose feature_name column is lowercased Ensembl ids.
# Handing it symbols resolves nothing -- measured at 0% -- which is the whole
# reason the catalog stores both namespaces.
rummagene_catalog_omic_signature <- function(entry) {
  provenance <- base::sprintf(
    paste0("source=rummagene; term=%s; pmcid=%s; organism and assay_type from ",
           "PubMed MeSH (%s); phenotype not stated by source"),
    entry$term, entry$pmcid, entry$mesh_evidence
  )

  metadata <- base::list(
    signature_name = rummagene_signature_name(entry$term),
    phenotype      = "unknown",
    organism       = entry$organism,
    direction_type = "uni-directional",
    assay_type     = entry$assay_type,
    description    = entry$description,
    year           = entry$year,
    others         = provenance
  )

  signature <- base::data.frame(
    feature_name = base::as.character(entry$feature_names),
    stringsAsFactors = FALSE
  )

  OmicSignature::OmicSignature$new(metadata = metadata, signature = signature)
}
```

- [ ] **Step 4: Implement the route**

Append to `api/api.R`:

```r
#* Pull one Rummagene catalog entry into this repository as a signature owned by
#* the caller. The signature is private by default and records its Rummagene and
#* MeSH provenance in `others`.
#* @parser json
#* @param api_key
#* @param term The exact Rummagene term, as returned by /rummagene/catalog
#' @post /rummagene/pull
rummagene_pull_route <- function(req, res, api_key = "", term = ""){
  auth <- validate_api_key(res, api_key)
  if (is_json_error(auth)) {
    return(auth)
  }

  term_value <- json_scalar(term)
  if (!base::nzchar(term_value)) {
    return(json_error(res, 400, "Provide the `term` of the catalog entry to pull."))
  }

  conn <- db_connect_local()
  base::on.exit(DBI::dbDisconnect(conn), add = TRUE)

  entry <- get_rummagene_catalog_entry(conn, term_value)
  if (base::is.null(entry)) {
    return(json_error(res, 404, "No Rummagene catalog entry with that term. The catalog may have been rebuilt since you loaded the page."))
  }

  omic_signature <- base::tryCatch(
    base::suppressWarnings(rummagene_catalog_omic_signature(entry)),
    error = function(e) e
  )
  if (base::inherits(omic_signature, "error")) {
    return(json_error(res, 500, base::sprintf("Could not build a signature from that entry: %s", base::conditionMessage(omic_signature))))
  }

  # Deliberately the SAME upload path every other signature takes -- same
  # feature resolution, same rollback, same access grant.
  result <- upload_signature(uploaded = omic_signature, auth = auth, difexp_dir = difexp_dir)
  if (!base::isTRUE(result$ok)) {
    status <- base::switch(result$reason %||% "", duplicate = 409, invalid_upload = 422, unknown_features = 422, 500)
    return(json_error(res, status, result$message))
  }

  json_response(res, 200, payload = base::list(signature_hashkey = result$signature_hashkey))
}
```

Before writing this, read `upload_signature()`'s actual return shape in `api/lib/create_signature.R` and match the field names exactly — this plan assumes `list(ok, reason, message, signature_hashkey)`.

- [ ] **Step 5: Run the tests and verify they pass**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_file("tests/testthat/test-rummagene-catalog.R")'
```

Expected: PASS, 29 tests.

- [ ] **Step 6: End-to-end check against the live local stack**

```bash
docker restart sigrepo-local-api
# seed one catalog row, then:
curl -s -X POST "http://localhost:8000/rummagene/pull" \
  -H 'Content-Type: application/json' \
  -d "{\"api_key\":\"$LOCAL_API_KEY\",\"term\":\"PMC1-t.xlsx-keep\"}"
```

Expected: `{"signature_hashkey":"..."}`, and the signature visible in `/signatures/search`.

- [ ] **Step 7: Commit**

```bash
git add api/lib/rummagene_catalog.R api/api.R tests/testthat/test-rummagene-catalog.R
git commit -m "feat: POST /rummagene/pull"
```

---

### Task 10: Web client functions

**Files:**
- Modify: `web/src/api/client.ts`

**Interfaces:**
- Consumes: the two routes from Tasks 8 and 9
- Produces:
  - `RummageneCatalogRow`, `RummageneCatalogPage`, `RummageneCatalogParams` types
  - `searchRummageneCatalog(params)` → `Promise<RummageneCatalogPage>`
  - `pullRummageneSignature(term)` → `Promise<{ signature_hashkey: string }>`

- [ ] **Step 1: Implement, following `searchSignaturesPage`'s established shape**

Append to `web/src/api/client.ts`:

```ts
export interface RummageneCatalogRow {
  rummagene_catalog_id: number;
  term: string;
  pmcid: string;
  pmid: string | null;
  title: string | null;
  year: number | null;
  doi: string | null;
  description: string | null;
  organism: string;
  assay_type: string;
  mesh_evidence: string;
  n_genes: number;
}

export type RummageneCatalogSortKey =
  | "term" | "title" | "year" | "n_genes" | "organism" | "assay_type";

export interface RummageneCatalogParams {
  q?: string;
  organism?: string;
  assay_type?: string;
  year_min?: number;
  year_max?: number;
  n_genes_min?: number;
  n_genes_max?: number;
  limit?: number;
  offset?: number;
  sortBy?: RummageneCatalogSortKey;
  sortDir?: "asc" | "desc";
}

export interface RummageneCatalogPage {
  rows: RummageneCatalogRow[];
  total: number;
}

// Server-side paged, exactly like searchSignaturesPage: the client holds one
// page, so sorting and filtering must happen server-side or they would only
// reorder the visible page and look like they had sorted the whole catalog.
export async function searchRummageneCatalog(
  params: RummageneCatalogParams = {}
): Promise<RummageneCatalogPage> {
  const query = new URLSearchParams({ api_key: requireApiKey() });
  if (params.q) query.set("q", params.q);
  if (params.organism) query.set("organism", params.organism);
  if (params.assay_type) query.set("assay_type", params.assay_type);
  if (params.year_min != null) query.set("year_min", String(params.year_min));
  if (params.year_max != null) query.set("year_max", String(params.year_max));
  if (params.n_genes_min != null) query.set("n_genes_min", String(params.n_genes_min));
  if (params.n_genes_max != null) query.set("n_genes_max", String(params.n_genes_max));
  if (params.limit != null) query.set("limit", String(params.limit));
  if (params.offset != null) query.set("offset", String(params.offset));
  if (params.sortBy) query.set("sort_by", params.sortBy);
  if (params.sortDir) query.set("sort_dir", params.sortDir);

  const raw = await apiFetch<{ count: number; rows: RummageneCatalogRow[] }>(
    `/rummagene/catalog?${query.toString()}`
  );
  return { rows: raw.rows ?? [], total: Number(raw.count) || 0 };
}

export interface RummageneCatalogEntry extends RummageneCatalogRow {
  gene_symbols: string[];
  feature_names: string[];
}

// Fetched only when a row is expanded. The list endpoint omits the gene columns
// on purpose: at ~135k rows they would dominate every page response.
export async function getRummageneCatalogEntry(term: string): Promise<RummageneCatalogEntry> {
  const query = new URLSearchParams({ api_key: requireApiKey(), term });
  return apiFetch<RummageneCatalogEntry>(`/rummagene/catalog/entry?${query.toString()}`);
}

export async function pullRummageneSignature(
  term: string
): Promise<{ signature_hashkey: string }> {
  return apiFetch<{ signature_hashkey: string }>("/rummagene/pull", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ api_key: requireApiKey(), term }),
  });
}
```

Check `apiFetch`'s existing POST call sites first and match how they pass `method`, `headers`, and `body` — this plan assumes the standard `fetch` init shape.

- [ ] **Step 2: Verify it type-checks**

```bash
cd web && npm run build
```

Expected: no TypeScript errors.

- [ ] **Step 3: Commit**

```bash
git add web/src/api/client.ts
git commit -m "feat: Rummagene catalog client functions"
```

---

### Task 11: The catalog page

**Files:**
- Create: `web/src/pages/RummagenePage.tsx`
- Modify: `web/src/App.tsx:51` (add the route)
- Modify: `web/src/components/Sidebar.tsx:16-24` (add the nav entry)
- Modify: `web/src/App.css` (append styles)

**Interfaces:**
- Consumes: `searchRummageneCatalog()`, `pullRummageneSignature()` from Task 10
- Produces: the `/rummagene` page

- [ ] **Step 1a: Extract the shared term tidier**

`RummagenePanel.tsx` already has `tidyTerm()`. Two copies would drift, so move it to `web/src/lib/rummagene.ts`:

```ts
// Rummagene terms look like
// "PMC6819084-elife-47013-supp2.xlsx-IPA_mono_upstream-...". Trim the PMC id
// and the source filename into something a person can read.
export function tidyTerm(term: string): string {
  const parts = term.split("-");
  return parts.length > 2
    ? parts.slice(2).join(" ").replace(/_/g, " ")
    : term.replace(/_/g, " ");
}
```

Update `RummagenePanel.tsx` to import it and delete its local copy.

- [ ] **Step 1b: Create the page**

Create `web/src/pages/RummagenePage.tsx`:

```tsx
import { useCallback, useEffect, useState } from "react";
import { Link } from "react-router-dom";
import { BookOpen, Download, ExternalLink } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import { tidyTerm } from "../lib/rummagene";
import {
  searchRummageneCatalog,
  getRummageneCatalogEntry,
  pullRummageneSignature,
  ApiError,
  type RummageneCatalogRow,
  type RummageneCatalogEntry,
  type RummageneCatalogSortKey,
} from "../api/client";

const PAGE_SIZE = 25;

type PullState =
  | { status: "idle" }
  | { status: "pulling" }
  | { status: "done"; hashkey: string }
  | { status: "error"; message: string };

export default function RummagenePage() {
  const [rows, setRows] = useState<RummageneCatalogRow[]>([]);
  const [total, setTotal] = useState(0);
  const [offset, setOffset] = useState(0);
  const [loading, setLoading] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // Filters. Kept separate from the committed query so typing does not fire a
  // request per keystroke against a 135k-row table.
  const [draftQ, setDraftQ] = useState("");
  const [q, setQ] = useState("");
  const [yearMin, setYearMin] = useState("");
  const [genesMin, setGenesMin] = useState("");

  const [sortBy, setSortBy] = useState<RummageneCatalogSortKey>("year");
  const [sortDir, setSortDir] = useState<"asc" | "desc">("desc");

  const [expanded, setExpanded] = useState<string | null>(null);
  const [entry, setEntry] = useState<RummageneCatalogEntry | null>(null);
  const [pulls, setPulls] = useState<Record<string, PullState>>({});

  const load = useCallback(async () => {
    setLoading(true);
    setError(null);
    try {
      const page = await searchRummageneCatalog({
        q: q || undefined,
        year_min: yearMin ? Number(yearMin) : undefined,
        n_genes_min: genesMin ? Number(genesMin) : undefined,
        limit: PAGE_SIZE,
        offset,
        sortBy,
        sortDir,
      });
      setRows(page.rows);
      setTotal(page.total);
    } catch (err) {
      setRows([]);
      setError(err instanceof ApiError ? err.message : "Could not load the catalog.");
    } finally {
      setLoading(false);
    }
  }, [q, yearMin, genesMin, offset, sortBy, sortDir]);

  useEffect(() => {
    void load();
  }, [load]);

  // Sorting is server-side: the client holds one page, so sorting here would
  // reorder 25 rows and look like it had sorted the whole catalog.
  function toggleSort(key: RummageneCatalogSortKey) {
    if (key === sortBy) {
      setSortDir((d) => (d === "asc" ? "desc" : "asc"));
    } else {
      setSortBy(key);
      setSortDir("asc");
    }
    setOffset(0);
  }

  async function toggleExpand(term: string) {
    if (expanded === term) {
      setExpanded(null);
      setEntry(null);
      return;
    }
    setExpanded(term);
    setEntry(null);
    try {
      setEntry(await getRummageneCatalogEntry(term));
    } catch {
      setEntry(null);
    }
  }

  async function pull(term: string) {
    setPulls((p) => ({ ...p, [term]: { status: "pulling" } }));
    try {
      const res = await pullRummageneSignature(term);
      setPulls((p) => ({ ...p, [term]: { status: "done", hashkey: res.signature_hashkey } }));
    } catch (err) {
      // 409 is the ordinary "you already pulled this" case, not a failure worth
      // showing as a raw API error.
      const message =
        err instanceof ApiError && err.status === 409
          ? "You already have this signature."
          : err instanceof ApiError
            ? err.message
            : "Pull failed.";
      setPulls((p) => ({ ...p, [term]: { status: "error", message } }));
    }
  }

  return (
    <>
      <PageHeader
        title="Rummagene"
        subtitle={`${total.toLocaleString()} literature-mined gene sets ready to pull`}
      />

      <p className="cell-sub connections-intro">
        These are gene sets mined from the supplementary tables of published papers.
        Organism and assay type come from the paper&rsquo;s PubMed MeSH indexing, and every
        gene resolves in this repository &mdash; but whether a given table is a
        differential-expression contrast is not something we can verify. Read the source
        before pulling.
      </p>

      <Card>
        <div className="rmg-filters">
          <input
            type="search"
            placeholder="Search term, title or description"
            value={draftQ}
            onChange={(e) => setDraftQ(e.target.value)}
            onKeyDown={(e) => {
              if (e.key === "Enter") {
                setQ(draftQ);
                setOffset(0);
              }
            }}
          />
          <input
            type="number"
            placeholder="Year from"
            value={yearMin}
            onChange={(e) => {
              setYearMin(e.target.value);
              setOffset(0);
            }}
          />
          <input
            type="number"
            placeholder="Min genes"
            value={genesMin}
            onChange={(e) => {
              setGenesMin(e.target.value);
              setOffset(0);
            }}
          />
        </div>

        {error && <p className="cell-sub">{error}</p>}

        <table className="rmg-table">
          <thead>
            <tr>
              <th onClick={() => toggleSort("term")}>Gene set</th>
              <th onClick={() => toggleSort("title")}>Paper</th>
              <th onClick={() => toggleSort("year")}>Year</th>
              <th onClick={() => toggleSort("n_genes")}>Genes</th>
              <th>Source</th>
              <th />
            </tr>
          </thead>
          <tbody>
            {rows.map((r) => {
              const state = pulls[r.term] ?? { status: "idle" };
              return (
                <>
                  <tr key={r.term}>
                    <td>
                      <button className="rmg-term" onClick={() => void toggleExpand(r.term)}>
                        {tidyTerm(r.term)}
                      </button>
                    </td>
                    <td>{r.title ?? "—"}</td>
                    <td>{r.year ?? "—"}</td>
                    <td>{r.n_genes}</td>
                    <td>
                      <a
                        href={`https://www.ncbi.nlm.nih.gov/pmc/articles/${r.pmcid}/`}
                        target="_blank"
                        rel="noreferrer"
                      >
                        {r.pmcid} <ExternalLink size={12} />
                      </a>
                    </td>
                    <td>
                      {state.status === "done" ? (
                        <Link to={`/signatures/${state.hashkey}`}>View signature</Link>
                      ) : (
                        <button
                          disabled={state.status === "pulling"}
                          onClick={() => void pull(r.term)}
                        >
                          <Download size={14} />
                          {state.status === "pulling" ? "Pulling…" : "Pull"}
                        </button>
                      )}
                      {state.status === "error" && (
                        <span className="cell-sub">{state.message}</span>
                      )}
                    </td>
                  </tr>
                  {expanded === r.term && (
                    <tr key={`${r.term}-detail`}>
                      <td colSpan={6}>
                        <p className="cell-sub">
                          <BookOpen size={12} /> Attested by MeSH: {r.mesh_evidence}
                          {" · "}
                          {r.organism} {" · "} {r.assay_type}
                        </p>
                        {r.description && <p className="cell-sub">{r.description}</p>}
                        <p className="rmg-genes">
                          {entry ? entry.gene_symbols.join(", ") : "Loading genes…"}
                        </p>
                      </td>
                    </tr>
                  )}
                </>
              );
            })}
          </tbody>
        </table>

        <div className="rmg-pager">
          <button disabled={offset === 0 || loading} onClick={() => setOffset((o) => Math.max(0, o - PAGE_SIZE))}>
            Previous
          </button>
          <span className="cell-sub">
            {total === 0 ? "0" : `${offset + 1}–${Math.min(offset + PAGE_SIZE, total)}`} of{" "}
            {total.toLocaleString()}
          </span>
          <button
            disabled={offset + PAGE_SIZE >= total || loading}
            onClick={() => setOffset((o) => o + PAGE_SIZE)}
          >
            Next
          </button>
        </div>
      </Card>
    </>
  );
}
```

Before writing this, open `SignaturesPage.tsx` and check two things: whether `PageHeader` takes `subtitle` under that name, and whether `ApiError` exposes `status`. If this codebase already wraps paged tables in `DataTable`, use that component instead of the raw `<table>` above and keep the handlers as written — the sorting, paging, expand, and pull logic is what matters and is independent of the table component.

- [ ] **Step 2: Register the route**

In `web/src/App.tsx`, after the `/browse` line:

```tsx
          <Route path="/rummagene" element={<RummagenePage />} />
```

with the matching import at the top of the file.

- [ ] **Step 3: Add the nav entry**

In `web/src/components/Sidebar.tsx`, in the `NAV` array after the `/browse` entry:

```tsx
  { to: "/rummagene", label: "Rummagene", icon: Library },
```

Import `Library` from `lucide-react` alongside the existing icons.

- [ ] **Step 3b: Add the styles**

Append to `web/src/App.css`. Every token used here is one this stylesheet actually defines — `--mono`, `--text`, `--surface-hover`, `--table-stripe`. Do **not** reach for `--font-mono`, `--text-primary`, or `--surface-2`: they are undefined, and an unresolved `var()` with no fallback on an inherited property silently renders black text or the default font.

```css
/* Rummagene catalog browse page */
.rmg-filters {
  display: flex;
  gap: 12px;
  flex-wrap: wrap;
  margin-bottom: 16px;
}

.rmg-filters input[type="search"] {
  flex: 1 1 280px;
}

.rmg-table {
  width: 100%;
  border-collapse: collapse;
}

/* Sorting is server-side, so every header is a control. */
.rmg-table thead th {
  cursor: pointer;
  user-select: none;
  text-align: left;
  white-space: nowrap;
}

.rmg-table tbody tr:nth-child(even) {
  background: var(--table-stripe);
}

.rmg-table td {
  /* Uniform row height: long paper titles must not wrap the row taller than
     its neighbours -- the non-uniform-rows bug PR #52 fixed. */
  max-width: 320px;
  overflow: hidden;
  text-overflow: ellipsis;
  white-space: nowrap;
}

.rmg-term {
  background: none;
  border: none;
  padding: 0;
  cursor: pointer;
  text-align: left;
  color: inherit;
  font: inherit;
}

.rmg-term:hover {
  background: var(--surface-hover);
}

.rmg-genes {
  font-family: var(--mono);
  font-size: 12px;
  color: var(--text);
  max-height: 160px;
  overflow-y: auto;
  /* The one place a row is allowed to grow: an expanded gene list wraps. */
  white-space: normal;
  word-break: break-word;
}

.rmg-pager {
  display: flex;
  align-items: center;
  gap: 12px;
  margin-top: 16px;
}
```

- [ ] **Step 4: Verify in the browser**

```bash
docker restart sigrepo-local-web
```

Then open the preview, sign in, and confirm: the nav entry appears; the page loads a page of rows; sorting a column re-queries rather than reordering the page; each filter narrows the result; Pull creates a signature and links to it; pulling the same row twice shows the duplicate message.

- [ ] **Step 5: Check the CSS tokens actually exist**

Any new class must use tokens this codebase defines. `--mono`, `--text`, `--surface-hover`, `--surface-sunken`, `--table-stripe`, `--viz-1`…`--viz-5` are real. `--font-mono`, `--text-primary`, and `--surface-2` are **not defined** — using them silently renders black text or a default font.

```bash
grep -c '{' web/src/App.css && grep -c '}' web/src/App.css
```

Expected: equal counts. An unbalanced brace silently swallows every rule after it — this has broken this stylesheet before.

- [ ] **Step 6: Commit**

```bash
git add web/src/pages/RummagenePage.tsx web/src/App.tsx \
        web/src/components/Sidebar.tsx web/src/App.css
git commit -m "feat: Rummagene catalog browse page"
```

---

### Task 12: Full-suite verification and PR

**Files:** none

- [ ] **Step 1: Run the entire R suite**

```bash
docker exec -i sigrepo-local-api Rscript -e 'setwd("/SigRepo_Server"); library(testthat); testthat::set_max_fails(Inf); testthat::test_dir("tests/testthat", stop_on_failure = FALSE)'
```

Expected: 0 failures. If failures appear, re-run once from a completed state before diagnosing — an aborted earlier run corrupts the shared database and produces phantom failures in unrelated files.

- [ ] **Step 2: Build the web app**

```bash
cd web && npm run build
```

Expected: no TypeScript errors.

- [ ] **Step 3: Run the credential gate**

```bash
git ls-tree -r --name-only HEAD | grep -cE '^\.Renviron|^\.env|^\.local-data/|^\.superpowers/'
```

Expected: `0`. Anything else — stop and remove the file from the branch before pushing.

- [ ] **Step 4: Open the PR**

Use the repo's own `validate-and-ship` skill, which stands up a throwaway stack, runs the validation harness, tears it down, and opens the PR.

---

## Deferred

Recorded so they are not silently lost:

- **Mouse support.** Needs `org.Mm.eg.db` installed, and `updateTranscriptomicsFeatureSet()`'s `hgnc_symbol` changed to `mgi_symbol` for non-human organisms — that is the root cause of the mouse gene-symbol gap, and it is a separate fix with its own blast radius.
- **A curation signal.** MeSH describes the paper, not the table, so a set that is not a differential contrast can still reach the catalog. Whether to add a flag, rating, or review step is deferred until there is usage to learn from.
- **Scheduling the build.** The job is run by hand for now. Where and how it runs weekly — cron on a workstation, CI, or the droplet with a memory budget — is a deployment decision, not a code one.
- **Dev-stack vocabulary seeding.** `.local-data/rebuild_local_db.R` seeds `platforms` only with values the basket uses, so the local stack diverges from production. Worth fixing so local behaviour matches, but unrelated to this feature.
