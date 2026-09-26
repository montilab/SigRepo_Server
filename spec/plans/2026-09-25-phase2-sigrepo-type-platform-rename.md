# Phase 2: SigRepo and SigRepo_Server `type` / `platform` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rename `signatures.direction_type` to `signatures.type` and `platforms.platform_name` to `platforms.platform` in the database, and carry those names through the SigRepo R client, the Plumber API, the MCP server, the legacy Shiny app and the React frontend.

**Architecture:** A metadata-only MySQL column rename plus a mechanical rename across the R and TypeScript layers. Two seams stay deliberately tolerant of the old names: uploaded signature metadata, because users hold files written by older OmicSignature versions, and the `sort_by` query parameter, because stale frontend state would otherwise sort by the wrong column silently. Everything the API emits uses only the new names.

**Tech Stack:** MySQL 8.0, R with Plumber / DBI / dplyr, testthat edition 3, Shiny, React + TypeScript, Docker Compose.

**Spec:** `spec/2026-09-25-type-platform-rename-design.md`

**Prerequisite:** Phase 1 (`spec/plans/2026-09-25-phase1-omicsignature-type-rename.md`) must be merged and released first. The client here reads `metadata$type` from OmicSignature objects.

**Repos:** `SigRepo` and `SigRepo_Server`, both using the `<Repo>/main` + `<Repo>/wt/<issue>` worktree layout. Create one issue per repo on the remote and branch with `gh issue develop`, no `fix/` or `feat/` prefix.

## Global Constraints

- Column names are exactly `type` and `platform`. Table names do not change.
- No stored value changes. `uni-directional`, `bi-directional`, `categorical` and every platform string stay as they are.
- Use `RENAME COLUMN`, never `CHANGE COLUMN`. `signatures.direction_type` is a `SET(...)` and restating that definition from the repo schema file is how data got truncated last time. See `mysql/schema/signatures.sql:9-17`.
- MySQL 8.0 or later is required for `RENAME COLUMN`.
- Client function arguments are a hard rename. Do not add deprecated `platform_name` arguments.
- API responses emit only `type` and `platform`. No dual keys.
- API *inputs* are tolerant: uploaded metadata and `sort_by` accept either spelling.
- `SIGREPO_TEST_*` environment variables must point at the local stack before any `R CMD check` or `devtools::check()`, because check builds vignettes that add and delete signatures through a connection handler that otherwise defaults to `sigrepo.org`.
- Wrap `testServer()` calls in `later::with_temp_loop` or the server suite hangs on leaked pools.
- An aborted `test_dir` run leaves state that makes the next run report failures that are not real. Always rerun clean before treating red as a regression.
- Do not run the migration against `sigrepo.org` or `montilab.bu.edu`. Local stack only.

## Review Focus

1. A signature uploaded as an OmicSignature object built by the *pre-phase-1* package carries `direction_type` in its metadata and must still upload successfully. Covered in Task 6.
2. An `.rds` produced by `/signatures/export` carries `platform_name` rather than `platform`, and must still upload once the `platform_field` indirection is removed. Covered in Task 6.
3. A stale frontend or bookmarked URL sending `sort_by=direction_type` currently falls through to sorting by `signature_name` silently, which looks like a broken sort rather than a rejected parameter. Covered in Task 7.
4. A fresh install reads `mysql/data/platforms.csv`'s header straight into `addPlatform()`, whose required column name is changing in the other repo. If the two disagree, install breaks. Covered in Task 2 and verified in Task 10.
5. Running the migration twice, or against an already-migrated database, must be a clean no-op rather than a half-applied failure. Covered in Task 1.

---

### Task 1: Migration scripts

**Files:**
- Create: `mysql/migrations/2026-09-25-rename-type-platform.sql`
- Create: `mysql/migrations/2026-09-25-rename-type-platform-rollback.sql`
- Create: `mysql/migrations/README.md`

**Interfaces:**
- Consumes: nothing.
- Produces: two idempotent SQL scripts runnable via `mysql < file`.

- [ ] **Step 1: Write the forward migration**

Create `mysql/migrations/2026-09-25-rename-type-platform.sql`:

```sql
-- Rename signatures.direction_type -> signatures.type
-- Rename platforms.platform_name  -> platforms.platform
--
-- Requires MySQL 8.0 or later for RENAME COLUMN.
--
-- RENAME COLUMN is used deliberately instead of CHANGE COLUMN.
-- signatures.direction_type is a SET(...) column, and CHANGE COLUMN would
-- require restating that definition. The repo schema file and the deployed
-- schema have drifted on exactly such a definition before, truncating data
-- (see the comment in mysql/schema/signatures.sql). RENAME COLUMN needs no
-- type restatement, so that failure mode is impossible here.
--
-- This is a metadata-only, in-place operation: no table rebuild, no row copy,
-- and duration is independent of row count. Indexes follow the renamed column
-- automatically, so the UNIQUE constraint on platforms needs no separate
-- statement. No foreign key references either column; they reference
-- platform_id.
--
-- Each rename is guarded so re-running this file is a no-op.

SET @has_direction_type := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'signatures'
    AND COLUMN_NAME = 'direction_type'
);
SET @sql := IF(
  @has_direction_type > 0,
  'ALTER TABLE `signatures` RENAME COLUMN `direction_type` TO `type`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;

SET @has_platform_name := (
  SELECT COUNT(*) FROM information_schema.COLUMNS
  WHERE TABLE_SCHEMA = DATABASE()
    AND TABLE_NAME = 'platforms'
    AND COLUMN_NAME = 'platform_name'
);
SET @sql := IF(
  @has_platform_name > 0,
  'ALTER TABLE `platforms` RENAME COLUMN `platform_name` TO `platform`',
  'DO 0'
);
PREPARE stmt FROM @sql;
EXECUTE stmt;
DEALLOCATE PREPARE stmt;
```

- [ ] **Step 2: Write the rollback migration**

Create `mysql/migrations/2026-09-25-rename-type-platform-rollback.sql` with the same guarded structure, reversed: guard on `type` existing in `signatures` and rename it back to `direction_type`; guard on `platform` existing in `platforms` and rename it back to `platform_name`. Also metadata-only.

- [ ] **Step 3: Write the runbook**

Create `mysql/migrations/README.md` covering: that migrations are applied by hand and in order, the exact `docker exec ... mysql` invocation for the local stack, the requirement to take a `mysqldump` of `signatures` and `platforms` before running on `montilab.bu.edu` or `sigrepo.org`, that the API container must be restarted after the rename because its code expects the new names, and that these two repos' code and this migration must move together per environment.

- [ ] **Step 4: Verify idempotency against the local stack**

Apply the migration twice in a row and confirm the second run changes nothing:

```bash
docker exec -i sigrepo-local-mysql mysql -u"$MYSQL_USER" -p"$MYSQL_PASSWORD" sigrepo \
  < mysql/migrations/2026-09-25-rename-type-platform.sql
docker exec -i sigrepo-local-mysql mysql -u"$MYSQL_USER" -p"$MYSQL_PASSWORD" sigrepo \
  < mysql/migrations/2026-09-25-rename-type-platform.sql
docker exec -i sigrepo-local-mysql mysql -u"$MYSQL_USER" -p"$MYSQL_PASSWORD" sigrepo -e \
  "SELECT COLUMN_NAME FROM information_schema.COLUMNS
   WHERE TABLE_SCHEMA='sigrepo' AND TABLE_NAME IN ('signatures','platforms')
     AND COLUMN_NAME IN ('type','platform','direction_type','platform_name');"
```

Read the credentials from the stack's compose env file rather than guessing; the `root` password in this environment is not the obvious one.

Expected: both runs succeed, and the final query returns exactly `type` and `platform`.

Then confirm no data moved:

```bash
docker exec -i sigrepo-local-mysql mysql -u"$MYSQL_USER" -p"$MYSQL_PASSWORD" sigrepo -e \
  "SELECT type, COUNT(*) FROM signatures GROUP BY type;"
```

Expected: the same counts per direction value as before the migration. Capture those counts *before* running step 4 so there is something to compare against.

- [ ] **Step 5: Verify rollback, then re-apply forward**

Run the rollback, confirm the columns are back to `direction_type` and `platform_name` with unchanged counts, then run the forward migration again to leave the local stack migrated.

- [ ] **Step 6: Commit**

```bash
git add mysql/migrations
git commit -m "Add migration renaming signatures.direction_type to type and platforms.platform_name to platform"
```

---

### Task 2: Schema files, seed data and test fixtures

**Files:**
- Modify: `mysql/schema/signatures.sql:8`
- Modify: `mysql/schema/platforms.sql:6,8`
- Modify: `mysql/data/platforms.csv` (header line only)
- Modify: `tests/testthat/fixtures/seed.sql:20,24,32,51,59,70`

**Interfaces:**
- Consumes: nothing.
- Produces: a schema that creates fresh databases with the new column names, and a seed fixture that populates them.

**Why this matters more than it looks:** `mysql/data/platforms.csv`'s header is read by `read.csv()` and passed straight into `SigRepo::addPlatform()` at `api/lib/database_admin.R:126-127`, whose required column name changes in Task 3, in the other repo. If only one side moves, unit tests still pass and a fresh install breaks.

- [ ] **Step 1: Update the schema files**

In `mysql/schema/signatures.sql`, change the column name on line 8 from `direction_type` to `type`, leaving the `SET(...)` definition and the `NOT NULL` exactly as they are.

In `mysql/schema/platforms.sql`, change `platform_name` to `platform` on both line 6 and the `UNIQUE` constraint on line 8.

- [ ] **Step 2: Update the seed CSV header**

Change the first line of `mysql/data/platforms.csv` from `platform_name` to `platform`. Do not touch any other line; the file's remaining lines are platform values.

Verify only the header changed:

```bash
head -2 mysql/data/platforms.csv
wc -l mysql/data/platforms.csv
```

- [ ] **Step 3: Update the test seed fixture**

In `tests/testthat/fixtures/seed.sql`, change `platform_name` to `platform` on lines 20, 32, 59 and 70, and `direction_type` to `type` in the two insert column lists on lines 24 and 51.

Note line 24 and 51 are multi-line `INSERT ... (col, col, ...)` lists; change only the column name, not the value ordering.

- [ ] **Step 4: Verify no stale names remain in schema or fixtures**

Run: `grep -rn "direction_type\|platform_name" mysql/ tests/testthat/fixtures/`
Expected: matches only inside `mysql/migrations/` (the migration scripts must name the old columns) and inside the explanatory comment in `mysql/schema/signatures.sql`.

- [ ] **Step 5: Commit**

```bash
git add mysql/schema mysql/data/platforms.csv tests/testthat/fixtures/seed.sql
git commit -m "Rename type and platform columns in schema, seed data and test fixtures"
```

---

### Task 3: SigRepo client platform argument renames

**Repo:** `SigRepo`

**Files:**
- Modify: `R/searchSignature.R` (argument, roxygen, `lookup_table_sql` column strings)
- Modify: `R/searchPlatform.R` (argument, roxygen, examples)
- Modify: `R/deletePlatform.R` (argument, roxygen, examples)
- Modify: `R/addPlatform.R:5,15,58,92` (required column, roxygen, example)
- Modify: `R/getSignature.R:150,165`
- Modify: `R/createOmicCollection.R:103,118`
- Modify: `R/createSignatureMetadata.R:134,135`
- Test: `tests/testthat/test_platforms.R`, `tests/testthat/test_signatures.R`, `tests/testthat/test_lookup_table_sql_db.R`

**Interfaces:**
- Consumes: the migrated local database from Task 1.
- Produces: `searchSignature(conn_handler, signature_id, signature_name, user_name, organism, phenotype, sample_type, platform, verbose)`; `searchPlatform(conn_handler, platform, verbose)`; `deletePlatform(conn_handler, platform, verbose)`; `addPlatform(conn_handler, platform_tbl, verbose)` where `platform_tbl` requires a column named `platform`.

- [ ] **Step 1: Run the platform tests to capture the starting state**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_platforms.R")'`
Expected: PASS before any edit, against the already-migrated local database only if the client still matched the old schema. Since Task 1 already renamed the columns, expect FAIL here with an unknown column error. That failure is the signal that the client is now the thing out of date.

- [ ] **Step 2: Rename the arguments**

In `searchSignature.R`, `searchPlatform.R` and `deletePlatform.R`, rename the formal argument `platform_name` to `platform`, and update each `@param platform_name` roxygen line to `@param platform` along with any `platform_name =` usage inside the roxygen `@examples` block.

In each function body, update the `lookup_table_sql()` calls so the column name strings `"platform_name"` become `"platform"`. These are database column names passed as strings, so they must match the migrated schema.

- [ ] **Step 3: Rename `addPlatform()`'s required column**

In `addPlatform.R`, change `required_column_fields <- "platform_name"` on line 58 to `"platform"`, the `coln_var = "platform_name"` on line 92 to `"platform"`, and the roxygen on lines 4-5 and the example on line 15 so the documented input column is `platform`.

- [ ] **Step 4: Make the joined-name vectors uniform**

In `getSignature.R:150` and `createOmicCollection.R:103`, change `return_var = c("platform_id", "platform_name")` to `c("platform_id", "platform")`.

In `getSignature.R:165` and `createOmicCollection.R:118`, change the replacement vector `c("organism", "phenotype", "sample_type", "platform_name")` to `c("organism", "phenotype", "sample_type", "platform")`. All four entries are now bare nouns, which is the point.

In `createSignatureMetadata.R:134-135`, change `filter_coln_var = "platform_name"` and the `filter_coln_val` list key to `"platform"`.

- [ ] **Step 5: Run the tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_platforms.R")'`
Then: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_lookup_table_sql_db.R")'`

Update the two to three `platform_name` occurrences inside those test files to `platform` as you go; they are asserting on column names and argument names that just changed.

Expected: PASS. If an aborted earlier run left state, rerun clean before believing any failure.

- [ ] **Step 6: Commit**

```bash
git add R man tests
git commit -m "Rename client platform_name arguments and column references to platform"
```

---

### Task 4: SigRepo client exported symbols and the stale object guard

**Repo:** `SigRepo`

**Files:**
- Modify: `R/direction_types.R` (renamed content; consider renaming the file to `R/signature_types.R`)
- Modify: `R/checkFunctions.R:369-380` (the check function) and `checkOmicSignature()` at `R/checkFunctions.R:389`
- Modify: `NAMESPACE` (regenerated)
- Delete: `man/direction_types.Rd`, `man/checkDirectionType.Rd` (regenerated under new names)
- Test: `tests/testthat/test_checkFunctions.R`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `SigRepo::signature_types`, a character vector `c("uni-directional", "bi-directional", "categorical")`. `SigRepo::checkSignatureType(type)`, which stops when `type` is not one of those. `checkOmicSignature()` gains a stale-object check.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test_checkFunctions.R`:

```r
test_that("checkSignatureType() accepts valid types and rejects others", {
  expect_silent(checkSignatureType("uni-directional"))
  expect_silent(checkSignatureType("BI-DIRECTIONAL"))
  expect_error(checkSignatureType("sideways"), "'type' must be one of the following options")
})

test_that("signature_types exposes the three supported values", {
  expect_setequal(SigRepo::signature_types, c("uni-directional", "bi-directional", "categorical"))
})

test_that("checkOmicSignature() rejects an object built by a pre-rename OmicSignature", {
  ## Simulates an object deserialized from an RDS written before the metadata
  ## field was renamed. It never passes through OmicSignature's normalizer, so
  ## metadata$type is NULL and every downstream read would silently misbehave.
  stale <- structure(
    list(metadata = list(signature_name = "stale", direction_type = "uni-directional")),
    class = c("OmicSignature", "R6")
  )
  expect_error(
    checkOmicSignature(stale),
    "reinstall OmicSignature"
  )
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_checkFunctions.R")'`
Expected: FAIL with `could not find function "checkSignatureType"`.

- [ ] **Step 3: Rename the data object and the check function**

Rename `R/direction_types.R` to `R/signature_types.R` with content:

```r
#' Exported signature types
#'
#' The signature types supported by SigRepo. Named `signature_types` rather
#' than `types` because a bare `types` object is too generic to read at a
#' call site.
#'
#' @keywords internal
#'
#' @export
signature_types <- c("uni-directional", "bi-directional", "categorical")
```

In `R/checkFunctions.R`, replace `checkDirectionType()` with:

```r
checkSignatureType <- function(
    type
){

  # Get signature type options
  signature_type_options <- SigRepo::signature_types |> base::tolower() |> base::trimws()

  # Return message
  if(!base::trimws(base::tolower(type[1])) %in% signature_type_options)
    base::stop(base::sprintf("'type' must be one of the following options: %s", base::paste0(signature_type_options, collapse = "/")))

}
```

Keep its roxygen block, renaming `@title`, `@description` and `@param` to match. Update every caller of `checkDirectionType()` and every reference to `SigRepo::direction_types` across `R/`.

- [ ] **Step 4: Add the stale object guard**

In `checkOmicSignature()`, after its existing validation that the input is an OmicSignature R6 object, add:

```r
  # An OmicSignature deserialized from an RDS written before the metadata
  # field rename never passes through the package's own normalizer, so
  # metadata$type is NULL. Fail here with something actionable rather than
  # letting a NULL propagate into SQL and surface as a confusing insert error.
  if(base::is.null(omic_signature$metadata$type) &&
     !base::is.null(omic_signature$metadata$direction_type)){
    base::stop(
      "This OmicSignature object uses the retired metadata field 'direction_type'. ",
      "Please reinstall OmicSignature (>= 1.4.0) and rebuild the object so its ",
      "metadata uses 'type'."
    )
  }
```

- [ ] **Step 5: Regenerate docs and run tests**

Run: `Rscript -e 'devtools::document()'`
Then: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test_checkFunctions.R")'`

Delete the now-orphaned `man/direction_types.Rd` and `man/checkDirectionType.Rd` if `document()` did not remove them.

Expected: PASS, and `NAMESPACE` exports `signature_types` and `checkSignatureType` in place of the old names.

- [ ] **Step 6: Commit**

```bash
git add R man NAMESPACE tests
git commit -m "Rename direction_types to signature_types and add stale OmicSignature guard"
```

---

### Task 5: SigRepo client metadata read and write paths

**Repo:** `SigRepo`

**Files:**
- Modify: `R/createOmicSignature.R:87,93,123,131,438,444,450`
- Modify: `R/createSignatureMetadata.R:53,54,322`
- Modify: `R/hypeR_examples.R:1241,1249`
- Modify: the roxygen field lists in `R/omic_signature_1.R` through `R/omic_signature_4.R`, `R/LLFS_Aging_Gene_2023.R`, `R/Myc_reduce_mice_liver_24m.R`
- Modify: `vignettes/signature-tutorials.Rmd`
- Test: `tests/testthat/test_signatures.R`, `tests/testthat/test_client_regressions_db.R`, `tests/testthat/test_hypergem_client.R`

**Interfaces:**
- Consumes: `signature_types` and `checkSignatureType()` from Task 4; the migrated schema from Task 1.
- Produces: `buildRetrievedMetadata()` emitting metadata with `type` and `platform`; `sanitizeRetrievedSignature(signature, type)`.

- [ ] **Step 1: Update the metadata construction path**

In `createOmicSignature.R:87`, change `direction_type = db_signature_tbl$direction_type[1]` to `type = db_signature_tbl$type[1]`.

On line 93, change `platform = parseRetrievedMetadataList(db_signature_tbl$platform_name, split_values = FALSE)` to read `db_signature_tbl$platform`. The metadata key stays `platform`, because that is already the OmicSignature field name; only the database column reference changes.

Rename `sanitizeRetrievedSignature()`'s second parameter from `direction_type` to `type` (line 123), update its body (line 131) and its call sites. Update lines 438, 444 and 450 to read `metadata$type`.

- [ ] **Step 2: Update the metadata consumption path**

In `createSignatureMetadata.R:53-54`, change the comment and `direction_type <- metadata$direction_type[1]` to `type <- metadata$type[1]`. On line 322, change the emitted field `direction_type = direction_type` to `type = type`.

Confirm the variable is not shadowed elsewhere in that function before renaming it.

- [ ] **Step 3: Collapse the defensive coalesce**

In `hypeR_examples.R:1241`, change `direction_type = flatten_report_value(metadata$direction_type)` to `type = flatten_report_value(metadata$type)`.

On line 1249, change `platform = flatten_report_value(metadata$platform_name %||% metadata$platform)` to `platform = flatten_report_value(metadata$platform)`. The coalesce existed only because the two layers disagreed; they no longer do.

- [ ] **Step 4: Update the data object documentation**

The six data object roxygen blocks list metadata fields including `direction_type`. Change each to `type`. These are documentation only, but `R CMD check` compares documented fields against the shipped `.rda` objects, so also confirm whether the shipped data objects themselves embed the old key:

```bash
Rscript -e 'load("data/omic_signature_1.rda"); print(names(omic_signature_1$metadata))'
```

If they embed `direction_type`, they must be rebuilt from their `data-raw` builder against the phase-1 OmicSignature package, not hand edited.

- [ ] **Step 5: Run the client suite**

Ensure `SIGREPO_TEST_*` point at the local stack first. Then:

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`

Update `direction_type` occurrences in `test_signatures.R`, `test_client_regressions_db.R` and `test_hypergem_client.R` to `type` as failures surface.

Expected: PASS. Rerun clean if a prior run aborted.

- [ ] **Step 6: Commit**

```bash
git add R man vignettes tests data
git commit -m "Read and emit metadata fields type and platform throughout the client"
```

---

### Task 6: API upload path, tolerant input

**Repo:** `SigRepo_Server`

**Files:**
- Modify: `api/lib/create_signature.R:41` (`REQUIRED_UPLOAD_METADATA_FIELDS`), `:59-84` (`normalize_upload`), `:165-166`, `:412`, `:445`, `:453`
- Test: `tests/testthat/test-create-signature.R`

**Interfaces:**
- Consumes: the migrated schema from Task 1.
- Produces: `normalize_upload(uploaded)` returning `list(ok, metadata, feature_tbl, difexp_tbl, feature_key)`. The `platform_field` element is **removed**, because both accepted upload shapes now use `platform`. `REQUIRED_UPLOAD_METADATA_FIELDS` becomes `c("signature_name", "type", "assay_type", "organism", "phenotype")`.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-create-signature.R`:

```r
test_that("normalize_upload() accepts legacy metadata field names from both shapes", {
  legacy_omics_like <- base::list(
    metadata = base::list(
      signature_name = "legacy_upload",
      direction_type = "uni-directional",
      assay_type = "transcriptomics",
      organism = "Homo sapiens",
      phenotype = "test",
      platform = "unknown"
    ),
    signature = base::data.frame(feature_id = 1L, score = 1)
  )
  norm <- normalize_upload(legacy_omics_like)
  expect_true(norm$ok)
  expect_equal(norm$metadata$type, "uni-directional")
  expect_null(norm$metadata$direction_type)

  export_shape <- base::list(
    metadata = base::list(
      signature_name = "export_upload",
      type = "uni-directional",
      assay_type = "transcriptomics",
      organism = "Homo sapiens",
      phenotype = "test",
      platform_name = "unknown"
    ),
    signature = base::data.frame(feature_id = 1L, score = 1)
  )
  norm2 <- normalize_upload(export_shape)
  expect_true(norm2$ok)
  expect_equal(norm2$metadata$platform, "unknown")
  expect_null(norm2$metadata$platform_name)
})

test_that("normalize_upload() rejects metadata carrying both spellings", {
  conflicting <- base::list(
    metadata = base::list(
      signature_name = "conflict",
      type = "uni-directional",
      direction_type = "bi-directional",
      assay_type = "transcriptomics",
      organism = "Homo sapiens",
      phenotype = "test"
    ),
    signature = base::data.frame(feature_id = 1L, score = 1)
  )
  norm <- normalize_upload(conflicting)
  expect_false(norm$ok)
  expect_match(norm$message, "both")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the server test suite for that file inside the API image or with the API libs sourced, per this repo's usual test invocation. Expected: FAIL, because `normalize_upload()` returns the metadata untouched and still reports `platform_field`.

- [ ] **Step 3: Add the input normalizer and remove `platform_field`**

In `api/lib/create_signature.R`, add above `normalize_upload()`:

```r
# Accept the retired metadata field names on upload.
#
# Deliberately asymmetric with everything this API emits: responses use only
# `type` and `platform`, but uploads must keep working for users holding .rds
# files written by an older OmicSignature, and for /signatures/export output
# written before the platform column was renamed. Strict out, tolerant in.
#
# Returns list(ok = TRUE, metadata = <normalized>) or
# list(ok = FALSE, message = ...) when both spellings are present, which is
# ambiguous rather than merely old.
normalize_upload_metadata_names <- function(metadata) {
  renames <- base::list(direction_type = "type", platform_name = "platform")
  for (old_name in base::names(renames)) {
    new_name <- renames[[old_name]]
    if (!old_name %in% base::names(metadata)) {
      next
    }
    if (new_name %in% base::names(metadata)) {
      return(base::list(ok = FALSE, message = base::sprintf(
        "Uploaded metadata contains both '%s' and the retired '%s'. Keep only '%s'.",
        new_name, old_name, new_name
      )))
    }
    base::names(metadata)[base::names(metadata) == old_name] <- new_name
  }
  base::list(ok = TRUE, metadata = metadata)
}
```

In `normalize_upload()`, route both accepted shapes through it and drop `platform_field` from both returned lists. For the `OmicSignature` branch and the list branch alike:

```r
    normalized <- normalize_upload_metadata_names(uploaded$metadata)
    if (!normalized$ok) {
      return(base::list(ok = FALSE, reason = "invalid_upload", message = normalized$message))
    }
```

then return `metadata = normalized$metadata` and omit `platform_field`. Update the long explanatory comment above `normalize_upload()` so it no longer describes a `platform_field` that does not exist; replace that paragraph with a note that both shapes are normalized to `platform` on entry.

- [ ] **Step 4: Update the field list, reads and insert**

Change line 41 to:

```r
REQUIRED_UPLOAD_METADATA_FIELDS <- c("signature_name", "type", "assay_type", "organism", "phenotype")
```

At line 165, change `direction_type <- meta_str(metadata, "direction_type")` to `type <- meta_str(metadata, "type")` and update the line 166 condition to test `type`.

At line 412, change `platform_value <- meta_str(metadata, norm$platform_field) %||% "unknown"` to `meta_str(metadata, "platform") %||% "unknown"`, and change the `lookup_id(conn, "platforms", "platform_id", "platform_name", platform_value)` column argument to `"platform"`.

At line 445, change the `insert_cols` entry `"direction_type"` to `"type"`. At line 453, change `sql_value(conn, meta_str(metadata, "direction_type"))` to `meta_str(metadata, "type")`. The positional correspondence between `insert_cols` and `insert_vals` must be preserved, so change both in place rather than reordering.

- [ ] **Step 5: Run tests to verify they pass**

Run the `test-create-signature.R` file, then the whole server suite. Update the two `direction_type` and two `platform_name` occurrences already in that test file. Expected: PASS.

- [ ] **Step 6: Commit**

```bash
git add api/lib/create_signature.R tests/testthat/test-create-signature.R
git commit -m "Accept retired metadata names on upload and insert into the renamed columns"
```

---

### Task 7: API read paths, MCP and the `sort_by` alias

**Repo:** `SigRepo_Server`

**Files:**
- Modify: `api/lib/signature.R:58-68` (sort map), `:139` (SELECT), `:221` (return_var)
- Modify: `api/lib/compare.R:67,211`
- Modify: `api/lib/vocabulary.R:26-29`
- Modify: `mcp/lib/queries.R:25-28`
- Modify: `api/api.R:1294-1295` (sort_by docs), and the `direction_type` and `platform_name` occurrences at `api/api.R`'s other match
- Test: `tests/testthat/test-signature.R`, `test-signature-visibility.R`, `test-gene-search.R`, `test-annotate.R`, `test-gem-enrichment.R`, `test-compare` helpers

**Interfaces:**
- Consumes: the migrated schema from Task 1.
- Produces: `.signature_sort_columns` mapping `type` and `platform` to the renamed columns, plus retired-name aliases pointing at the same expressions.

**Note:** the list query uses `SELECT s.*`, so the response key for the direction field changes from `direction_type` to `type` automatically once the migration runs, with no code change. The explicit `pl.platform_name` in the same SELECT does need editing.

- [ ] **Step 1: Write the failing test for the sort alias**

Append to `tests/testthat/test-signature.R`:

```r
test_that("sort_by accepts the retired column names instead of silently sorting by name", {
  ## An unknown sort_by falls through to s.signature_name, so a stale bookmark
  ## or cached frontend sending direction_type would look like a broken sort
  ## rather than a rejected parameter. Keep the old keys as input aliases.
  expect_equal(.signature_sort_columns[["direction_type"]], .signature_sort_columns[["type"]])
  expect_equal(.signature_sort_columns[["platform_name"]], .signature_sort_columns[["platform"]])
  expect_equal(.signature_sort_columns[["type"]], "s.type")
  expect_equal(.signature_sort_columns[["platform"]], "pl.platform")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run the server suite for `test-signature.R`. Expected: FAIL, because the map still has `direction_type = "s.direction_type"` and no `type` key.

- [ ] **Step 3: Update the sort map with aliases**

Replace the map at `api/lib/signature.R:58-68`:

```r
.signature_sort_columns <- base::list(
  signature_name = "s.signature_name",
  organism       = "o.organism",
  assay_type     = "s.assay_type",
  type           = "s.type",
  phenotype      = "p.phenotype",
  sample_type    = "st.sample_type",
  platform       = "pl.platform",
  year           = "s.year",
  user_name      = "s.user_name",
  visibility     = "s.visibility",
  # Retired input names kept as aliases. An unrecognized sort_by falls through
  # to s.signature_name, so without these a stale bookmark or a cached frontend
  # bundle would sort by the wrong column with no error anywhere. Input only:
  # responses never emit these names.
  direction_type = "s.type",
  platform_name  = "pl.platform"
)
```

- [ ] **Step 4: Update the remaining read paths**

At `api/lib/signature.R:139`, change `pl.platform_name` to `pl.platform` in the SELECT list. At line 221, change `return_var = c("platform_id", "platform_name")` to `c("platform_id", "platform")`.

In `api/lib/compare.R`, change the `sig_meta` field at line 211 from `direction_type = ...$metadata$direction_type` to `type = ...$metadata$type`, and update the comment at line 67 to describe `{name, hashkey, type}`.

In `api/lib/vocabulary.R:26-29` and `mcp/lib/queries.R:25-28`, change the SQL to `SELECT DISTINCT pl.platform FROM platforms pl ... ORDER BY pl.platform` and the extraction from `$platform_name` to `$platform`. The R variable and the emitted JSON key are already `platform`, so no response key changes here.

In `api/api.R`, update the `sort_by` documentation at lines 1294-1295 to list `type` and `platform`, and fix the remaining `direction_type` and `platform_name` occurrences in that file.

- [ ] **Step 5: Run the server suite**

Run the full server suite, wrapping any `testServer()` calls in `later::with_temp_loop` if the suite hangs. Update the `direction_type` and `platform_name` occurrences in `test-signature.R`, `test-signature-visibility.R`, `test-gene-search.R`, `test-annotate.R`, `test-gem-enrichment.R` and the compare helpers as failures surface.

Expected: PASS. Rerun clean if a prior run aborted.

- [ ] **Step 6: Commit**

```bash
git add api mcp tests
git commit -m "Emit type and platform from API reads, with retired sort_by names as input aliases"
```

---

### Task 8: Legacy Shiny app

**Repo:** `SigRepo_Server`

**Files:**
- Modify: `legacy_app/modules/compare_module.R`, `legacy_app/modules/signature_module.R`, `legacy_app/modules/annotate_module.R`
- Modify: `legacy_app/utils/compare_utils.R`, `legacy_app/utils/signature_utils.R`, `legacy_app/utils/utils.R`
- Modify: `legacy_app/www/R_client.Rmd` and its knitted `legacy_app/www/R_client.html`, `legacy_app/www/knits/R_client.Rmd`, `legacy_app/www/knits/R_client.html`
- Test: `tests/testthat/test-shiny-compare-helpers.R`, `test-shiny-signature-helpers.R`, `helper-shiny-signature.R`, `helper-shiny-compare.R`

**Interfaces:**
- Consumes: the client and API changes from Tasks 3 through 7.
- Produces: no new interfaces.

**Why this task matters more than the React app:** per the spec, nginx has served the Shiny app at `/` since mid September with `sigrepo-web` stopped. Verify that routing before deploying rather than assuming it, but plan on Shiny being what users actually see.

- [ ] **Step 1: Update the modules and utils**

Change each `direction_type` occurrence to `type` and each `platform_name` to `platform` across the six files. These read API responses and client output, both of which now use the new names.

Read each site rather than running a blind substitution: `compare_module.R` has five occurrences and `signature_module.R` four, and some are user-facing column labels where the displayed text should stay human readable (for example a column header reading "Direction Type" can stay "Direction" rather than becoming "type").

- [ ] **Step 2: Update the R client documentation page**

`legacy_app/www/R_client.Rmd` is a user-facing tutorial rendered into the app. Update its `direction_type` occurrences to `type`.

**Do not knit `legacy_app/www/knits/R_client.Rmd` to regenerate the `.html`.**
That document's chunks execute by default against the production droplet and
contain live `addSignature()`, `deleteSignature()`, and `updateSignature()`
calls -- rendering it writes to and deletes from production data. Edit the
committed `.html` output directly instead (it is not derived from the `.Rmd`
at build time; nothing in this repo re-knits it), and get the corresponding
source edits into the `.Rmd` without executing it.

Confirm whether `legacy_app/www/R_client.html` is a copy of the knit output or a separately maintained file before editing it directly.

- [ ] **Step 3: Update the Shiny test helpers and tests**

Change the `direction_type` and `platform_name` occurrences in the four test files. These build fake API response frames, so their column names must match what the API now returns.

- [ ] **Step 4: Run the Shiny helper tests**

Run the server suite's Shiny helper test files. Expected: PASS.

- [ ] **Step 5: Drive the app headlessly**

Start the Shiny app against the migrated local stack and confirm the signature table renders its direction and platform columns with values rather than blanks, and that the Annotate tab still loads a signature. A blank column is the signature of a missed rename, and it will not fail any test.

- [ ] **Step 6: Commit**

```bash
git add legacy_app tests
git commit -m "Update legacy Shiny app to the renamed type and platform fields"
```

---

### Task 9: React frontend

**Repo:** `SigRepo_Server`

**Files:**
- Modify: `web/src/api/client.ts:306,311,334,335,464`
- Modify: `web/src/pages/SignaturesPage.tsx:160,163`
- Modify: `web/src/pages/SignatureDetailPage.tsx:83,307,308`

**Interfaces:**
- Consumes: the API response shape from Task 7.
- Produces: TypeScript interfaces whose field names match the API.

- [ ] **Step 1: Update the interfaces and the sort union**

In `web/src/api/client.ts`, rename the interface field `direction_type` to `type` (lines 306 and 464) and `platform_name` to `platform` (line 311). In the sort union at lines 334-335, replace `"direction_type"` with `"type"` and `"platform_name"` with `"platform"`. Update the comment at line 300 that names the joined lookups.

- [ ] **Step 2: Update the two pages**

In `SignaturesPage.tsx:160`, change the column key to `"type"`, keeping the human-readable label as `"Direction"` or `"Type"` rather than the raw field name. On line 163, change the key and the `render` accessor from `r.platform_name` to `r.platform`.

In `SignatureDetailPage.tsx`, change `sig.platform_name` to `sig.platform` (line 307) and `sig.direction_type` to `sig.type` (line 308), and update the comment on line 83.

- [ ] **Step 3: Type-check and build**

Run the web workspace's type check and build (`npm run build` in `web/`, or whatever the repo's script is; check `web/package.json`).
Expected: no type errors. A missed rename surfaces here as a compile error, which is the main value of doing this in TypeScript.

- [ ] **Step 4: Rebuild the web image**

The web image is baked rather than bind-mounted, so the built assets only reach a running stack through an image rebuild. Rebuild it per this repo's compose build workflow and confirm the Signatures list and a signature detail page render both fields.

- [ ] **Step 5: Commit**

```bash
git add web
git commit -m "Update React client types and pages to the renamed type and platform fields"
```

---

### Task 10: Full validation across both install paths

**Files:** none modified.

**Interfaces:**
- Consumes: everything.
- Produces: evidence that both a fresh install and a migrated install work.

**Why two paths:** the `validate-and-ship` skill builds its database from `mysql/schema/*.sql`, so it exercises the fresh-install path and the changed `platforms.csv` header, but it never runs the migration, because a new database has nothing to migrate. Passing one says nothing about the other.

- [ ] **Step 1: Fresh install path**

Invoke the repo's `validate-and-ship` skill. It stands up a throwaway stack (MySQL, the Plumber API, the MCP server), bootstraps it, runs the `local_validation` harness and tears it down.

Expected: clean. Pay specific attention to the platform bootstrap step, since that is where a `platforms.csv` header disagreeing with `addPlatform()`'s required column shows up.

- [ ] **Step 2: Migrated install path**

Against the persistent local stack that Task 1 migrated: restart the API container so it picks up the new code, then exercise through the client `searchSignature()`, `getSignature()`, `addSignature()`, `deleteSignature()` and `compareSignatures()`.

Expected: all succeed. A signature added here and then retrieved should round-trip with `metadata$type` and `metadata$platform` populated.

- [ ] **Step 3: Backward compatibility spot check**

Upload a signature built with the pre-phase-1 OmicSignature package, so its metadata carries `direction_type`, and confirm the API accepts it and stores the value under the `type` column.

Expected: upload succeeds. This is Review Focus item 1, end to end rather than only at the unit level.

- [ ] **Step 4: Confirm no stale names remain**

```bash
grep -rn "direction_type\|platform_name" --exclude-dir=docs --exclude-dir=.git --exclude-dir=node_modules .
```

Expected: matches only in `mysql/migrations/` (which must name the old columns), the retired-name aliases in `.signature_sort_columns`, the upload normalizer in `create_signature.R`, the stale-object guard in the client's `checkOmicSignature()`, and their tests. Every other match is a miss.

- [ ] **Step 5: Package checks**

With `SIGREPO_TEST_*` pointed at the local stack, run `devtools::check()` in both repos.
Expected: no new errors, warnings or notes relative to their base branches.

- [ ] **Step 6: Open the pull requests**

Open both PRs, cross-referencing each other and noting in each description that the migration must be applied to an environment before or with the code deploy, and that BUMC and production are deliberately left for a manual run after a `mysqldump`.
