# Phase 1: OmicSignature `direction_type` to `type` Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rename the OmicSignature metadata field `direction_type` to `type`, while still accepting the old name from existing metadata lists and JSON files with a deprecation warning.

**Architecture:** One internal normalizer function converts a legacy metadata list to the new key. It is called from `checkMetadata()`, which is the single choke point every construction path flows through: `OmicSignature$new()`, the `metadata<-` active binding, and `readJson()` (which constructs via `$new()`). No other code needs to know the old name exists.

**Tech Stack:** R 4.x, R6, testthat edition 3, roxygen2, dplyr.

**Spec:** `spec/2026-09-25-type-platform-rename-design.md` (in the SigRepo_Server repo, alongside this plan)

**Repo:** `OmicSignature` at `~/Documents/GitHub/OmicSignature`. This is a single checkout, not a `main` + worktree layout. It currently sits on the already merged branch `fix/difexp-null-probe-id`.

## Global Constraints

- The field is named exactly `type`. Not `direction`, not `signature_type`.
- Stored values never change: `uni-directional`, `bi-directional`, `categorical`.
- `platform` is already correct in this package. Do not touch it in phase 1.
- A metadata list carrying both `type` and `direction_type` is an error, never a guess.
- Legacy input warns. It does not message, and it does not fail.
- testthat edition 3 is configured. Use `expect_error()` with regexp matching freely.
- **Warning assertions must not use `expect_warning(expr, regexp)` when the expression constructs an `OmicSignature`.** `checkMetadata()` also warns about `platform`, `organism` and `sample_type` defaults, so several warnings fire from one construction and `expect_warning()` matches only the first. Use this idiom instead, which is order independent:
  ```r
  warns <- testthat::capture_warnings(sig <- OmicSignature$new(...))
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  ```
  `expect_warning(expr, regexp)` is fine where exactly one warning can fire, such as calling `.normalize_metadata_names()` or `createMetadata()` directly with all fields valid.
- `OmicSignature$new()`, `readJson()` and `writeJson()` all print. Wrap them in `capture.output()` the way the existing tests do, and never assert `expect_silent()` around them.
- `checkMetadata()` sorts metadata alphabetically before returning it. Do not write a test that asserts metadata key order matches input order.

## Review Focus

1. A metadata list carrying both `type` and `direction_type` must error rather than silently preferring one. Covered in Task 1.
2. A JSON file written by an older version of the package, carrying `direction_type`, must still load through `readJson()` with a warning. Covered in Task 5.
3. Assigning a legacy metadata list through `obj$metadata <- ...` must normalize, not store the old key. This path does not go through `initialize()`. Covered in Task 2.
4. `createMetadata(direction_type = "bi")` must still apply the `"bi"` to `"bi-directional"` shorthand recoding after forwarding through the deprecated argument. Covered in Task 3.
5. `createMetadata()` receiving both `type` and `direction_type` must error. Covered in Task 3.

---

### Task 1: Internal metadata key normalizer

**Files:**
- Create: `R/normalizeMetadataNames.R`
- Test: `tests/testthat/test-normalize-metadata-names.R`

**Interfaces:**
- Consumes: nothing.
- Produces: `.normalize_metadata_names(metadata)`, taking a list and returning a list. Renames a `direction_type` element to `type` and warns. Errors when both keys are present. Returns its input unchanged when there is no `direction_type` element. Not exported.

- [ ] **Step 1: Write the failing test**

Create `tests/testthat/test-normalize-metadata-names.R`:

```r
test_that(".normalize_metadata_names() renames direction_type to type and warns", {
  input <- list(signature_name = "s1", direction_type = "bi-directional", assay_type = "transcriptomics")
  expect_warning(
    out <- .normalize_metadata_names(input),
    "direction_type.*deprecated"
  )
  expect_equal(out$type, "bi-directional")
  expect_false("direction_type" %in% names(out))
  expect_equal(out$signature_name, "s1")
  expect_equal(out$assay_type, "transcriptomics")
})

test_that(".normalize_metadata_names() leaves a modern metadata list untouched and silent", {
  input <- list(signature_name = "s1", type = "uni-directional")
  expect_silent(out <- .normalize_metadata_names(input))
  expect_identical(out, input)
})

test_that(".normalize_metadata_names() errors when both keys are present", {
  input <- list(signature_name = "s1", type = "uni-directional", direction_type = "bi-directional")
  expect_error(
    .normalize_metadata_names(input),
    "both 'type' and the deprecated 'direction_type'"
  )
})

test_that(".normalize_metadata_names() passes through inputs it cannot normalize", {
  expect_silent(out <- .normalize_metadata_names(list()))
  expect_identical(out, list())
  expect_silent(out2 <- .normalize_metadata_names("not a list"))
  expect_identical(out2, "not a list")
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-normalize-metadata-names.R")'`
Expected: FAIL with `could not find function ".normalize_metadata_names"`.

- [ ] **Step 3: Write minimal implementation**

Create `R/normalizeMetadataNames.R`:

```r
#' Normalize deprecated metadata field names
#'
#' Accepts a metadata list that may use the pre-1.4.0 field name
#' `direction_type` and returns the same list using the current name `type`.
#' Called from `OmicSignature`'s private `checkMetadata()`, which every
#' construction and assignment path routes through, so callers do not need to
#' normalize themselves.
#'
#' @param metadata a metadata list. Inputs that are not named lists are
#' returned unchanged, leaving their validation to the caller.
#' @return the metadata list using `type`.
#' @keywords internal
.normalize_metadata_names <- function(metadata) {
  if (!is.list(metadata)) {
    return(metadata)
  }
  field_names <- names(metadata)
  if (is.null(field_names) || !"direction_type" %in% field_names) {
    return(metadata)
  }
  if ("type" %in% field_names) {
    stop(
      "metadata contains both 'type' and the deprecated 'direction_type'. ",
      "Keep only 'type'."
    )
  }
  names(metadata)[field_names == "direction_type"] <- "type"
  warning(
    "metadata field 'direction_type' is deprecated and was renamed to 'type'. ",
    "Update your metadata to use 'type'."
  )
  metadata
}
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-normalize-metadata-names.R")'`
Expected: PASS, 4 tests.

- [ ] **Step 5: Commit**

```bash
git add R/normalizeMetadataNames.R tests/testthat/test-normalize-metadata-names.R
git commit -m "Add internal normalizer for deprecated direction_type metadata field"
```

---

### Task 2: Switch the R6 class to `type`

**Files:**
- Modify: `R/OmicSignature.R` — `checkMetadata()` at `:303-323`, `initialize()` at `:92-95`, `:122`, `:151`, the `metadata` active binding at `:157-183`, `:189`, `:199`, `:224-240`, and `.extract_signature_rows()` at `:3-19`
- Test: `tests/testthat/test-OmicSignature.R`

**Interfaces:**
- Consumes: `.normalize_metadata_names()` from Task 1.
- Produces: `checkMetadata(metadata, v = FALSE)`. The `signatureType` parameter is removed, because the method body never reads it. `.extract_signature_rows(difexp, conditions, type)`. Every internal read of the direction field goes through `private$.metadata$type`.

**Why `checkMetadata` and `initialize` are one task rather than two:** `initialize()` reads `metadata$direction_type` three times *before* `checkMetadata()` runs. Changing only `checkMetadata()` would leave the class unable to construct from the new field name at all, because `initialize()` would pass `signatureType = NULL` into `checkSignature()`, which stops with "Signature type not specified" (`R/OmicSignature.R:420`). That is the entire feature, so there is no state in which a reviewer could accept the first half and reject the second.

- [ ] **Step 1: Write the failing tests**

Append to `tests/testthat/test-OmicSignature.R`:

```r
test_that("OmicSignature$new() constructs from metadata using the field name type", {
  ## This is the guard for the initialize() bypass. Before the fix, initialize()
  ## reads metadata$direction_type, which is NULL on a modern metadata list, so
  ## checkSignature() receives signatureType = NULL and stops with
  ## "Signature type not specified" -- the new field name does not work at all.
  modern_metadata <- list(
    signature_name = "modern_sig",
    phenotype = "test",
    organism = predefined_organisms[1],
    type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(
    probe_id = c("probe_1", "probe_2"),
    feature_name = c("A", "B"),
    score = c(2, -2),
    stringsAsFactors = FALSE
  )
  capture.output(sig <- OmicSignature$new(metadata = modern_metadata, signature = signature))
  expect_equal(sig$metadata$type, "uni-directional")
  expect_equal(nrow(sig$signature), 2)
})

test_that("OmicSignature$new() accepts legacy direction_type metadata with a warning", {
  legacy_metadata <- list(
    signature_name = "legacy_sig",
    phenotype = "test",
    organism = predefined_organisms[1],
    direction_type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(
    probe_id = c("probe_1", "probe_2"),
    feature_name = c("A", "B"),
    score = c(2, -2),
    stringsAsFactors = FALSE
  )
  ## capture_warnings() rather than expect_warning(): checkMetadata() also
  ## warns about the platform and sample_type defaults this metadata omits.
  warns <- testthat::capture_warnings(
    capture.output(sig <- OmicSignature$new(metadata = legacy_metadata, signature = signature))
  )
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  expect_equal(sig$metadata$type, "uni-directional")
  expect_false("direction_type" %in% names(sig$metadata))
})

test_that("OmicSignature$new() errors when metadata omits type", {
  bad_metadata <- list(
    signature_name = "no_type",
    phenotype = "test",
    organism = predefined_organisms[1],
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(
    probe_id = "probe_1", feature_name = "A", score = 1, stringsAsFactors = FALSE
  )
  expect_error(
    OmicSignature$new(metadata = bad_metadata, signature = signature),
    "does not contain attribute\\(s\\): type"
  )
})

test_that("a bi-directional signature is still validated when metadata uses type", {
  ## Proves the direction value actually reaches checkSignature(), rather than
  ## the construction merely succeeding: a bi-directional signature requires a
  ## group_label column.
  modern_bi <- list(
    signature_name = "modern_bi",
    phenotype = "test",
    organism = predefined_organisms[1],
    type = "bi-directional",
    assay_type = predefined_assaytypes[1]
  )
  no_group <- data.frame(
    probe_id = c("probe_1", "probe_2"),
    feature_name = c("A", "B"),
    score = c(2, -2),
    stringsAsFactors = FALSE
  )
  expect_error(
    OmicSignature$new(metadata = modern_bi, signature = no_group),
    "group_label"
  )
})

test_that("assigning a legacy metadata list through the active binding normalizes it", {
  ## Builds its own object rather than using make_uni_test_signature(), whose
  ## metadata is still keyed direction_type until Task 4. Keeping this test
  ## self-contained means every test in this task passes by the end of it.
  modern_metadata <- list(
    signature_name = "binding_sig",
    phenotype = "test",
    organism = predefined_organisms[1],
    type = "uni-directional",
    assay_type = predefined_assaytypes[1]
  )
  signature <- data.frame(
    probe_id = c("probe_1", "probe_2"),
    feature_name = c("A", "B"),
    score = c(2, 1),
    stringsAsFactors = FALSE
  )
  capture.output(sig <- OmicSignature$new(metadata = modern_metadata, signature = signature))

  replacement <- sig$metadata
  names(replacement)[names(replacement) == "type"] <- "direction_type"
  warns <- testthat::capture_warnings(sig$metadata <- replacement)
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  expect_equal(sig$metadata$type, "uni-directional")
  expect_false("direction_type" %in% names(sig$metadata))
})
```

- [ ] **Step 2: Run tests to verify they fail**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-OmicSignature.R")'`
Expected: FAIL. The first test fails because `checkMetadata()` still requires `direction_type`; the "omits type" test fails because the missing-attribute message still names `direction_type`.

- [ ] **Step 3: Update `checkMetadata()`**

In `R/OmicSignature.R`, change the `checkMetadata` signature and opening block from:

```r
      checkMetadata = function(metadata, signatureType = NULL, v = FALSE) {
        if (!is(metadata, "list")) stop("metadata must be a list. See createMetadata() for details.")

        # check required metadata fields
        metadataRequired <- c("signature_name", "phenotype", "organism", "direction_type", "assay_type")
```

to:

```r
      checkMetadata = function(metadata, v = FALSE) {
        if (!is(metadata, "list")) stop("metadata must be a list. See createMetadata() for details.")

        ## accept the pre-1.4.0 field name `direction_type`; every construction
        ## and assignment path reaches checkMetadata(), so normalizing here
        ## covers $new(), the metadata<- binding and readJson() at once.
        metadata <- .normalize_metadata_names(metadata)

        # check required metadata fields
        metadataRequired <- c("signature_name", "phenotype", "organism", "type", "assay_type")
```

The `signatureType` parameter is removed rather than kept, because the method body never reads it; Step 4 removes the only call that passes it.

Then change the validation block from:

```r
        # check direction_type
        if (!metadata$direction_type %in% c("uni-directional", "bi-directional", "categorical")) {
          stop("direction_type must be uni-directional, bi-directional, or categorical. ")
        }
```

to:

```r
        # check type
        if (!metadata$type %in% c("uni-directional", "bi-directional", "categorical")) {
          stop("type must be uni-directional, bi-directional, or categorical. ")
        }
```

- [ ] **Step 4: Fix `initialize()` and the active bindings**

In `initialize()`, replace lines 92-95:

```r
        private$.metadata <- private$checkMetadata(metadata, signatureType = metadata$direction_type, v = print_message)
        private$.signature <- private$checkSignature(signature, signatureType = metadata$direction_type, v = print_message)
        if (!is.null(difexp)) {
          difexp <- private$checkDifexp(difexp, signatureType = metadata$direction_type, v = print_message)
```

with:

```r
        ## check metadata first, then read the direction field from the
        ## normalized result: reading it off the raw `metadata` argument would
        ## bypass .normalize_metadata_names() for legacy inputs and would be
        ## NULL for modern ones, which checkSignature() rejects outright.
        private$.metadata <- private$checkMetadata(metadata, v = print_message)
        signature_type <- private$.metadata$type
        private$.signature <- private$checkSignature(signature, signatureType = signature_type, v = print_message)
        if (!is.null(difexp)) {
          difexp <- private$checkDifexp(difexp, signatureType = signature_type, v = print_message)
```

In the `metadata` active binding, replace every `$direction_type` read with `$type`:

```r
          new_metadata <- private$checkMetadata(value, v = print_message)
          if (!identical(new_metadata$type, private$.metadata$type)) {
            ## type governs what's required/meaningful in signature
            ## and difexp (e.g. group_label); re-validate both against the
            ## new type instead of letting metadata and data go structurally
            ## out of sync silently.
            private$.signature <- private$checkSignature(
              private$.signature, signatureType = new_metadata$type, v = print_message
            )
            if (!is.null(private$.difexp)) {
              private$.difexp <- private$checkDifexp(
                private$.difexp, signatureType = new_metadata$type, v = print_message
              )
            }
            if (new_metadata$type == "uni-directional") {
              private$checkNoStaleGroupLabel(private$.signature, private$.difexp)
            }
          }
```

Replace the remaining `private$.metadata$direction_type` reads at lines 122, 151, 189 and 199 with `private$.metadata$type`. Rename `.extract_signature_rows()`'s third parameter from `direction_type` to `type`, updating its body (`is_grouped <- type %in% c("bi-directional", "categorical")`) and its call site at line 151. Update the comment block near line 224 and the error string at line 234 from `direction_type` to `type`.

- [ ] **Step 5: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-OmicSignature.R")'`
Expected: all five new tests PASS. Other test files in the suite still fail, because `helper-signatures.R` and the JSON fixtures still use `direction_type`; Tasks 4 and 5 fix those. Do not chase them here.

Confirm specifically that the first and fourth new tests pass, since those two are the guard for the `initialize()` bypass.

- [ ] **Step 6: Commit**

```bash
git add R/OmicSignature.R tests/testthat/test-OmicSignature.R
git commit -m "Switch OmicSignature R6 class to the metadata field type"
```

### Task 3: `createMetadata()` takes `type`, with a deprecated `direction_type`

**Files:**
- Modify: `R/createMetadata.R` (whole file)
- Test: `tests/testthat/test-createMetadata.R`

**Interfaces:**
- Consumes: nothing from earlier tasks.
- Produces: `createMetadata(signature_name, organism, phenotype, assay_type, covariates, platform, type, sample_type, signature_collection, author, year, PMID, keywords, description, category_num, logfc_cutoff, p_value_cutoff, adj_p_cutoff, score_cutoff, cutoff_description, others, direction_type)`. Returns a metadata list whose direction element is named `type`. `direction_type` is the last argument, defaults to `NULL`, and is deprecated.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-createMetadata.R`:

```r
test_that("createMetadata() returns the direction under the name 'type'", {
  metadata <- createMetadata(
    signature_name = "sig_type",
    organism = predefined_organisms[2],
    assay_type = predefined_assaytypes[1],
    type = "bi-directional",
    platform = predefined_platforms[2],
    phenotype = "test"
  )
  expect_equal(metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(metadata))
})

test_that("createMetadata() accepts deprecated direction_type and still recodes shorthand", {
  expect_warning(
    metadata <- createMetadata(
      signature_name = "sig_legacy",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      direction_type = "bi",
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "'direction_type' argument is deprecated"
  )
  expect_equal(metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(metadata))
})

test_that("createMetadata() errors when both type and direction_type are supplied", {
  expect_error(
    createMetadata(
      signature_name = "sig_both",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      type = "bi-directional",
      direction_type = "bi-directional",
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "Supply only 'type'"
  )
})

test_that("createMetadata() errors when type is missing entirely", {
  expect_error(
    createMetadata(
      signature_name = "sig_none",
      organism = predefined_organisms[2],
      assay_type = predefined_assaytypes[1],
      platform = predefined_platforms[2],
      phenotype = "test"
    ),
    "'type' is required"
  )
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-createMetadata.R")'`
Expected: FAIL with `unused argument (type = ...)`.

- [ ] **Step 3: Write minimal implementation**

In `R/createMetadata.R`, change the argument list so `direction_type` becomes `type` in position 7 and a deprecated `direction_type = NULL` is appended last:

```r
createMetadata <- function(signature_name, organism, phenotype = "unknown", assay_type,
                           covariates = NULL, platform = "unknown", type,
                           sample_type = NULL, signature_collection = NULL,
                           author = NULL, year = NULL, PMID = NULL,
                           keywords = NULL, description = NULL, category_num = NULL,
                           logfc_cutoff = NULL, p_value_cutoff = NULL,
                           adj_p_cutoff = NULL, score_cutoff = NULL,
                           cutoff_description = NULL, others = NULL,
                           direction_type = NULL) {
  ## accept the pre-1.4.0 argument name
  if (!is.null(direction_type)) {
    if (!missing(type)) {
      stop("Supply only 'type'. The 'direction_type' argument is deprecated.")
    }
    warning("The 'direction_type' argument is deprecated; use 'type' instead.")
    type <- direction_type
  }
  if (missing(type)) {
    stop("'type' is required.")
  }

  # check sig type
  ## lowercase first as its own statement so `default = type` below
  ## refers to the already-lowercased value, not the original mixed-case
  ## input (recode_values() evaluates `default` eagerly against whatever
  ## `type` is bound to at call time).
  type <- tolower(type)
  type <- dplyr::recode_values(
    type,
    default = type,
    "bi" ~ "bi-directional",
    "uni" ~ "uni-directional"
  )
  if (!type %in% c("bi-directional", "uni-directional", "categorical")) {
    stop("type should be uni-directional, bi-directional or categorical.")
  }
```

In the result list, change `"direction_type" = direction_type,` to `"type" = type,`. Change the categorical block from `if (direction_type == "categorical")` to `if (type == "categorical")`.

Update the roxygen block: rename `@param direction_type` to `@param type` keeping the same prose, change the `@param category_num` line to read `required when type = "categorical"`, and add a new final param:

```r
#' @param direction_type deprecated. the former name of `type`. Supplying it warns and forwards to `type`. Supplying both is an error.
```

- [ ] **Step 4: Run test to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-createMetadata.R")'`
Expected: The four new tests PASS. The three pre-existing tests shown in the spec that pass `direction_type =` and assert `metadata$direction_type` now warn and fail. Update them in place to use `type =` and assert `metadata$type`, and change the invalid-value test's expected regexp from `"direction_type should be"` to `"type should be"`. Rerun until the file is green.

- [ ] **Step 5: Commit**

```bash
git add R/createMetadata.R tests/testthat/test-createMetadata.R
git commit -m "Rename createMetadata() direction_type argument to type with deprecation shim"
```

---

### Task 4: Update remaining callers, helpers and test fixtures

**Files:**
- Modify: `R/OmicSigFromDifexp.R:26`
- Modify: `R/compare_omic_signatures.R:567-583`
- Modify: `tests/testthat/helper-signatures.R` (two `direction_type = ` entries)
- Modify: `tests/testthat/test-OmicSigFromDifexp.R`, `tests/testthat/test-readwriteJson.R`, `tests/testthat/test-compare-omic-signatures.R`
- Modify: `data-raw/create_compare_unidirectional_data.R`, `data-raw/create_compare_mixed_direction_data.R`, `data-raw/create_compare_label_pairing_data.R`

**Interfaces:**
- Consumes: the `type` metadata field from Tasks 2 and 3.
- Produces: nothing new. This task makes the existing suite green.

- [ ] **Step 1: Run the full suite to enumerate remaining failures**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: FAIL. Record the failing files. These are the callers still reading or writing `direction_type`.

- [ ] **Step 2: Update the two library call sites**

In `R/OmicSigFromDifexp.R:26`, change `signatureType <- metadata$direction_type` to `signatureType <- metadata$type`.

In `R/compare_omic_signatures.R`, change lines 567 through 583 so the direction field is read as `$type`. The local variable currently named `direction_type` becomes `signature_types`, because `type` alone would shadow the inner `type <- sig$metadata$type` read:

```r
  signature_types <- vapply(sig_list, function(sig) {
    type <- sig$metadata$type
    if (is.null(type)) NA_character_ else as.character(type)[1]
  }, character(1))
  if (any(signature_types == "categorical", na.rm = TRUE)) {
    stop("compare_omic_signatures() does not support categorical signatures: ",
         paste(names(sig_list)[signature_types == "categorical"], collapse = ", "))
  }
```

and at line 583, `identical(sig$metadata$type, "uni-directional")`.

Note: read the current lines before editing. The snippet above reproduces the structure but the exact surrounding code must be preserved.

- [ ] **Step 3: Update helpers, tests and data-raw builders**

In `tests/testthat/helper-signatures.R`, change both `direction_type = "bi-directional"` and `direction_type = "uni-directional"` entries to `type =`.

In the three test files and three `data-raw` builders, change each `direction_type` occurrence to `type`. These are metadata list keys and argument names, so the change is mechanical. Verify none remain:

```bash
grep -rn "direction_type" R tests data-raw
```

Expected output: only the deliberate deprecation references, which are `R/normalizeMetadataNames.R`, `R/createMetadata.R`'s deprecated argument and roxygen line, and the shim tests in `tests/testthat/test-normalize-metadata-names.R`, `test-createMetadata.R` and `test-OmicSignature.R`.

- [ ] **Step 4: Run the full suite to verify it passes**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: PASS, zero failures. JSON tests may still fail if they read `inst/extdata` fixtures; if so, leave them and finish in Task 5.

- [ ] **Step 5: Commit**

```bash
git add R tests data-raw
git commit -m "Update OmicSignature callers, helpers and builders to metadata field type"
```

---

### Task 5: JSON fixtures and the legacy-file regression test

**Files:**
- Modify: `inst/extdata/OmS_example_1.json`, `inst/extdata/OmS_example_2.json`, `inst/extdata/OmS_example_3.json`, `inst/extdata/Myc_reduce_mice_liver_24m_OmS.json`
- Create: `inst/extdata/OmS_legacy_direction_type.json`
- Test: `tests/testthat/test-readwriteJson.R`

**Interfaces:**
- Consumes: the shim from Tasks 1 and 2.
- Produces: `inst/extdata/OmS_legacy_direction_type.json`, a fixture that deliberately keeps the old field name so the shim has a file-level regression test rather than only a unit test.

- [ ] **Step 1: Write the failing test**

Append to `tests/testthat/test-readwriteJson.R`:

```r
test_that("readJson() loads a legacy file whose metadata uses direction_type", {
  path <- system.file("extdata", "OmS_legacy_direction_type.json", package = "OmicSignature")
  expect_true(file.exists(path))
  warns <- testthat::capture_warnings(capture.output(sig <- readJson(path)))
  expect_true(any(grepl("direction_type.*deprecated", warns)))
  expect_true(inherits(sig, "OmicSignature"))
  expect_equal(sig$metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(sig$metadata))
})

test_that("a legacy file round-trips out under the new field name", {
  path <- system.file("extdata", "OmS_legacy_direction_type.json", package = "OmicSignature")
  sig <- suppressWarnings(readJson(path))
  ## tempfile() rather than withr::local_tempfile(): withr is not in this
  ## package's Suggests, and the rest of this file already uses tempfile().
  out <- tempfile(fileext = ".json")
  ## writeJson() and readJson() both print; the rest of this file wraps them in
  ## capture.output() for that reason. Do not use expect_silent() here.
  capture.output(writeJson(sig, out))
  capture.output(reread <- readJson(out))
  expect_equal(reread$metadata$type, "bi-directional")
  expect_false("direction_type" %in% names(reread$metadata))
})
```

- [ ] **Step 2: Run test to verify it fails**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_file("tests/testthat/test-readwriteJson.R")'`
Expected: FAIL on `expect_true(file.exists(path))`, because the fixture does not exist yet.

- [ ] **Step 3: Create the legacy fixture and update the four current ones**

Create the legacy fixture by copying an existing bi-directional example before renaming its field, so it is a genuine older-format file:

```bash
cp inst/extdata/OmS_example_1.json inst/extdata/OmS_legacy_direction_type.json
```

Then confirm the copied file's metadata field is `direction_type` and that its value is `bi-directional`. If `OmS_example_1.json` is not bi-directional, use whichever of the three examples is, and adjust the test's expected value to match.

Now rename the field in the four shipped fixtures only, leaving the new legacy fixture alone:

```bash
for f in inst/extdata/OmS_example_1.json inst/extdata/OmS_example_2.json \
         inst/extdata/OmS_example_3.json inst/extdata/Myc_reduce_mice_liver_24m_OmS.json; do
  sed -i '' 's/"direction_type"/"type"/g' "$f"
done
```

Each of these files also contains a `metadata_fields` array listing field names, written from `names(OmicObj$metadata)`. The `sed` above updates it in the same pass because the name appears there as a quoted string too. Verify:

```bash
grep -c '"type"' inst/extdata/OmS_example_1.json
grep -c 'direction_type' inst/extdata/OmS_example_1.json
```

Expected: at least 1 for the first command, 0 for the second.

- [ ] **Step 4: Run tests to verify they pass**

Run: `Rscript -e 'devtools::load_all("."); testthat::test_dir("tests/testthat")'`
Expected: PASS, zero failures across the whole suite.

- [ ] **Step 5: Commit**

```bash
git add inst/extdata tests/testthat/test-readwriteJson.R
git commit -m "Rename JSON fixture metadata field to type and add a legacy-format fixture"
```

---

### Task 6: Documentation, vignettes and a clean package check

**Files:**
- Modify: `vignettes/CreateOmS.Rmd`, `vignettes/CompareUnidirectional.Rmd`, `vignettes/CompareMixedDirection.Rmd`
- Modify: `man/createMetadata.Rd`, `man/OmicSignature.Rd` (regenerated, not hand edited)
- Modify: `DESCRIPTION` (version bump), `NAMESPACE` (regenerated)

**Interfaces:**
- Consumes: everything from Tasks 1 through 5.
- Produces: a package that passes `R CMD check` with no new warnings or notes.

- [ ] **Step 1: Update the vignettes**

Change every `direction_type` occurrence in the three vignettes to `type`. In `CreateOmS.Rmd`, which has seven occurrences including prose describing the field, make sure the prose reads naturally rather than being a blind substitution. Add one short paragraph noting that `direction_type` is still accepted from older metadata and files, and warns.

- [ ] **Step 2: Regenerate documentation**

Run: `Rscript -e 'devtools::document()'`
Expected: `man/createMetadata.Rd` gains a `type` param and a deprecated `direction_type` param; `NAMESPACE` is unchanged because `.normalize_metadata_names()` is internal and not exported.

- [ ] **Step 3: Bump the version**

In `DESCRIPTION`, change `Version: 1.3.0` to `Version: 1.4.0`. This is a renamed public field, so it is a minor bump rather than a patch.

- [ ] **Step 4: Run the full check**

Run: `Rscript -e 'devtools::check()'`
Expected: 0 errors, 0 warnings. Any note must be one that already existed before this work; compare against a check of `main` if unsure.

- [ ] **Step 5: Commit**

```bash
git add vignettes man DESCRIPTION NAMESPACE
git commit -m "Document metadata field rename to type and bump version to 1.4.0"
```

---

## Final verification

- [ ] `grep -rn "direction_type" R tests vignettes data-raw inst` returns only the intentional deprecation sites listed in Task 4 Step 3, plus the legacy JSON fixture.
- [ ] `Rscript -e 'devtools::check()'` is clean.
- [ ] Open the pull request against `main`. Do not merge phase 2 in either SigRepo repo until this is released, because phase 2's client reads `metadata$type`.
