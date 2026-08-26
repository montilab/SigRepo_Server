# build_omic_signature() (api/lib/omic_signature.R) picks between two
# createOmicSignature() call shapes depending on what the installed SigRepo
# client actually accepts. These tests force each shape directly via
# .omic_signature_supports_difexp() rather than relying on whichever client
# happens to be loaded -- production runs the 2-arg client, a local
# SIGREPO_DIR=/SigRepo checkout runs the 4-arg one, and a test that only
# exercises whichever is installed can never see the other branch. See the
# comment atop api/lib/omic_signature.R and the note in
# api/lib/gem_enrichment.R for why that distinction matters.
source(testthat::test_path("../../api/lib/omic_signature.R"), local = FALSE)

# Temporarily overrides a global binding, returning a function that restores
# whatever was there before (or removes it, if it did not exist). Called with
# on.exit(restore(), add = TRUE) directly inside the test that needs it, so
# the restore runs in that test's own frame rather than this helper's.
stash_global <- function(name) {
  existed <- base::exists(name, envir = globalenv(), inherits = FALSE)
  old <- if (existed) base::get(name, envir = globalenv()) else NULL
  function() {
    if (existed) {
      base::assign(name, old, envir = globalenv())
    } else if (base::exists(name, envir = globalenv(), inherits = FALSE)) {
      base::rm(list = name, envir = globalenv())
    }
  }
}

test_that("build_omic_signature still refuses a has_difexp = 1 signature under a 2-arg client by default", {
  # This is compare.R's call shape: build_omic_signature(db_row = ..., difexp
  # = ...) with require_difexp left at its default. It must keep refusing --
  # silently dropping the difexp there is exactly the deadlock-avoidance this
  # guard exists for.
  restore_supports <- stash_global(".omic_signature_supports_difexp")
  on.exit(restore_supports(), add = TRUE)
  assign(".omic_signature_supports_difexp", function() FALSE, envir = globalenv())

  db_row <- data.frame(has_difexp = 1, stringsAsFactors = FALSE)
  expect_error(
    build_omic_signature(db_row = db_row, difexp = list(some = "difexp")),
    "cannot accept one directly"
  )
})

test_that("build_omic_signature still builds a difexp-free signature under a 2-arg client by default", {
  # The guard is specifically about has_difexp = 1; a signature that has no
  # difexp at all was never affected and must keep working the same way.
  testthat::skip_if_not(requireNamespace("SigRepo", quietly = TRUE), "SigRepo package not installed")

  restore_supports <- stash_global(".omic_signature_supports_difexp")
  on.exit(restore_supports(), add = TRUE)
  assign(".omic_signature_supports_difexp", function() FALSE, envir = globalenv())

  restore_conn <- stash_global("conn_handler")
  on.exit(restore_conn(), add = TRUE)
  assign("conn_handler", "stub-conn-handler", envir = globalenv())

  seen <- NULL
  testthat::local_mocked_bindings(
    createOmicSignature = function(conn_handler, db_signature_tbl, ...) {
      seen <<- list(conn_handler = conn_handler, extra = base::list(...))
      "mocked-omic-signature"
    },
    .package = "SigRepo"
  )

  db_row <- data.frame(has_difexp = 0, signature_id = 42L, stringsAsFactors = FALSE)
  out <- build_omic_signature(db_row = db_row)

  expect_equal(out, "mocked-omic-signature")
  expect_equal(base::length(seen$extra), 0)
})

test_that("build_omic_signature with require_difexp = FALSE does not refuse a has_difexp = 1 signature under a 2-arg client", {
  # GEM's call shape (api/lib/gem_enrichment.R): it never reads the difexp at
  # all, so it opts out of the guard entirely. This is the fix for GEM
  # refusing every metabolomics signature (all of which have has_difexp = 1)
  # on a server running the 2-arg client.
  testthat::skip_if_not(requireNamespace("SigRepo", quietly = TRUE), "SigRepo package not installed")

  restore_supports <- stash_global(".omic_signature_supports_difexp")
  on.exit(restore_supports(), add = TRUE)
  assign(".omic_signature_supports_difexp", function() FALSE, envir = globalenv())

  restore_conn <- stash_global("conn_handler")
  on.exit(restore_conn(), add = TRUE)
  assign("conn_handler", "stub-conn-handler", envir = globalenv())

  seen <- NULL
  testthat::local_mocked_bindings(
    createOmicSignature = function(conn_handler, db_signature_tbl, ...) {
      seen <<- list(conn_handler = conn_handler, db_signature_tbl = db_signature_tbl, extra = base::list(...))
      "mocked-omic-signature"
    },
    .package = "SigRepo"
  )

  db_row <- data.frame(has_difexp = 1, signature_id = 42L, stringsAsFactors = FALSE)
  out <- build_omic_signature(db_row = db_row, require_difexp = FALSE)

  expect_equal(out, "mocked-omic-signature")
  expect_false(base::is.null(seen))
  expect_equal(seen$conn_handler, "stub-conn-handler")
  expect_equal(seen$db_signature_tbl, db_row)
  # The 2-arg call shape specifically: no difexp/fetch_difexp passed through.
  expect_equal(base::length(seen$extra), 0)
})

test_that("build_omic_signature passes difexp through unchanged when the client supports it, regardless of require_difexp", {
  # require_difexp only governs the 2-arg fallback's guard. When the
  # installed client actually accepts a difexp, that richer path must be
  # completely unaffected by the new parameter.
  testthat::skip_if_not(requireNamespace("SigRepo", quietly = TRUE), "SigRepo package not installed")

  restore_supports <- stash_global(".omic_signature_supports_difexp")
  on.exit(restore_supports(), add = TRUE)
  assign(".omic_signature_supports_difexp", function() TRUE, envir = globalenv())

  restore_conn <- stash_global("conn_handler")
  on.exit(restore_conn(), add = TRUE)
  assign("conn_handler", "stub-conn-handler", envir = globalenv())

  seen <- NULL
  testthat::local_mocked_bindings(
    createOmicSignature = function(conn_handler, db_signature_tbl, difexp, fetch_difexp) {
      seen <<- list(conn_handler = conn_handler, difexp = difexp, fetch_difexp = fetch_difexp)
      "mocked-omic-signature-4arg"
    },
    .package = "SigRepo"
  )

  db_row <- data.frame(has_difexp = 1, signature_id = 42L, stringsAsFactors = FALSE)
  my_difexp <- data.frame(gene = "TP53", stringsAsFactors = FALSE)
  out <- build_omic_signature(db_row = db_row, difexp = my_difexp, require_difexp = FALSE)

  expect_equal(out, "mocked-omic-signature-4arg")
  expect_equal(seen$difexp, my_difexp)
  expect_false(seen$fetch_difexp)
})
