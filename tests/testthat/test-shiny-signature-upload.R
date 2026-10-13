# The Signature tab's upload dialog: what reaches addSignature().
#
# The dialog offers Private or Public, and the handler used to drop that
# choice on the floor, so every upload took the client's default (issue #159).
load_signature_app()

test_that("an uploaded signature is added with the visibility chosen in the dialog", {
  adds <- recording_adds()
  upload <- tempfile(fileext = ".rds")
  saveRDS(list(kind = "signature"), upload)

  run_signature_module(add_fn = adds$fn, {
    session$setInputs(
      upload_file = data.frame(name = "s.rds", size = 1, type = "", datapath = upload, stringsAsFactors = FALSE),
      visibility = "FALSE"
    )
    session$setInputs(upload_btn = 1)

    calls <- adds$calls()
    expect_length(calls, 1)
    expect_true(isFALSE(calls[[1]]$visibility))
    expect_equal(calls[[1]]$conn_handler, "user-conn")
    expect_equal(calls[[1]]$omic_signature, list(kind = "signature"))
  })
})

test_that("Public in the dialog uploads a public signature", {
  adds <- recording_adds()
  upload <- tempfile(fileext = ".rds")
  saveRDS(list(kind = "signature"), upload)

  run_signature_module(add_fn = adds$fn, {
    session$setInputs(
      upload_file = data.frame(name = "s.rds", size = 1, type = "", datapath = upload, stringsAsFactors = FALSE),
      visibility = "TRUE"
    )
    session$setInputs(upload_btn = 1)

    expect_true(isTRUE(adds$calls()[[1]]$visibility))
  })
})
