# Manage Access on the Signature tab (issue #96).
#
# The modal opened, but nothing behind it was connected: signature_module.R
# listened for an input id the modal never created, the modal waited on a button
# and a permissions control that did not exist, and its server was registered
# inside an observeEvent. Selecting a user and confirming wrote nothing to the
# database and reported nothing to the user. These tests hold each of those
# links shut.
load_signature_app()

test_that("confirming the modal grants access to the selected users", {
  # The regression test for the dead wiring: before the fix, nothing at all
  # reached the granter.
  grants <- recording_grants()

  run_signature_module(
    {
      session$setInputs(signature_tbl_rows_selected = 1, signature_tbl_row_last_clicked = 1)
      session$setInputs(access_btn = 1)
      session$setInputs(user_selector = c("ann", "bo"), access_type_selector = "viewer")
      session$setInputs(add_users_confirm = 1)

      expect_length(grants$calls(), 1)
      expect_identical(grants$calls()[[1]]$user_names, c("ann", "bo"))
      expect_identical(grants$calls()[[1]]$access_type, "viewer")
    },
    grant_fn = grants$fn
  )
})

test_that("access is granted for the signature the user actually selected", {
  grants <- recording_grants()

  run_signature_module(
    {
      session$setInputs(signature_tbl_rows_selected = 2, signature_tbl_row_last_clicked = 2)
      session$setInputs(access_btn = 1)
      session$setInputs(user_selector = "ann", access_type_selector = "editor")
      session$setInputs(add_users_confirm = 1)

      expect_identical(grants$calls()[[1]]$signature_id, signature_db_rows$signature_id[[2]])
    },
    grant_fn = grants$fn
  )
})

test_that("confirming twice grants twice, not three times", {
  # The modal's server used to be registered inside an observeEvent, so every
  # click added another copy of its observers and the nth click fired n grants.
  grants <- recording_grants()

  run_signature_module(
    {
      session$setInputs(signature_tbl_rows_selected = 1, signature_tbl_row_last_clicked = 1)
      session$setInputs(access_btn = 1)
      session$setInputs(user_selector = "ann", access_type_selector = "viewer")
      session$setInputs(add_users_confirm = 1)
      session$setInputs(user_selector = "bo", access_type_selector = "viewer")
      session$setInputs(add_users_confirm = 2)

      expect_length(grants$calls(), 2)
    },
    grant_fn = grants$fn
  )
})

test_that("no grant is attempted when no user is selected", {
  grants <- recording_grants()

  run_signature_module(
    {
      session$setInputs(signature_tbl_rows_selected = 1, signature_tbl_row_last_clicked = 1)
      session$setInputs(access_btn = 1)
      session$setInputs(user_selector = character(0), access_type_selector = "viewer")
      session$setInputs(add_users_confirm = 1)

      expect_length(grants$calls(), 0)
    },
    grant_fn = grants$fn
  )
})

test_that("the modal offers every access level the client accepts", {
  # match.arg() in SigRepo::addUserToSignature() is the source of truth; the
  # modal must not drift from it.
  expect_setequal(
    SIGNATURE_ACCESS_TYPES,
    eval(formals(SigRepo::addUserToSignature)$access_type)
  )
})
