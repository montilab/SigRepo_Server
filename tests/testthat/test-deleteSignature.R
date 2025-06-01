# Unit test to verify the deleteSignature function works on all omics types

# This test will;
# delete a known test signature in the database
# verify that the signature has been deleted from the database
# deleting a nonexistent signature will expect an error
# deleting a signature without permission will expect and error




test_that("deleteSignature deletes an existing transcriptomics signature in the database",{
  
  # using the already created test connection handler
  
  # confirming the signatue exists
  transcriptomics_signature <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  expect_true(!is.null(transcriptomics_signature))
  
  
  # deleting the signature
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = transcriptomics_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  
  deleted_sig_transcriptomics <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  expect_null(deleted_result)

})


test_that("delete Signature deletes an existing proteomics signature from the database", {
  
  # using the already created test connection handler
  
  # confirming the signatue exists
  proteomics_signature <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = proteomics_signature_id,
    verbose = FALSE
  )
  
  expect_true(!is.null(proteomics_signature))
  
  
  # deleting the signature
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = proteomics_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  
  deleted_sig_proteomics <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = proteomics_signature_id,
    verbose = FALSE
  )
  expect_null(deleted_result)
})


test_that("deleteSignature deletes an existing metabolomics signature from the database", {
  
  # using the already created test connection handler
  
  # confirming the signatue exists
  metabolomics_signature <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = metabolomics_signature_id,
    verbose = FALSE
  )
  
  expect_true(!is.null(metabolomics_signature))
  
  
  # deleting the signature
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = metabolomics_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  
  deleted_sig_metabolomics <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = metabolomics_signature_id,
    verbose = FALSE
  )
  expect_null(deleted_result)
  
})

test_that("deleteSignature deletes and existing methylomics signature from the database",{
  
  # using the already created test connection handler
  
  # confirming the signatue exists
  methylomics_signature <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = methylomics_signature_id,
    verbose = FALSE
  )
  
  expect_true(!is.null(methylomics_signature))
  
  
  # deleting the signature
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = methylomics_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  
  deleted_sig_methylomics <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = methylomics_signature_id,
    verbose = FALSE
  )
  expect_null(deleted_result)
})

test_that("deleteSignature deletes an existing genetic_variations signature from the database", {
  
  # using the already created test connection handler
  
  # confirming the signatue exists
  genetic_variations_signature <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = genetic_variations_signature_id,
    verbose = FALSE
  )
  
  expect_true(!is.null(genetic_variations_signature))
  
  
  # deleting the signature
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = genetic_variations_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  
  deleted_sig_genetic_variations <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = genetic_variations_signature_id,
    verbose = FALSE
  )
  expect_null(deleted_result)
})

test_that("deleteSignature deletes an existing dna_binding_sites signature from the database", {
  
  # using the already created test connection handler
  
  # confirming the signatue exists
  dna_binding_sites_signature <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = dna_binding_sites_signature_id,
    verbose = FALSE
  )
  
  expect_true(!is.null(dna_binding_sites_signature))
  
  
  # deleting the signature
  
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = dna_binding_sites_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  
  deleted_sig_dna_binding_sites <- SigRepo::getsignature(
    conn_handler = test_conn,
    signature_id = dna_binding_sites_signature_id,
    verbose = FALSE
  )
  expect_null(deleted_result)
})

# test that delete signature will throw an error when signature_id does not exist

test_that("deleteSignature throws and error when signature does not exist", {
  
  fake_id <- "non_existent_id_11122324455"
  
  expect_error(
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = fake_id,
      verbose = FALSE
    ), regexp = "There is no signature_id = 'non_existent_id_11122324455' existed in the 'signatures' table of the SigRepo database."
  )
})
