# Unit test for addSignature Function

library(SigRepo)
library(testthat)
library(DBI)



test_that("addSignature correctly adds a transcriptomic signature into the database", {
  
  
  # creating connectin handler
  
  test_conn <- SigRepo::setup_test_db.R
  
  
  
  # loading in testing omic signature
  
  test_path <- base::system.file("tests/test_data", package = "SigRepo")
  
  test_transcriptomics_sig <- base::readRDS(base::file.path(test_path, "test_data_transcriptomics.RDS"))
  
  # calling the function to add the test signature into the database
  
  transcriptomics_signature_id <- SigRepo::addsignature(
    conn_handler = test_conn,
    omic_signature = test_transcriptomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  # check that the function returned a signature id
  expect_type(transcriptomics_signature_id, "character")
  expect_true(nchar(transcriptomics_signature_id) > 0)
  
  
  # verify directly in the database that the signature has been uploaded
  
  result <- DBO::dbGetQuery(
    test_conn,
    "SELECT * FROM signature WHERE signature_id = '%s'",
    transcriptomics_signature_id
  )
  

  
})


test_that("addSignature correctly adds a proteomics signature into the database". {
  

  test_proetomics_sig <- base::readRDS(base::file.path(test_path, "test_data_proteomics.RDS"))
  
  # calling the function to add the test signature into the database
  proteomics_signature_id <- SigRepo::addSignature(
    conn_handler = test_conn,
    omic_signature = test_proteomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(signture_id, "character")
  expect_true(nchar(signature_id) > 0)
  
  # verify directly in the database that the signature has been uploaded
  
  result <- DBI::dbGetQuery(
    test_conn,
    "SELECT * FROM signature WHERE signature_id = '%s'",
    signature_id
  )
  

})


test_that("addSignature correctly adds a metabolomics signature into the database", {
  
  test_metabolomics_sig <- base::readRDS(base::file.path(test_path, "test_data_metabolomics.RDS")) # need to find test data for this
  
  metabolomics_signature_id <- SigRepo::addSignature(
    conn_handler = test_conn,
    omic_signature = test_metabolomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  ecpect_type(signature_id, "character")
  expect_true(nchar(signature_id) > 0)
  
  # verify directly in the database that the signature has been uploaded in the database
  
  result <- DBI::dbGetQuery(
    test_conn,
    "SELECT * FROM signature WHERE signature_id = '%s'",
    metabolomics_signature_id
  )
})


test_that("addSignature correctly adds a methylomics signature into the database"), {
  methylomics_signature_id <- SigRepo::addSignature(
    conn_handler = test_conn,
    omic_signature = test_methylomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(signature_id, "character")
  expect_true(nchar(signature_id)
  
  # verify directly in the database that the signature has been uploaded in the database
  
  result <- DBI::dbGetQuery(
    test_conn,
    "SELECT * FROM signature WHERE signature_id = '%s'",
    methylomics_signature_id
  )
  
  
}

test_that("addSignature correctly adds a genetic_variations signature into the database", {
  genetic_varations_test_sig <- SigRepo:addSignature(
    conn_handler = test_conn,
    omic_signature = test_genetic_variations_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(signature_id, "character")
  expect true(nchar(signature_id))
  
  # verify directly in the database that the signature has been uploaded in the database
  result <- DBI::dbGetQuery(
    test_conn,
    "SELECT * FROM signature WHERE signature_id = '%s'",
    genetic_variations_signature_id
  )
})

test_that("addSignature correctly adds a dna_binding_sites signature into the database", {
  dna_binding_sites_signature_id <- SigRepo::addSignature(
    conn_handler = test_conn,
    omic_signature = test_dna_binding_sites_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(signature_id, "character")
  expect_true(nchar(signature_id) > 0)
  
  # verify directly in the database that the signature has been uploaded in the database
  
  result <- DBI::dbGetQuery(
    test_conn,
    "SELECT * FROM signature WHERE signature_id = '%s'",
    dna_binding_sites_signature_id
  )
})

