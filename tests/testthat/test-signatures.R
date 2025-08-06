# Unit testing the core workflow of the SigRepo package
# addSignature
# deleteSignature
# updateSignature
# getSignature
# searchSignature


# just use a test for one, not all omics types ####

test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <<- SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = "sigrepo.org",  # grabbing the env variables so this can run on local machine and server version
    port = 3306,
    user = "montilab", # account for testing
    password = "sigrepo" 
  )
  
  # expects, the test_conn is a list object of 6 elements
  
  expect_true(!is.null(test_conn))
  expect_true(is.list(test_conn))
  expect_true(all(c("dbname", "host", "port", "user", "password", "api_port") %in% names(test_conn)))
  
  
  
})


# testing for Transcriptomic Signature ### 
test_that("addSignature correctly adds a transcriptomic signature into the database", {
  
  
  
  # loading in testing omic signature object ####
  
  test_path <- base::system.file("tests/test_data", package = "SigRepo") # using Test Data
  
  test_transcriptomics_sig <- readRDS(testthat::test_path("test_data", "test_data_transcriptomics.RDS"))
  
  
  # calling the function to add the test signature into the database ####
  
  # using <<- to add the id globally so the tests can use it ####
  transcriptomics_signature_id <<- SigRepo::addSignature(
    conn_handler = test_conn,
    omic_signature = test_transcriptomics_sig,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  # check that the function returned a signature id ###
  expect_type(transcriptomics_signature_id, "double") # the signature id is a double which is confusing I may need to look into this.
  expect_true(nchar(transcriptomics_signature_id) > 0)
  
  
  
  
})

test_that("getSignature properly retrieves the added transcriptomic signature from the database", {
  
  
  # retrieving the signature
  retrieved_signature <- SigRepo::getSignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  # check that the retrieved signature is not NULL
  expect_true(!is.null(retrieved_signature))
  
  # check that the retrieved signature matches the original test signature, need to fix this one test
  # expect_equal(retrieved_signature$omic_type, "Transcriptomic")
  
})

test_that( "searchSignature accurately searches for the provided signature in the database", {
  
  # searching for the signature by id
  search_result <- SigRepo::searchSignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  # check that the search result is not NULL
  expect_true(!is.null(search_result))
  
  # check that the search result contains the expected signature id
  expect_equal(search_result$signature_id, transcriptomics_signature_id)
  
  
})

test_that("updateSignature updates the old data with the new data in the database",{
  
  # using the revised transcriptomics signature
  
  test_path <- base::system.file("tests/test_data", package = "SigRepo") # using Test Data
  
  test_transcriptomics_sig_revised <- readRDS(testthat::test_path("test_data", "test_data_transcriptomics_revised.RDS"))
  
  # updating the signature in the database
  
  expect_no_error({
    updated_signature_id <- SigRepo::updateSignature(
      conn_handler = test_conn,
      signature_id = transcriptomics_signature_id,
      omic_signature = test_transcriptomics_sig_revised,
      verbose = FALSE
    )
  })
  
  
  
})

test_that("deleteSignature deleted the provided signature from the database along with its difexp file", {
  
  # deleting the signature
  expect_no_error({
    SigRepo::deleteSignature(
      conn_handler = test_conn,
      signature_id = transcriptomics_signature_id,
      verbose = FALSE
    )
  })
  
  # confirm it is deleted from the database
  deleted_result <- SigRepo::getSignature(
    conn_handler = test_conn,
    signature_id = transcriptomics_signature_id,
    verbose = FALSE
  )
  
  expect_null(deleted_result)
})