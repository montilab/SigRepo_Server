# unit testing for retrieving a signature in the database


library(SigRepo)
library(testthat)
library(DBI)


test_that("getSignature retrieves uploaded transcriptomics signature by id", {
  
  # using the already created test_data
  result_by_id <- getSignature(
    conn_handler = test_conn,
    signature_id = transcriptmics_signature_id,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(result_by_id, "list")
  # 
  
  
  
})

test_that("getSignature retrieves uploaded proteomics signature by id", {
  
  # using the already created test_data
  result_by_id <- getSignature(
    conn_handler = test_conn,
    signature_id = proteomics_signature_id,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(result_by_id, "list")
  # 
  
  
  
})

test_that("getSignature retrieves uploaded metabolomics signature by id and name", {
  
  # using the already created test_data
  result_by_id <- getSignature(
    conn_handler = test_conn,
    signature_id = metabolomics_signature_id,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(result_by_id, "list")
  # 
  
  
  
})

test_that("getSignature retrieves uploaded methylomics signature by id and name", {
  
  # using the already created test_data
  result_by_id <- getSignature(
    conn_handler = test_conn,
    signature_id = transcriptmics_signature_id,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(result_by_id, "list")
  # 
  
  
  
})

test_that("getSignature retrieves uploaded genetic_variations signature by id and name", {
  
  # using the already created test_data
  result_by_id <- getSignature(
    conn_handler = test_conn,
    signature_id = genetic_variations_signature_id,
    verbose = FALSE
  )
  
  
 
  
  
})


test_that("getSignature retrieves uploaded dna_binding_sites signature by id and name", {
  
  # using the already created test_data
  result_by_id <- getSignature(
    conn_handler = test_conn,
    signature_id = dna_binding_sites_signature_id,
    return_signature_id = TRUE,
    verbose = FALSE
  )
  
  expect_type(result_by_id, "list")
  # 
  
  
  
})



