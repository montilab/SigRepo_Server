library(testthat)
library(SigRepo)
library(digest)
library(DBI)
library(mockery)

# Mocked objects and data
mock_conn_handler <- "mock_connection_handler"
mock_omic_signature <- list(
  difexp = "mock_difexp_data",  # assuming difexp is a dataframe or list
  signature = "mock_signature_data"
)

# Mock metadata table
mock_metadata_tbl <- data.frame(
  signature_name = "Test Signature",
  organism_id = 1,
  direction_type = "upregulated",
  assay_type = "transcriptomics",
  phenotype_id = 1,
  user_name = "user1",
  signature_hashkey = "mock_hashkey",
  has_difexp = TRUE
)

# Test for signature already existing in the database
test_that("addSignature shows warning when signature exists", {
  # Mock database lookup to return a result indicating the signature exists
  stub(SigRepo, "lookup_table_sql", function(conn, db_table_name, return_var, filter_coln_var, filter_coln_val, check_db_table) {
    return(data.frame(signature_hashkey = "mock_hashkey"))  # Simulating signature existence
  })
  
  expect_warning(
    addSignature(mock_conn_handler, mock_omic_signature),
    "You already uploaded a signature with similar contents into the SigRepo Database."
  )
})

# Test for successful signature insertion and difexp saving
test_that("addSignature inserts metadata and saves difexp for new signature", {
  # Mock functions to simulate successful insertion and difexp saving
  stub(SigRepo, "conn_init", function(conn_handler) "mock_connection")
  stub(SigRepo, "checkPermissions", function(conn, action_type, required_role) TRUE)
  
  # Mock creating the signature metadata
  stub(SigRepo, "createSignatureMetadata", function(conn_handler, omic_signature) mock_metadata_tbl)
  
  # Mock database table check and insertion
  stub(SigRepo, "checkTableInput", function(conn, db_table_name, table, exclude_coln_names, check_db_table) table)
  stub(SigRepo, "insert_table_sql", function(conn, db_table_name, table, check_db_table) NULL)
  
  # Mock difexp saving
  stub(base, "saveRDS", function(object, file) NULL)
  
  # Mock lookup to simulate signature ID retrieval
  mock_signature_id <- 123
  stub(SigRepo, "lookup_table_sql", function(conn, db_table_name, return_var, filter_coln_var, filter_coln_val, check_db_table) {
    return(data.frame(signature_id = mock_signature_id))
  })
  
  # Mock adding user to signature access
  stub(SigRepo, "addUserToSignature", function(conn_handler, signature_id, user_name, access_type) NULL)
  
  # Mock adding signature set to the database
  stub(SigRepo, "addTranscriptomicsSignatureSet", function(conn_handler, signature_id, organism_id, signature_set) NULL)
  
  # Run the function and check for success (no errors or warnings)
  expect_silent(addSignature(mock_conn_handler, mock_omic_signature))
})

# Test for error handling during signature insertion
test_that("addSignature handles errors during signature insertion", {
  # Simulate an error during adding signature set to the database
  stub(SigRepo, "addTranscriptomicsSignatureSet", function(conn_handler, signature_id, organism_id, signature_set) {
    stop("Error adding signature set to the database")
  })
  
  # Mock lookup for signature ID
  mock_signature_id <- 123
  stub(SigRepo, "lookup_table_sql", function(conn, db_table_name, return_var, filter_coln_var, filter_coln_val, check_db_table) {
    return(data.frame(signature_id = mock_signature_id))
  })
  
  # Expect the error to be caught and the signature to be deleted
  expect_error(addSignature(mock_conn_handler, mock_omic_signature),
               "Error adding signature set to the database")
  
  # Ensure that the signature is deleted after the error
  expect_called(SigRepo, "deleteSignature", times = 1)
})

# Test that the database disconnects correctly
test_that("addSignature disconnects from the database", {
  # Create a mock for DBI::dbDisconnect
  mock_db_disconnect <- mock()
  stub(DBI, "dbDisconnect", mock_db_disconnect)
  
  # Mock all relevant database functions
  stub(SigRepo, "conn_init", function(conn_handler) "mock_connection")
  stub(SigRepo, "checkPermissions", function(conn, action_type, required_role) TRUE)
  stub(SigRepo, "createSignatureMetadata", function(conn_handler, omic_signature) mock_metadata_tbl)
  stub(SigRepo, "checkTableInput", function(conn, db_table_name, table, exclude_coln_names, check_db_table) table)
  stub(SigRepo, "insert_table_sql", function(conn, db_table_name, table, check_db_table) NULL)
  stub(base, "saveRDS", function(object, file) NULL)
  stub(SigRepo, "lookup_table_sql", function(conn, db_table_name, return_var, filter_coln_var, filter_coln_val, check_db_table) {
    return(data.frame(signature_id = 123))
  })
  stub(SigRepo, "addUserToSignature", function(conn_handler, signature_id, user_name, access_type) NULL)
  stub(SigRepo, "addTranscriptomicsSignatureSet", function(conn_handler, signature_id, organism_id, signature_set) NULL)
  
  # Run the function and check if dbDisconnect is called
  addSignature(mock_conn_handler, mock_omic_signature)
  expect_called(mock_db_disconnect, 1)
})
