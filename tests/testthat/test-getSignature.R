library(testthat)
library(mockery)
library(SigRepo)

test_that("getSignature returns filtered OmicSignature objects with correct names", {
  # Mocks
  mock_conn <- mock("conn")
  mock_disconnect <- mock(TRUE)
  mock_permissions <- mock(list(user_role = "viewer", user = "testuser"))
  
  # Simulated signature table
  mock_signature_table <- data.frame(
    signature_id = c("sig001", "sig002"),
    signature_name = c("MySig1", "MySig2"),
    visibility = c(TRUE, FALSE),
    organism_id = c(1, 1),
    phenotype_id = c(10, 10),
    sample_type_id = c(100, 100),
    stringsAsFactors = FALSE
  )
  
  # Simulated joined tables
  mock_organisms <- data.frame(organism_id = 1, organism = "Homo sapiens")
  mock_phenotypes <- data.frame(phenotype_id = 10, phenotype = "disease")
  mock_sample_types <- data.frame(sample_type_id = 100, sample_type = "tissue")
  
  # Mock return for signature_access (only one signature is visible to this viewer)
  mock_signature_access <- data.frame(signature_id = "sig002", user_name = "testuser", access_type = "viewer")
  
  # Create a dummy omic signature to return
  dummy_omic <- list(new("OmicSignature"))  # Could be a simple R6 mock too
  
  # Stub SigRepo internals
  stub(getSignature, "SigRepo::conn_init", mock_conn)
  stub(getSignature, "SigRepo::checkPermissions", mock_permissions)
  stub(getSignature, "SigRepo::lookup_table_sql", function(conn, db_table_name, ...) {
    switch(db_table_name,
           signatures = mock_signature_table,
           signature_access = mock_signature_access,
           organisms = mock_organisms,
           phenotypes = mock_phenotypes,
           sample_types = mock_sample_types,
           stop("unexpected table")
    )
  })
  stub(getSignature, "SigRepo::createOmicSignature", function(conn_handler, db_signature_tbl) {
    # Return mock OmicSignature object named by signature_name
    structure(list(signature_name = db_signature_tbl$signature_name), class = "OmicSignature")
  })
  stub(getSignature, "DBI::dbDisconnect", mock_disconnect)
  stub(getSignature, "SigRepo::print_messages", function(...) NULL)
  stub(getSignature, "SigRepo::verbose", function(...) NULL)
  
  # Test call
  conn_handler <- list(host = "localhost", api_port = "8000")
  result <- getSignature(conn_handler)
  
  # Assertions
  expect_type(result, "list")
  expect_length(result, 2)
  expect_true(all(sapply(result, function(x) inherits(x, "OmicSignature"))))
  expect_named(result, c("MySig1", "MySig2"))
})

test_that("getSignature returns NULL when no matches", {
  mock_conn <- mock("conn")
  mock_disconnect <- mock(TRUE)
  mock_permissions <- mock(list(user_role = "viewer", user = "testuser"))
  
  # Empty signature table
  empty_signature_table <- data.frame(
    signature_id = character(0),
    signature_name = character(0),
    visibility = logical(0),
    organism_id = integer(0),
    phenotype_id = integer(0),
    sample_type_id = integer(0)
  )
  
  stub(getSignature, "SigRepo::conn_init", mock_conn)
  stub(getSignature, "SigRepo::checkPermissions", mock_permissions)
  stub(getSignature, "SigRepo::lookup_table_sql", function(...) empty_signature_table)
  stub(getSignature, "DBI::dbDisconnect", mock_disconnect)
  stub(getSignature, "SigRepo::print_messages", function(...) NULL)
  stub(getSignature, "SigRepo::verbose", function(...) NULL)
  
  conn_handler <- list(host = "localhost", api_port = "8000")
  result <- getSignature(conn_handler)
  
  expect_null(result)
})
