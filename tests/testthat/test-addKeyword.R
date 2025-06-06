# Load necessary libraries
library(DBI)
library(SigRepo)
library(testthat)
library(mockery)



# Test function with a mock connection handler
test_that("addOrganism adds organisms to the database correctly", {
  
  # Create a mock connection handler as a list that mimics the real connection object
  mock_conn_handler <- list(
    dbname = "test_db", 
    host = "localhost", 
    port = 5432, 
    user = "test_user", 
    password = "test_password"
  )
  
  # Mock the behavior of conn_init (to return the mock connection handler)
  mock_conn_init <- mock(mock_conn_handler)  # Return the mock connection handler
  
  # Stub the conn_init function to return our mock connection handler
  stub(addOrganism, "SigRepo::conn_init", mock_conn_init)
  
  # Mock the input data (organism table)
  organism_tbl <- data.frame(organism = c("organism1", "organism2"))
  
  # Run the actual function (which will use the mocked connection)
  result <- addOrganism(mock_conn_handler, organism_tbl)
  
  # Check that the mock connection handler was used
  expect_called(mock_conn_init, 1)  # Expect that conn_init was called once
  
  # Verify that the function completes as expected
  expect_true(result)  # In this case, addOrganism is returning TRUE
})
