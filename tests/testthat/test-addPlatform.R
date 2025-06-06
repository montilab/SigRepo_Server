library(testthat)
library(DBI)
library(SigRepo)
library(mockery)

# Mocked database connection and test data, need to figure out how to get the conn handler to work
mock_conn_handler <- "mock_connection_handler"
mock_platform_tbl <- data.frame(
  platform_id = c("P1", "P2"),
  platform_name = c("Platform1", "Platform2"),
  seq_technology = c("Technology1", "Technology2"),
  organisms = c("Organism1", "Organism2")
)

# Test if the function throws an error when a required column is missing
test_that("addPlatform throws error when required column is missing", {
  # Create a table without the required 'platform_id' column
  incomplete_table <- data.frame(platform_name = c("Platform1", "Platform2"))
  
  expect_error(addPlatform(mock_conn_handler, incomplete_table), 
               "the table is missing the following required column names: platform_id")
})

# Test if the function throws an error when required column has NA values
test_that("addPlatform throws error when required column has NA values", {
  # Create a table with NA values in the 'platform_id' column
  table_with_na <- data.frame(platform_id = c("P1", NA),
                              platform_name = c("Platform1", "Platform2"),
                              seq_technology = c("Technology1", "Technology2"),
                              organisms = c("Organism1", "Organism2"))
  
  expect_error(addPlatform(mock_conn_handler, table_with_na), 
               "All required column names: platform_id cannot contain any empty values.")
})

# Test if the function runs without errors when the data is valid
test_that("addPlatform works without error for valid data", {
  expect_silent(addPlatform(mock_conn_handler, mock_platform_tbl))
})

# Test database interaction (mocking functions like `SigRepo::conn_init`, `SigRepo::insert_table_sql`, etc.)
test_that("addPlatform interacts with database functions as expected", {
  # Mock the database interaction functions (use mock functions for testing)
  
  # Mock SigRepo::conn_init function to return a mock connection
  mock_conn <- mock("mock_connection", class = "connection")
  stub(SigRepo, "conn_init", function(conn_handler) mock_conn)
  
  # Mock the `SigRepo::checkPermissions` to simulate permission check success
  stub(SigRepo, "checkPermissions", function(conn, action_type, required_role) TRUE)
  
  # Mock the database functions that will be called within addPlatform
  stub(SigRepo, "checkTableInput", function(conn, db_table_name, table, exclude_coln_names, check_db_table) table)
  stub(SigRepo, "removeDuplicates", function(conn, db_table_name, table, coln_var, check_db_table) table)
  stub(SigRepo, "insert_table_sql", function(conn, db_table_name, table, check_db_table) NULL)
  
  # Run the function and check if it works without error
  expect_silent(addPlatform(mock_conn_handler, mock_platform_tbl))
})

# Test if the database disconnect is called after function execution
test_that("addPlatform disconnects from the database", {
  # Create a mock for DBI::dbDisconnect
  mock_db_disconnect <- mock()
  stub(DBI, "dbDisconnect", mock_db_disconnect)
  
  # Run the function
  addPlatform(mock_conn_handler, mock_platform_tbl)
  
  # Check if dbDisconnect was called
  expect_called(mock_db_disconnect, 1)
})
