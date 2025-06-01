# Unit Testing to validate the addUser Function



library(testthat)
library(SigRepo)
library(DBI)


test_that("addUser works with valid input", {
  
  test_user <- data.frame(
    user_name = "testuser1",
    user_password = "password123",
    user_email = "testuser1@gmail.com",
    user_first = "test"
    user_last = "user",
    user_affiliation = "testing sect",
    user_role = "admin",
    stringsAsFactors = FALSE
  )
  
  expect_silent(addUser(test_conn, test_user, verbose = FALSE))
})

test_that("addUser throws error with missing required fields", {
  
  
  bad_user <- data.frame(
    user_name = "testuser2",
    user_password = "password132",
    user_first = "test",
    user_last = "bad",
    user_affiliation = "testing sect",
    stringsAsFactors = FALSE
  )
  
  expect_error(addUser(test_conn, badser, verbose = FALSE),
               "The table is missing the following required column names: user_email, user_role.")))
})


test_that("addUser throws error for invalid role", {
  conn_handler <- SigRepo::newConnhandler(
    dbname = "testdb",
    host = "localhost",
    user = "test_admin",
    password = "test_password"
  )
  
  invalid_role_user <- data.frame(
    user_name = "testuser4",
    user_password = "SecurePass123!",
    user_email = "testuser4@example.com",
    user_first = "Test",
    user_last = "User",
    user_affiliation = "TestOrg",
    user_role = "invalidrole",
    stringsAsFactors = FALSE
  )
  
  expect_error(
    addUser(conn_handler, invalid_role_user, verbose = FALSE),
    "user_role' column must contain one of the following roles"
  )
})
