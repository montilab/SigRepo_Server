# tests/testthat/test-newConnHandler.R ####
# loading required packages ####
library(testthat)
library(dotenv)
library(SigRepo)

test_that("newConnHandler returns correct structure based on .env", {
  # Load the .env file explicitly
  dotenv::load_dot_env(file = ".env")
  
  # Grab expected values from environment
  expected_dbname   <- Sys.getenv("DB_NAME")
  expected_host     <- Sys.getenv("DB_HOST")
  expected_port     <- as.integer(Sys.getenv("DB_PORT"))
  expected_api_port <- as.integer(Sys.getenv("API_PORT"))
  expected_user     <- Sys.getenv("DB_USER")
  expected_password <- Sys.getenv("DB_PASSWORD")
  
  handler <- newConnHandler()
  
  expect_type(handler, "list")
  expect_named(handler, c("dbname", "host", "port", "api_port", "user", "password"))
  expect_equal(handler$dbname, expected_dbname)
  expect_equal(handler$host, expected_host)
  expect_equal(handler$port, expected_port)
  expect_equal(handler$api_port, expected_api_port)
  expect_equal(handler$user, expected_user)
  expect_equal(handler$password, expected_password)
})
