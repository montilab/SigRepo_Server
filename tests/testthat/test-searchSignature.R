# unit testing for the searchSignature function

library(DBI)
library(SigRepo)
library(testthat)



test_that("searchSignature returns all signature with no filter",{
  # using the test connection
  
  sigantures search Signature(conn_handler )
})