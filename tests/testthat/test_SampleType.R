# testing script for the SampleType functions in the SigRepo package

library(SigRepo)
library(testthat)
library(DBI)
library(RMySQL)
library(rlang)


# just use a test for one, not all omics types ####

test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <<- SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = Sys.getenv("HOST"),  # grabbing the env variables so this can run on local machine and server version
    port = as.integer(Sys.getenv("PORT")), 
    user = "montilab", # account for testing
    password = "sigrepo" 
  )
  
  # expects, the test_conn is a list object of 6 elements
  
  expect_true(!is.null(test_conn))
  expect_true(is.list(test_conn))
  expect_true(all(c("dbname", "host", "port", "user", "password", "api_port") %in% names(test_conn)))
  
  

})

test_that("addSampleType correctly adds the desired sample type into the database",{
  
  # loading in SampleType Data
  sample_data <- read.csv(testthat::test_path("test_data", "test_sampletype.csv"))
  
  sample_msg <- SigRepo::addSampleType(
    conn_handler = conn_handler,
    sample_type_tbl = sample_data,
    verbose = TRUE
  )
  
  expect_equal(sample_msg, "Finished uploading.")
  
})

test_that("searchSampleType correctly searches for the desired sample type", {
  
  sample_table <- SigRepo::searchSampleType(
    conn_handler = test_conn,
    sample = "test_sample"
  )
  
  expect_equal(sample_table$sample_type[1], "test_sample")
})