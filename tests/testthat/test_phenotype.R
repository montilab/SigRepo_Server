# testing script for phenotype function in the database
# addphenotype


library(SigRepo)
library(testthat)
library(DBI)
library(RMySQL)
library(rlang)


# connection handler test

test_that("newConnHandler creates a connection handler correctly",{
  
  test_conn <<- SigRepo::newConnHandler(
    dbname = "sigrepo",
    host = Sys.getenv("HOST"),
    port = as.integer(Sys.getenv("PORT")),
    user = "montilab",
    password = "sigrepo"
  )
  
  # expect statements
  
  expect_true(!is.null(test_conn))
  expect_true(is.list(test_conn))
  expect_true(all(c("dbname", "host", "port","user","password","api_port") %in% names(test_conn)))
  
})


test_that("addPhenotype correctly adds the phenotype into the database",{
  
  #reading in the phenotype data
  
  phenotype_table <- testthat:test_data('test_data', 'test_phenotype.csv')
  
  # sigrepo function
  
  phenotype_msg <- SigRepo::addPhenotype(
    conn_handler = test_conn,
    phenotype_tbl = phenotyp_table,
    verbose = TRUE
  )
  
  expect_equals(phenotype_msg, "Finished uploading")
  
  
})


test_that("searchPhenotype correcty searches for the desired phenotype",{
  
  phenotype_search <- SigRepo::searchPhenotype(
    conn_handler = test_conn,
    phenotype = 'test_phenotype',
    verbose = TRUE
  )
  
  expect_equals(phenotype_search$phenotype[1], "test_phenotype")
})

