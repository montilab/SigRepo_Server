# script to test the platform functions for SigRepo
# addPlatform
# searchPlatform




# connection handler testing


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

# Testing the add Platform works

# loading in test data 

test_that("addPlatforms correctly adds the desired platform into the database",{

platforms_table <- read.csv(testthat::test_path("test_data", "test_platform.csv"))

# add platform function

expect_message(
  SigRepo::addPlatform(
    conn_handler = test_conn,
    platform_tbl = platforms_table,
    verbose = TRUE
  ),
  regexp = "Finished uploading\\."
)
})

test_that("searchPlatforms correctly searches for the desired platform",{
  
  platforms_search <- SigRepo::searchPlatform(
    conn_handler = test_conn,
    platform_id = "test_platform",
    verbose = TRUE
  )
  
  expect_equal(platforms_search$platform_id[1], "test_platform")
})


