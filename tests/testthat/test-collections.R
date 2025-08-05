# testing scripts for the signature collection functions in the SigRepo package




# test connection 

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


test_that("addCollection correctly adds the signature collection into the database", {
  
  # loading in the data
  test_collection <- base::readRDS(testthat:test_data("test_data", "test_collection.RDS"))
  
  # using the SigRepo function
  
  collection_id <<- SigRepo::addCollection(
    conn_handler = test_conn,
    omic_collection = test_collection,
    return_collection_id = TRUE,
    verbose = TRUE
  )
    
  # check that the function returned a signature id ###
  expect_type(collection_id, "double") # the signature id is a double which is confusing I may need to look into this.
  expect_true(nchar(collection_id) > 0)
  
})

test_that("searchCollection correctly searches for the desired signature collection", {
  
 collection_table <-  SigRepo::searchCollection(
    conn_handler = test_conn,
    collection_id = collection_id,
    verbose = TRUE
  )
})