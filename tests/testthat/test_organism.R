# script to test organism functions for the SigRepo package



# testing connection handler

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

test_that("addOrganism prints success message", {
  
  organism_table <<- read.csv(testthat::test_path('test_data', 'test_organism.csv'))
  
  
    expect_message(SigRepo::addOrganism(conn_handler = test_conn,
                                       organism_tbl = organism_table,
                                       verbose = TRUE), "Finished uploading.")
   
    
    
  
})


test_that("searchOrganism correctly searches for the desired organisms", {
  
  organism_tbl <- SigRepo::searchOrganism(
    conn_handler = test_conn,
    organism = "test_organism",
  )
  
  expect_equal(organism_tbl$organism[1], "test_organism")
})


# delete test organism

 conn <- DBI::dbConnect(
  drv = RMySQL::MySQL(),
  dbname = 'sigrepo', 
  host = 'sigrepo.org', 
  port = 3306, 
  user = 'montilab', 
  password = 'sigrepo'
)


statement <- "DELETE FROM organisms WHERE organism = 'test_organism';"

DBI::dbGetQuery(conn = conn, statement = statement)

DBI::dbDisconnect(conn = conn) 