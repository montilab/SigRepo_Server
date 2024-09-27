#' @title addUser
#' @description Add user information to database
#' @param conn An established database connection using newConnhandler() 
#' @param user_tbl A data frame containing appropriate column names:
#' user_id, user_password, user_email, user_first, user_last, user_affiliation, user_role
#' @export
addUser <- function(
    conn,
    user_tbl
){
  
  # Check connection
  conn_info <- SigRepoR::checkConnection(conn = conn)
  
  # Create a list of variables to check database
  database <- conn_info$dbname
  db_table_name <- "users"
  table <- user_tbl
  require_tbl_colnames <- c("user_id", "user_password", "user_email", "user_role")
  include_tbl_colnames <- c("user_password")
  exclude_db_colnames <- c("user_password_hashkey", "api_key")
  
  # Check if table exists in database
  table <- SigRepoR::checkTableInput(
    conn = conn, 
    database = database,
    db_table_name = db_table_name,
    table = table,
    require_tbl_colnames = require_tbl_colnames,
    include_tbl_colnames = include_tbl_colnames,
    exclude_db_colnames = exclude_db_colnames
  ) %>% 
    dplyr::mutate(
      user_password_hashkey = sodium::password_store(as.character(user_password)),
      api_key = sodium::password_store(paste0(user_id, "-", Sys.Date()))
    )
  
  # GRANT USER PERMISSION TO DATABASE
  purrr::walk(
    seq_len(nrow(table)),
    function(u){
      #u=1;      
      check_user_tbl <- DBI::dbGetQuery(conn = conn, statement = sprintf("SELECT host, user FROM mysql. user WHERE user IN ('%s') AND host IN ('168.122.76.140');", table$user_id[u]))
      if(nrow(check_user_tbl) == 0 && table$user_role[u] == "admin"){
        DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'168.122.76.140' IDENTIFIED BY '%s';", table$user_id[u], table$user_password[u]))
        DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT ALL PRIVILEGES ON * . * TO '%s'@'168.122.76.140';", table$user_id[u]))
        DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")
      }else if(nrow(check_user_tbl) == 0 && table$user_role[u] == "user"){
        DBI::dbGetQuery(conn = conn, statement = sprintf("CREATE USER '%s'@'168.122.76.140' IDENTIFIED BY '%s';", table$user_id[u], table$user_password[u]))
        DBI::dbGetQuery(conn = conn, statement = sprintf("GRANT SELECT ON * . * TO '%s'@'168.122.76.140';", table$user_id[u]))
        DBI::dbGetQuery(conn = conn, statement = "FLUSH PRIVILEGES;")        
      }
    }
  )
                       
  # Get SQL statement
  statement <- SigRepoR::insert_table_sql(conn = conn, db_table_name = db_table_name, table = table %>% dplyr::select(-user_password))
  
  # Insert table into database
  tryCatch({
    DBI::dbGetQuery(conn = conn, statement = statement)
  }, error = function(e){
    stop(e, "\n")
  }, warning = function(w){
    message(w, "\n")
  })
  
}



