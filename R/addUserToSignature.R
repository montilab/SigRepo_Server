#' @title addUserToSignature
#' @description Add user to signature access table in database
#' @param conn An established database connection using newConnhandler() 
#' @param signature_id signature id 
#' @param user_id user id
#' @param access_type access type: 'owner' or 'user'
#' @export
addUserToSignature <- function(
    conn,
    signature_id,
    user_id,
    access_type = c("owner", "user")
){
  
  # Check user connection and permission ####
  conn_info <- SigRepo::checkPermissions(
    conn = conn, 
    action_type = "INSERT",
    required_role = "user"
  )
  
  # Check access_type
  access_type <- match.arg(access_type)  
  
  # Check signature_id
  stopifnot("'signature_id' cannot be empty." = 
              (length(signature_id) == 1 && !signature_id %in% c(NA, "")))
  
  # Check user_id
  stopifnot("'user_id' cannot be empty." = 
              (length(user_id) == 1 && !user_id %in% c(NA, "")))
  
  # Get table name in database
  db_table_name <- "signature_access" 
  table <- data.frame(
    signature_id = signature_id,
    user_id = user_id,
    access_type = access_type,
    stringsAsFactors = FALSE
  )
  
  # Create a hash key to look up values in database ####
  table <- SigRepo::createHashKey(
    table = table,
    hash_var = "access_sig_hashkey",
    hash_columns = c("signature_id", "user_id", "access_type"),
    hash_method = "md5"
  )

  # Check table against database table ####
  table <- SigRepo::checkTableInput(
    conn = conn, 
    db_table_name = db_table_name,
    table = table, 
    exclude_coln_names = "access_signature_id",
    check_db_table = TRUE
  )
  
  # Remove duplicates from table before inserting into database ####
  table <- SigRepo::removeDuplicates(
    conn = conn,
    db_table_name = db_table_name,
    table = table,
    coln_var = "access_sig_hashkey",
    check_db_table = FALSE
  )

  # Insert table into database ####
  SigRepo::insert_table_sql(
    conn = conn, 
    db_table_name = db_table_name, 
    table = table,
    check_db_table = FALSE
  )  
  
}



