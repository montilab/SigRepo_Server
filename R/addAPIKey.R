#' @title addAPIKey
#' @description Add an API Key for one or more users in the database
#' @param conn An established database connection using newConnhandler() 
#' @param user_id A list of user_ids in the database
#' @export
addAPIKey <- function(
    conn,
    user_id,
    user_email
){
  
  # Define a table in the database
  table <- "users"
  
  # Check database connection and table name in database
  checkTable(conn = conn, table = table)

  # Check user_id 
  stopifnot("'user_id' cannot be empty." = 
              (length(user_id) > 0 && !user_id %in% c(NA, "")))
  
  # Make sure users are existed in the database
  all_users_hashkey <- seq_along(user_id) %>% 
    purrr::map2_dfr(
      function(u){
        #u=1;
        
        # Check if user exists in the database
        SigRepoR::checkTableVar(
          conn = conn,
          table = table,
          return_var = "user_id",
          filter_coln_var = "user_id",
          filter_coln_val = user_id[u]
        ) 
        
        # Create a sodium hash for api_key
        api_hashkey <- sodium::password_store(paste0(user_id, "-", Sys.Date()))
        
        # Update users with new api hash key
        SigRepoR::update_table_sql(
          conn = conn,
          table = table,
          update_coln_var = "api_key",
          update_coln_val = api_hashkey,
          filter_coln_var = "user_id",
          filter_coln_val = user_id[u]
        )
        
      }
    )
  
}



