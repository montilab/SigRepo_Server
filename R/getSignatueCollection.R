#' @title getOmicSignatureCollection
#' @description Get the signature set uploaded by a specific user in the database.
#' @param conn_handler A handler uses to establish connection to the database
#' obtained from SigRepo::newConnhandler() (required)
#' @param signature_collection The name of a OmicSignatureCollection object that belongs to a specific user 
#' (\code{user_name}) who previously uploaded the signature into the database (required. 
#' 
#' 
#' @export
#' @import OmicSignature


getOmicSignatureCollection <- function(
    conn_handler,
    db_signature_tbl
){

    conn_info <- SigRepo::checkPermissions(
        conn_handler = conn_handler,
        action_type = "SELECT",
        required_role = "admin",
    )
    # check if the signature collection name is a valid name within the database.
    
    # grabbing user role
    user_role <- conn_info$user_role[1]

    # grabbing user_name

    user_name <- conn_info$user[1]

    if(user_role != 'admin'){

        collection_access_tbl <- SigRepo::lookup_table_sql(
            conn = conn_info$conn,
            db_table_name = "collection_access",
            return_var = "*",
            filter_coln_var = c("user_name", "access_type"),
            filter_coln_val = list("user_name" = user_name, access_type = c("owner", "editor")),
            filter_var_by = "AND",
            check_db_table = TRUE
        )

        # if user does not have owner or editor perms then throw error.

        if(nrow(collection_access_tbl) == 0){
            base::stop(sprintf("there are no signatures that belon to User = '%s' in the database.\", user_name"))

            # disconnect from the database

            DBI::dbDisconnect(conn_info$conn)
        }
    }
    
    # lookup collections


    collection_tbl <- SigRep::lookup_table_sql(
        conn = conn_info$conn,
        db_table_name = "signature_collection",
        return_var = "*",
        filter_coln_var = c("collection_id, user_name"),
        filter_coln_val = "AND",
        check_db_table = TRUE
    )


} else {

    # lookup collections

    collection_tbl <- SigRepo::lookup_table_sql(
        conn = conn_info$conn,
        db_table_name = "signature_collection",
        return_var = "*",
        check_db_table = TRUE
    )
}

# get a list of filtered variables

for (r in base::seq_along(filter_var)){
    #r=1;
    filer_status <- ifelf(length(filter_val[r]) == 0 ||)
}

