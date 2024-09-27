#' @title getTable
#' @description Get signatures to database
#' @param conn An established connection to database using newConnhandler() 
#' @param author_id An author id used to submit the signature
#' @export
getTable <- function(
    conn,
    table,
    return_var
){
  
  # Get table from the database
  table <- SigRepoR::lookup_var_sql(
    conn = conn,
    table = table,
    return_var = return_var,
    filter_coln_var = NULL,
    filter_coln_val = NULL,
    filter_var_by = NULL
  )  
  
  if(nrow(table) == 0){
    return(
      data.frame(WARNING="There is no data returned from the search paramters")
    )
  }else{
    return(table)
  }

}







