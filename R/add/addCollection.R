#' @title addCollection
#' @description add an OmicSignature Collection into the database
#' @param conn An established connection to database using newConnhandler() 
#' @param OmSC A Signature collection object to be added to the database
#' @export


addCollection(conn, OmSC){

    SigRepoR::checkPermissions(
        conn = conn, 
        action_type = "INSERT",
        required_role = c('admin', 'editor') #TBD
    )
}

# wouldnt this be just grabbing the list of signatures in the collection and using addSignature for each one?

# also need to update collection tables too