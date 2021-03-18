#' @title deleteSignature
#' @description Function that executes all required deletess of a signature
#' @param signatureName character vector of the name of the target signature
#' @param connHandle Database connection handle
#' @param thisUser the username of the submitter
#' @param force whether to have a confirmation prompt before removing
#' @export
deleteSignature <- function(signatureName, connHandle, thisUser, force=F){
	confirm <- "NULL"
	acceptable <- c("y","n","c")
	if(force==T){
		confirm<-"y"
	}
	else{
		while(!(tolower(confirm) %in% acceptable)){
			confirm <- readline("Are you sure? [y/n/c]:   ")
		}
		if(tolower(confirm) %in% c("n","c")){
			print("Okay. Canceling.")
			break
		}
	}
	if(tolower(confirm)=="y"){
		sid <- as.integer(
			sqlFindingQuery(
				"signatures", c("signature_id"),
				ins = list("signature_name" = signatureName)
			)$signature_id[1]
		)
		if(!sid){
			stop(
				paste0("No signature exists under the name: ", signatureName)
			)
		}
		else{
			# Remove from keywords
			sqlGeneric(
				paste0("Delete from keyword_signatures where signature_id=",
							 as.character(sid),";"),
				conn=connHandle
			)
			# Remove from collection
			sqlGeneric(
				paste0("Delete from signature_to_collection where signature_id=",
							 as.character(sid),";"),
				conn=connHandle
			)
			# Remove from level2
			sqlGeneric(
				paste0("Delete from feature_signatures where signature_id=",
							 as.character(sid),";"),
				conn=connHandle
			)
			# Finally remove from parent table
			sqlGeneric(
				paste0("Delete from signatures where signature_id=",
												as.character(sid),";"),
				conn=connHandle
			)
			print("Successfully removed")
		}
	}
}