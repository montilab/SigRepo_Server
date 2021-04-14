#' @title requestUser
#' @description adds a request for a new user to have write access
#' to the database that they're pointing to. This function adds a record to the
#' "user_requests" table, which will then be reviewed by an administrator.
#' The record will not be added if:
#' ...There is already an existing user under that username
#' ...There is already a request under that username
#' @param newUserName string denoting your requested user name
#' @param newUserEmail string denoting the new user's email
#'
#' @export
requestUser <- function(newUserName=NULL, newUserEmail=NULL){
	if(is.null(newUserName)){
		newUserName = readline(prompt="Enter your new Username: ")
	}
	if(is.null(newUserEmail)){
		newUserEmail = readline(prompt="Enter your Email address: ")
	}
	#initialize password control
	newUserPassword = getPass::getPass(msg="Enter your password: ")
	newUserPasswordConfirm = getPass::getPass(msg="Confirm your password: ")
	while(newUserPassword != newUserPasswordConfirm){
		print("Passwords don't match. Try again.")
		newUserPassword = getPass::getPass(msg="Enter your password: ")
		newUserPasswordConfirm = getPass::getPass(msg="Confirm your password: ")
	}
	tryCatch({
		sqlGeneric(
			paste0(
				"CALL request_user(",
				paste(
					singleQuote(newUserName),
					singleQuote(newUserEmail),
					singleQuote(newUserPassword), 
					sep=","
				),
				");"	
			)
		)
		print("Request successfully sent.")
	}, 
	error=function(e){
		print(e)
		}
	)
}