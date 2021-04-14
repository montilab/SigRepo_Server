#'@title addUser
#'@description adds a user to your SigRepo system
#'@param newUserName A character vector with the new username.
#'@param newUserEmail A character vector with the new user's email
#'@param newUserPassword A character vector with the new user's password.
#'Strongly discouraged, will default to input prompts
#'@param connectionHandle your connection handle object. be sure the user
#'has "admin" privileges. 
#'@returns nothing.
#'@export
addUser <- function(newUserName=NULL, newUserEmail=NULL, 
										newUserPassword="NULL", connectionHandle){
	if(is.null(newUserName)){
		newUserName = readline(prompt="Enter your new Username: ")
	}
	if(is.null(newUserEmail)){
		newUserEmail = readline(prompt="Enter your Email address: ")
	}
	if((newUserPassword=="NULL")){
		newUserPasswordConfirm = "0"
		while(newUserPassword != newUserPasswordConfirm){
			newUserPassword = getPass::getPass(msg="Enter your password: ")
			newUserPasswordConfirm = getPass::getPass(msg="Confirm your password: ")
			if(newUserPassword != newUserPasswordConfirm){
				print("Passwords don't match. Try again.")
			}
		}
		
	}
	procedureCall <- paste(
		"CALL add_user(",
		singleQuote(newUserName),",",
		singleQuote(newUserEmail),",",
		singleQuote(newUserPassword),");")
	sqlGeneric(procedureCall, connectionHandle, disconnectAfter = T)
}
