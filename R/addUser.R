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
