#' @title userApproval
#' 
#' @description allows an admin to approve giving a user of interest write
#' privileges to the database of interest by calling the stored procedure
#' "approve_deny_user". You can either approve, which will
#' grant these permissions and add them to the "users" table, or reject, which
#' will remove the user from the "user_requests" table. Upon successful
#' approval, the request will also be deleted. The stored procedure, 
#' "approve_deny_user", itself, takes two arguments: the username and
#' {1,0} for {approve, reject}.
#' @param userName the proposed user's username
#' @param verdict Boolean or {1,0}. TRUE(T) for approve; FALSE(F) for reject. can also 
#' use {1,0}, respectively.
#' @param connHandle your DB connection handle. Needed because an admin is
#' the only one who's (supposed to be) allowed to do this
#' @export
userApproval <- function(userName, verdict, connHandle){
	assertthat::assert_that(typeof(verdict)=="logical" || verdict %in% c(0,1))
	# if the verdict is boolean, just multiply by 1 to make numeric
	verdict = verdict * 1
	tryCatch({
		sqlGeneric(
			paste0(
				"CALL approve_deny_user(",
				singleQuote(userName),
				verdict
			),
			conn = connHandle,
			disconnectAfter = F
		)
		print("Approval process completed.")
	},
	error=function(e){
		print(e)
	})
}