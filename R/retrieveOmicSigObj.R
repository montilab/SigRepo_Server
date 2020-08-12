
#' retrieveOmicSigObj()
#'
#' @title retrieve OmicSig obj from VM file system; hard coded
#'
#' @param signatureName 
#' @return OmicSignature object
#'
#' @example
#' retrieveOmicSigObj(signatureName="Cal27_BaP")
#'
retrieveOmicSigObj <-
	function(signatureName) {
		return(readJson(
			paste0(
				'http://sigrepo.bu.edu:3838/challenge_project/',
				'miscellanea/signatures/',
				signatureName,
				'_obj.json'
			)
		))
	}