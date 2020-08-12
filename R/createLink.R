#' Create a link to a signature file based on the signature name
#'
#' @param signatureName the name of the signature to link to
createLink <-
	function(signatureName) {
		paste0(
			'<a href="http://sigrepo.bu.edu:3838/',
			'signatures/',
			signatureName,
			'_obj.json',
			'"',
			'target="_blank">',
			signatureName,
			'</a>'
		)
	}
