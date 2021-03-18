addSignatureToQueue <- function(signature_name, signatureObject, verdict=NULL, verbose=F){
	query=paste0(
		"INSERT INTO sigrepo.signature_queue(signature_name) VALUES(",
		singleQuote(signature_name),");"
	)
	if(!is.null(verdict)){
		query=paste0(
			"INSERT INTO sigrepo.signature_queue(signature_name, verdict) VALUES(",
			singleQuote(signature_name),",", verdict, ");"
		)
	}
	if(verbose){
		print(query)
	}
	sqlGeneric(
		query
	)
	thisPath = "sigrepo.bu.edu:3838/signatures/queue/"
	writeJson(signatureObject, 
						paste0("/data_files/signatures/queue/",
									 signature_name, "_obj.json"))
}
