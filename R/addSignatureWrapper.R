#' @title addSignatureWrapper
#' @description Function that executes all required inserts and uploads of a signature
#' @importFrom xfun file_ext
#' @importFrom utils write.table
#' @importFrom OmicSignature readJson writeJson
#' @param objectFile a json or rds file, formatted in the way of an
#' OmicSignature Object. It can also be a variable that is the object itself.
#' @param thisHandle Database connection handle
#' @param uploadPath where to upload the object file
#' @param thisUser the username of the submitter
#' @export
addSignatureWrapper <-
	function(objectFile,
					 thisHandle,
					 uploadPath=Sys.getenv("signatureDirectory"),
					 thisUser) {
		if (typeof(objectFile) == "character") {
			qceMessage <- objectUploadQC(objectFile)
			if (qceMessage != "") {
				stop(qceMessage)
			}
			extension <- tolower(file_ext(objectFile))
			if (extension == "json") {
				signatureObject <- readJson(objectFile)
				copyFile(objectFile, thisUser)
			}
			else if (extension == "rds") {
				signatureObject <- readRDS(objectFile)
				copyFile(objectFile, thisUser)
			}
			writeSignatureFile(signatureObject, thisUser)
		}
		else if (typeof(objectFile) == "environment") {
			# If you already have the object as a variable
			# in your environment(proper form would be an environment type)
			signatureObject <- objectFile
			writeJson(objectFile,
								paste(
									".",
									paste0(signatureObject$metadata$signature_name, ".json"),
									sep="/"
								))
			command = paste(
				"scp",
				paste0(signatureObject$metadata$signature_name, ".json"),
				paste0(
					thisUser,
					"@",
					Sys.getenv("databaseServer"),
					":/",
					uploadPath,
					"/",
					paste0(signatureObject$metadata$signature_name, ".json")
				)
			)
			print(command)
			system(command)
		}
		else {
			stop("You need to upload with either an RDS file or a JSON file")
		}
		signatureName <- signatureObject$metadata$signatureName
		if (signatureName == "" || is.null(signatureName)) {
			stop("STOP. YOU HAVE VIOLATED THE LAW. need to have a signature name in the metadata")
		}
		if (length(signatureObject[["difexp"]]) > 0) {
			write.table(signatureObject[["difexp"]], 
									paste(uploadPath, paste0(signatureName, "_difexp.tsv"), sep = "/"))
		}
		these_signatures <- signatureObject$signature
		sig_meta <- signatureObject$metadata
		print("adding signature information")
		# if signature meta already in database, don't try to
		# add it again and keep going with the other inserts
		lastSid <-
			as.integer(sqlFindingQuery(
				"signatures",
				c("signature_id"),
				ins = list("signature_name" = sig_meta$signature_name)
			)$signature_id[1])
		if (is.na(lastSid)) {
			addSignature(
				sig_meta$signature_name,
				sig_meta$organism,
				sig_meta$platform,
				singleQuote(sig_meta$cell_lines),
				phenotype = sig_meta$phenotype,
				thisUser,
				uploadHandle = thisHandle,
				verbose = T,
				disconnectAfter = F
			)
		}
		else {
			print("signature meta info already added, moving on")
		}
		# since signature is inserted now, we can get its signature id
		# and feed it into the lvl2/3 upload function
		lastSid <-
			as.integer(sqlFindingQuery(
				"signatures",
				c("signature_id"),
				ins = list("signature_name" = sig_meta$signature_name)
			)$signature_id[1])
		print("added signature info successfully. inserting level2")
		level2DupeCheck <- sqlFindingQuery(
			"feature_signature",
			ins = list(signature_id = lastSid)
		)$signature_id
		if (length(level2DupeCheck) == 0) {
			addLevel2(these_signatures,
								lastSid,
								sig_meta$signature_name,
								thisHandle,
								verbose = T)
		}
		else{
			print("level2 for this signature was already inserted. moving on")
		}
		if (length(signatureObject$metadata$keywords) != 0) {
			keywordSignatureDupeCheck <- sqlFindingQuery(
				"keyword_signature",
				ins = list(signature_id = lastSid))$signature_id
			if (length(keywordSignatureDupeCheck) == 0) {
				addSignatureKeywords(signatureObject$metadata$keywords,
														 lastSid,
														 thisHandle)
			}
			else{
				print("keyword-signature pairs for this signature were already added.")
			}
			print("Signature-Keyword Pairs Added.")
		}
		print("finished inserting this signature")
	}
