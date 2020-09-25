library(R6)
library(dplyr)

#### OmicSigObj ####

#' @title OmicSignature R6 object
#' @description a R6 object to store signatures generated from experiments. In cluding metadata, signature, and an optional differential expression matrix.
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
#' @export

OmicSignature <-
  R6Class(
    "OmicSignature",
    list(
      metadata = NULL,
      signature = NULL,
      difexp = NULL,
      verbose = function(v, ...) {
        if (v) cat(...)
      },
      initialize = function(metadata, signature, difexp = NULL, print_message = FALSE) {
        if (!is.null(difexp)) {
          difexp <- self$checkDifexp(difexp, v = print_message)
        }
        self$metadata <- self$checkMetadata(metadata, v = print_message)
        self$signature <- self$checkSignature(signature, signatureType = metadata$direction_type, v = print_message)
        self$difexp <- difexp
        cat(paste(
          "  [Success] OmicSignature object",
          self$metadata$signature_name, "created.\n"
        ))
      },
      print = function(...) {
        cat("Signature Object: \n")
        cat("  Metadata: \n")
        sh <- mapply(function(k, v) {
          cat("   ", k, "=", paste(v, collapse = ", "), "\n")
        }, names(self$metadata), self$metadata)
        cat("  signature: \n")
        sh <- mapply(
          function(k, v) {
            cat("    ", k, " (", v, ")", "\n", sep = "")
          }, names(summary(self$signature$signature_direction)),
          summary(self$signature$signature_direction)
        )
        cat("  Differential Expression Data: \n")
        cat("    ", nrow(self$difexp), " x ", ncol(self$difexp), "\n", sep = "")
        invisible(self)
      },
      extract.signature = function(conditions) {
        if (is.null(self$difexp)) {
          stop("Error: Difexp matrix not found in OmicSignature object.")
        }
        v <- rlang::parse_exprs(conditions)
        res <- self$difexp %>%
          dplyr::filter(!!!v) %>%
          dplyr::select(symbol, score) %>%
          dplyr::mutate(direction = ifelse(score < 0, "-", "+")) %>%
          dplyr::arrange(direction)
        res <- res[complete.cases(res), ]
        res <- res[res$symbol != "", ]
        res <- res[res$score != "", ]
        res <- res[order(abs(as.numeric(as.character(res$score))), decreasing = T), ]
        res <- distinct(res, res$symbol, .keep_all = T)[, c(1:3)]
        return(res)
      },
      checkMetadata = function(metadata, v = FALSE) {
        ## metadata should be a list with required attributes
        if (is(metadata, "OmicSignature")) {
          metadata <- metadata$metadata
        }
        if (is(metadata, "list")) {
          self$verbose(v, "  Metadata: Checked; is a list. \n")
        } else {
          stop("Metadata not found or metadata is not a list. ")
        }
        metadataRequired <- c(
          "signature_name", "organism", "platform",
          "direction_type", "phenotype"
        )
        metadataMissing <- setdiff(metadataRequired, names(metadata))
        self$verbose(v, paste("  --Required attributes for metadata: ",
          paste(metadataRequired, collapse = ", "), " --\n",
          sep = ""
        ))

        if (length(metadataMissing) == 0) {
          self$verbose(v, paste("  Metadata: Checked; contains all the essential attributes. \n"))
        } else {
          stop("Metadata does not contain attribute(s): ",
            paste(metadataMissing, collapse = ", "),
            ". This can cause problem when retriving data.",
            sep = ""
          )
        }

        # check if sample_type is a valid BRENDA term
        if (!is.null(metadata$sample_type)) {
          tempSampleType <- try(SigRepoR::BRENDACurrentName(metadata$sample_type), silent = T)
          if (is(tempSampleType, "character")) {
            metadata$sample_type <- tempSampleType[2]
          } else {
            warning(paste("sample_type in metadata is not a valid BRENDA ontology term. Ignore this message if you intend to input other sample types, such as animal strains.",
              "  Otherwise, please consider using BRENDASearch() function to search for the correct BRENDA ontology term to use.",
              sep = "\n"
            ))
          }
        }

        # check if platform is a valid GPL platform
        if (!metadata$platform %in% GEOplatform$Accession) {
          warning("platform in metadata is not a valid GEO platform accession. Please see `View(GEOplatform)`.")
        }

        self$verbose(v, "  [Success] Metadata is saved. \n")
        metadata <- metadata[order(names(metadata))]
        return(metadata)
      },
      checkSignature = function(omicObj, signatureType = NULL, category_num = 0, v = FALSE) {
        ## category_num is used for multi-directional signature
        ## read the signature, and check if it is a dataframe:
        if (is(omicObj, "OmicSignature")) {
          signature <- omicObj$signature
          signatureType <- omicObj$metadata$direction_type
          if (signatureType == "multi-directional") {
            if (!is.null(omicObj$metadata$category_num)) {
              categoryNum <- omicObj$metadata$category_num
            } else {
              stop("Signature is specified as multi-directional, but sample number not found.")
            }
          }
        }
        else if (is(omicObj, "data.frame")) {
          signature <- omicObj
          remove(omicObj)

          ## change possible column names to standard column names:
          colnames(signature) <- colnames(signature) %>%
            tolower() %>%
            dplyr::recode(
              "signature" = "signature_symbol", "symbol" = "signature_symbol",
              "name" = "signature_symbol", "score" = "signature_score",
              "weight" = "signature_score", "direction" = "signature_direction"
            )
        } else {
          stop("Signature not found in OmicSignature object, or signature is not a dataframe.")
        }

        if (nrow(signature) == 0) {
          stop("Signature is empty.")
        }
        ## check if signature_symbol and signature_score (lv2 or lv3) exists:
        if (!c("signature_symbol") %in% colnames(signature)) {
          stop("Signature dataframe does not contain \"signature_symbol\" column.")
        }
        if (c("signature_score") %in% colnames(signature)) {
          self$verbose(v, "  Signature: contains lv2 data. \n")
        } else {
          warning("Feature score not found, please make sure column
                    \"signature_score\" presents if you have score for the features. \n")
        }

        ## check if the direction match with signature type:
        if (is.null(signatureType)) {
          stop("Signature type not specified. It needs to be uni-, bi- or multi-directional.")
        }
        ## each signature type need to be "else if". because if none of the
        ## type meet the criteria required, we need an "else" to output error.

        ## bi-directional signature:
        if (signatureType == "bi-directional") {
          if (!"signature_direction" %in% colnames(signature)) {
            stop("Signature is specified as bi-directional but \"signature_direction\" information not found.")
          }
          ## change direction symbol to + and - :
          colnames(signature) <- colnames(signature) %>%
            tolower() %>%
            dplyr::recode(
              "signature" = "signature_symbol", "symbol" = "signature_symbol",
              "name" = "signature_symbol", "score" = "signature_score",
              "weight" = "signature_score", "direction" = "signature_direction"
            )
          signature$signature_direction <- signature$signature_direction %>%
            tolower() %>%
            dplyr::recode("up" = "+", "dn" = "-", "down" = "-")
          signature$signature_direction <- as.factor(signature$signature_direction)

          ## check direction:
          summaryDirection <- summary(signature$signature_direction)
          if (isTRUE(all.equal(c("-", "+"), names(summary(signature$signature_direction))))) {
            self$verbose(v, "  Signature: Checked, signature is bi-directional
                        with - (Dn) and + (Up) directions. \n")
          } else if (names(summaryDirection) == c("-") | names(summaryDirection) == c("+")) {
            self$verbose(v, "  Signature: Checked, signature is bi-directional,
                        but only one direction is found. \n")
          } else {
            stop("Direction info in bi-directional signature is not valid.
                        Direction should be marked with \"-\" and \"+\".")
          }
        }

        ## uni-directional signature:
        else if (signatureType == "uni-directional") {
          if (!"signature_direction" %in% colnames(signature)) {
            self$verbose(v, "  Checked. signature is uni-directional. \n")
          } else {
            warning("signature is specified as uni-directional but
                        additional direction information found. \n")
          }
        }

        ## multi-directional signature:
        else if (signatureType == "multi-directional") {
          summaryDirection <- summary(signature$signature_direction)
          if (length(summaryDirection) == categoryNum) {
            self$verbose(v, paste("  Checked. signature is multi-directional with",
              category_num, "samples. \n",
              sep = " "
            ))
          } else {
            warning("Sample number in metadata does not match with the number of
                        categories in the signature.")
          }
        }

        ## if none of the signature type is met:
        else {
          stop("Error: Signature information invalid.")
        }
        self$verbose(v, "  [Success] Signature is valid. \n")
        return(signature)
      },

      checkDifexp = function(difexp, v = FALSE) {
        if (is(difexp, "OmicSignature")) {
          difexp <- difexp$difexp
        }
        if (!is(difexp, "data.frame")) {
          stop("Input difexp is not a dataframe. ")
        }
        ## check if it's empty:
        if (nrow(difexp) == 0) {
          stop("Differential Matrix (lv1 data) is empty. ")
        } else {
          self$verbose(v, "  difexp: Checked. Differential Matrix (lv1 data)
                    is a data frame. \n")
        }

        ## check column names:
        difexpColRequired <- c("probe_id", "symbol", "score", "p_value", "fdr")
        if ("q_value" %in% colnames(difexp)) {
          difexpColRequired <- c("probe_id", "symbol", "score", "p_value", "q_value")
        }
        if ("id" %in% colnames(difexp)) {
          difexpColRequired <- c("id", "symbol", "score", "p_value", "fdr")
        }
        difexpColMissing <- setdiff(difexpColRequired, colnames(difexp))
        difexpColAdditional <- setdiff(colnames(difexp), difexpColRequired)
        self$verbose(v, paste(
          "  --Required columns for Differential Matrix (lv1 data): ",
          paste(difexpColRequired, collapse = ", "), " --\n",
          sep = ""
        ))

        if (length(difexpColMissing) == 0) {
          self$verbose(v, "  difexp: Checked. Differential Matrix (lv1 data)
                    contain all the essential columns. \n")
        } else {
          stop("Differential Matrix (lv1 data) does not contain required column(s): ",
            paste(difexpColMissing, collapse = ", "), ".",
            sep = ""
          )
        }
        if (length(difexpColAdditional) > 0) {
          self$verbose(v, paste("  difexp: additional columns found: ",
            paste(difexpColAdditional, collapse = ", "), ". \n",
            sep = ""
          ))
        }
        ## check column type:
        ## "logfc","score","p_value","fdr" should be numerical
        for (difexpColNumeric in c("logfc", "score", "p_value", "fdr", "q_value", "aveexpr")) {
          if (difexpColNumeric %in% colnames(difexp)) {
            if (is(difexp[, difexpColNumeric], "numeric")) {
              self$verbose(v, paste("  difexp: Checked.", difexpColNumeric, "is numeric. \n"))
            } else {
              stop(paste("difexp:", difexpColNumeric, "is not numeric."))
            }
          }
        }
        ## "symbol" should be character
        if ("symbol" %in% colnames(difexp)) {
          if (is(difexp$symbol, "character") | is(difexp$symbol, "factor")) {
            difexp$symbol <- as.character(difexp$symbol)
            self$verbose(v, "  difexp: Checked. symbol is character. \n")
          } else {
            stop("difexp: signature symbol is not character.")
          }
        }
        self$verbose(v, "  [Success] difexp matrix is valid. \n")
        return(difexp)
      }
    )
  )

#### OmicSigCollection ####
#' @title OmicSignatureCollection R6 object
#' @description a R6 object to store a collection of OmicSignature objects.
#' In cluding metadata, OmicSigList which is a list of OmicSignature object.
#'
#' @importFrom R6 R6Class
#' @importFrom dplyr filter pull %>%
#'
#' @export
OmicSignatureCollection <- R6Class(
  "OmicSignatureCollection",
  list(
    metadata = NULL,
    OmicSigList = NULL,
    verbose = function(v, ...) {
      if (v) cat(...)
    },
    initialize = function(metadata, OmicSigList, print_message = FALSE) {
      self$metadata <- self$checkCollectionMetadata(metadata, v = print_message)
      self$OmicSigList <- self$checkCollectionOmicSigList(OmicSigList, v = print_message)
      # a list of OmicSig Obj, contains everything
      names(self$OmicSigList) <- mapply(function(k) {
        k$metadata$signature_name
      }, self$OmicSigList)
      cat(paste(
        "  [Success] OmicSignature Collection",
        self$metadata$collection_name, "created.\n"
      ))
    },
    print = function(...) {
      cat("Signature Collection: \n")
      cat("  Metadata: \n")
      sh <- mapply(function(k, v) {
        cat("   ", k, "=", v, "\n")
      }, names(self$metadata), self$metadata)
      cat("  OmicSignature Objects: \n")
      cat("   ", paste(names(self$OmicSigList), collapse = "\n    "))
      cat("\n  Available Difexp columns: \n")
      sh <- mapply(function(k) {
        cat("   ", k$metadata$signature_name, " (", paste(
          {
            if (is.null(k$difexp)) {
              "* NULL *"
            } else {
              colnames(k$difexp)
            }
          },
          collapse = ", "
        ), ") \n")
      }, self$OmicSigList)
      invisible(self)
    },
    extract.signature = function(conditions, bind = TRUE) {
      a <- mapply(function(x) {
        try_temp <- try(x$extract.signature(conditions), silent = TRUE)
        if (class(try_temp) == "try-error") {
          cat(paste("  Warning: OmicSignature", x$metadata$signature_name, "does not have diffexp matrix or does not have the specified column. \n"))
          try_temp <- NULL
        }
        return(try_temp)
      }, self$OmicSigList)
      if (class(a) == "matrix") {
        a <- apply(a, 2, function(x) {
          data.frame(matrix(unlist(x), nrow = length(x[[1]]), byrow = F), stringsAsFactors = F)
        })
      }
      if (class(a) == "list") {
        a <- lapply(a, function(x) {
          if (class(x) == "data.frame" && nrow(x) > 0) {
            colnames(x) <- c("symbol", "score", "direction")
            x$direction <- as.character(x$direction)
          } else {
            x <- NULL
          }
          return(x)
        })
      }
      if (bind) {
        res <- bind_rows(a, .id = "sig_name")
        if (is(res, "data.frame") && nrow(res) > 0) {
          colnames(res) <- c("sig_name", "symbol", "score", "direction")
          res <- res[order(res$score, decreasing = T), ]
        } else {
          res <- NULL
        }
        return(res)
      } else {
        return(a)
      }
    },
    metadataSummary = function(only_shared = TRUE) {
      if (only_shared == TRUE) {
        col <- Reduce(intersect, sapply(OmicCol$OmicSigList, function(x) {
          names(x$metadata)
        }))
      } else {
        col <- Reduce(union, sapply(OmicCol$OmicSigList, function(x) {
          names(x$metadata)
        }))
      }
      sapply(self$OmicSigList, function(x) {
        x$metadata[col]
      }, simplify = T)
    },
    checkCollectionMetadata = function(metadata, v = FALSE) {
      ## metadata should be a list with required attributes
      if (class(metadata)[1] == "OmicSignatureCollection") {
        metadata <- metadata$metadata
      }
      if (class(metadata) == "list") {
        self$verbose(v, "  Metadata: Checked; is a list. \n")
      } else {
        stop("Metadata not found or metadata is not a list. ")
      }

      metadataRequired <- c(
        "collection_name", "description"
      )
      self$verbose(v, paste("  --Required attributes for metadata: ",
        paste(metadataRequired, collapse = ", "), " --\n",
        sep = ""
      ))
      metadataMissing <- setdiff(metadataRequired, names(metadata))
      if (length(metadataMissing) == 0) {
        self$verbose(v, paste("  Metadata: Checked; contains all the essential attributes. \n"))
      } else {
        stop(
          "Metadata for this Collection does not contain attribute(s): ",
          paste(metadataMissing, collapse = ", "),
          ". Please check your input."
        )
      }
      self$verbose(v, "  [Success] Metadata is saved. \n")
      metadata <- metadata[order(names(metadata))]
      return(metadata)
    },
    checkCollectionOmicSigList = function(OmicSigList, v = FALSE) {
      ## OmicSigList should be a list of OmicSig object
      if (class(OmicSigList)[1] == "OmicSignatureCollection") {
        OmicSigList <- OmicSigList$OmicSigList
      }
      if (class(OmicSigList) == "list") {
        self$verbose(v, "  OmicSigList: Checked; is a list. \n")
      } else {
        stop("OmicSigList not found or OmicSigList is not a list. ")
      }

      if (length(OmicSigList) == 0) {
        stop("OmicSigList is empty. ")
      }

      # re-create all your input OmicSignature Objects to make sure everything is valid.
      for (OmicObj in OmicSigList) {
        if (class(OmicObj)[1] != "OmicSignature") {
          stop("Element in OmicSigList is not an OmicSignature object. Please check your input.")
        }
        OmicObj <- OmicSignature$new(
          metadata = OmicObj$metadata,
          signature = OmicObj$signature,
          difexp = OmicObj$difexp,
          print_message = v
        )
        self$verbose(v, paste(" - OmicObj", OmicObj$metadata$signature_name, "checked. \n"))
      }
      return(OmicSigList)
    }
  )
)
