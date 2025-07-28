#' @title RefCheck
#' @description Queries Ensembl API to cross-reference Ensembl feature_name_ids in batches of 1000
#' @param signature_object An omic signature object.
#' @param assay A string indicating the assay type (e.g., "transcriptomics")
#' @return A data frame of outdated or unresolvable feature_name IDs, if there are any(with optional replacements)
#' @examples
#' Refcheck(OmS_ex_sig, assay = "transcriptomics")
#' @keywords internal
#' @export
Refcheck <- function(signature_object, assay) {
  # Load required packages
  if (!requireNamespace("httr", quietly = TRUE)) stop("Package 'httr' is required.")
  if (!requireNamespace("jsonlite", quietly = TRUE)) stop("Package 'jsonlite' is required.")
  
  # Extract feature names depending on how signature object is structured
  if (!"signature" %in% names(signature_object)) {
    stop("signature_object must contain a 'signature' element.")
  }
  
  if (!"feature_name" %in% colnames(signature_object$signature)) {
    stop("signature_object$signature must contain a 'feature_name' column.")
  }
  
  ids <- unique(signature_object$signature$feature_name)
  ids <- ids[!is.na(ids)]  # Remove NAs
  
  if (length(ids) == 0) {
    stop("No feature_name IDs found in the signature.")
  }
  
  # Transcriptomics Ref Check ####
  
  if (assay == "transcriptomics") {
    server <- "https://rest.ensembl.org"
    ext <- "/archive/id"
    url <- paste0(server, ext)
    
    split_batches <- split(ids, ceiling(seq_along(ids) / 1000))
    all_results <- list()
    
    for (i in seq_along(split_batches)) {
      batch_ids <- split_batches[[i]]
      
      response <- httr::POST(
        url,
        httr::content_type("application/json"),
        httr::accept("application/json"),
        body = jsonlite::toJSON(list(id = batch_ids), auto_unbox = TRUE)
      )
      
      httr::stop_for_status(response)
      result_df <- jsonlite::fromJSON(httr::content(response, as = "text", encoding = "UTF-8"))
      
      if (is.data.frame(result_df)) {
        result_df <- result_df
      } else {
        result_df <- as.data.frame(result_df)
      }
      
      all_results[[i]] <- result_df
    }
    
    final_df <- do.call(rbind, all_results)
    
    # Clean and process results
    cols_to_remove <- c("version", "type", "latest")
    final_df <- final_df[ , !(names(final_df) %in% cols_to_remove), drop = FALSE]
    
    # Convert is_current to logical
    if (!is.logical(final_df$is_current)) {
      final_df$is_current <- as.logical(as.integer(final_df$is_current))
    }
    
    # Flatten replacement IDs if nested
    if ("possible_replacement" %in% names(final_df)) {
      final_df$possible_replacement <- vapply(final_df$possible_replacement, function(x) {
        if (is.null(x) || length(x) == 0) {
          return(NA_character_)
        } else if (is.data.frame(x) && "id" %in% names(x)) {
          return(paste(x$id, collapse = ","))
        } else {
          return(as.character(x))
        }
      }, character(1))
    }
    
    # Filter for deprecated or unresolved IDs
    filtered_df <- final_df[is.na(final_df$is_current) | final_df$is_current == FALSE, , drop = FALSE]
    
    if (nrow(filtered_df) == 0) {
      message("All provided feature_name IDs are current.")
    } else {
      message("️ Some feature_name IDs are deprecated or no longer resolvable.")
    }
    
    return(filtered_df)
    
  } else {
    message(" No external reference check performed for assay type: ", assay)
    return(NULL)
  }
}
