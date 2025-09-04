#' @title addRefFeatureSet
#' @description Add reference feature set to database
#' @param conn_handler A handler uses to establish connection to the database 
#' obtained from SigRepo::newConnhandler() (required) 
#' @param assay_type Type of assays: transcriptomics, proteomics, metabolomics, 
#' methylomics, genetic_variations, dna_binding_sites (required)
#' @param feature_set A data frame containing appropriate column names (required):
#' transcriptomics: feature_name, organism, gene_symbol, is_current, version
#' proteomics: feature_name, organism, gene_symbol, is_current, version
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.
#' 
#' @export
addRefFeatureSet <- function(
    conn_handler,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    feature_set,
    verbose = TRUE
){
  
  # Whether to print the diagnostic messages
  SigRepo::print_messages(verbose = verbose)
  
  # Check assay_type
  assay_type <- base::match.arg(assay_type)
  
  # Add reference feature set for a specified assay type
  if(assay_type == "transcriptomics"){
    SigRepo::addTranscriptomicsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "proteomics"){
    SigRepo::addProteomicsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "metabolomics"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
    # SigRepo::addMetabolomicsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "methylomics"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
    #SigRepo::addMethylomicsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "genetic_variations"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
    #SigRepo::addGeneticVariationsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "dna_binding_sites"){
    SigRepo::showAssayTypeErrorMessage(unknown_values = assay_type)
    #SigRepo::addDNABindingSitesFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }
  
}



