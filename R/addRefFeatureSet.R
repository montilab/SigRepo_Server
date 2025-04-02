#' @title addRefFeatureSet
#' @description Add reference feature set into database
#' @param conn_handler An established connection to database using newConnhandler() 
#' @param assay_type Type of assays: transcriptomics, proteomics, metabolomics, 
#' methylomics, genetic_variations, dna_binding_sites
#' @param feature_set A data frame containing appropriate column names
#' @param verbose a logical value indicates whether or not to print the
#' diagnostic messages. Default is \code{TRUE}.

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
    SigRepo::addMetabolomicsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "methylomics"){
    SigRepo::addMethylomicsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "genetic_variations"){
    SigRepo::addGeneticVariationsFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }else if(assay_type == "dna_binding_sites"){
    SigRepo::addDNABindingSitesFeatureSet(conn_handler = conn_handler, feature_set = feature_set, verbose = verbose)
  }

}



