#' @title addRefFeatureSet
#' @description Add reference feature set to database
#' @param conn An established connection to database using newConnhandler() 
#' @param assay_type Type of assays: transcriptomics, proteomics, metabolomics, 
#' methylomics, genetic_variations, DNA_binding_sites
#' @param feature_set A data frame containing appropriate column names
#' @export
addRefFeatureSet <- function(
    conn,
    assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                   "genetic_variations", "dna_binding_sites"),
    feature_set
){
  
  # Check assay_type
  assay_type <- match.arg(assay_type)
  
  if(assay_type == "transcriptomics"){
    addTranscriptomicsFeatureSet(conn = conn, feature_set = feature_set)
  }else if(assay_type == "proteomics"){
    addProteomicsFeatureSet(conn = conn, feature_set = feature_set)
  }else if(assay_type == "metabolomics"){
    addMetabolomicsFeatureSet(conn = conn, feature_set = feature_set)
  }else if(assay_type == "methylomics"){
    addMethylomicsFeatureSet(conn = conn, feature_set = feature_set)
  }else if(assay_type == "genetic_variations"){
    addGeneticVariationsFeatureSet(conn = conn, feature_set = feature_set)
  }else if(assay_type == "dna_binding_sites"){
    addDNABindingSitesFeatureSet(conn = conn, feature_set = feature_set)
  }
  
}



