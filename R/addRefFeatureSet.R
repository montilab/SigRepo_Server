
addRefFeatureSet <- function(
  conn,
  assay_type = c("transcriptomics", "proteomics", "metabolomics", "methylomics",
                 "genetic_variations", "DNA_binding_sites", "others"),
  feature_set
){
  
  # Check assay_type
  assay_type <- match.arg(assay_type)
  
  # Name of table in database
  table <- ifelse(assay_type %in% c("transcriptomics", "proteomics"), 
                  "transcriptomics_proteomics_features", paste0(assay_type, "_features"))
  
  # Check if table exists in database
  all_tables <- tryCatch({
    DBI::dbGetQuery(conn = conn, statement = "show tables;")
  }, error = function(e){
    stop(e)
  }, warning = function(w){
    message(w, "\n")
  })
  
  if(!table %in% all_tables[,1])
    stop(sprintf("There is no '%s' table in the database.", table))
  
  # Check if feature_set is a data frame object
  if(!is(feature_set, "data.frame"))
    stop("'feature_set' must be a data frame object")

  if(assay_type == "transcriptomics" || assay_type == "proteomics"){
    addTranscriptomicsFeatureSet(conn=conn, feature_set=feature_set)
  }else if(assay_type == "metabolomics"){
    addMetabolomicsFeatureSet(conn=conn, feature_set=feature_set)
  }else if(assay_type == "methylomics"){
    addMethylomicsFeatureSet(conn=conn, feature_set=feature_set)
  }else if(assay_type == "genetic_variations"){
    addGeneticVariationsFeatureSet(conn=conn, feature_set=feature_set)
  }else if(assay_type == "DNA_binding_sites"){
    addDNABindingSitesFeatureSet(conn=conn, feature_set=feature_set)
  }else if(assay_type == "others"){
    addOthersFeatureSet(conn=conn, feature_set=feature_set)
  }
  
}



