# Testing the addSignature function from the SigRepo package


#Loading required packages
library(mockery)
library(rlang)
library(SigRepo)
library(testthat)



test_that("addSignature uploads signatures for all omics types", {
  # Define all assay types and corresponding function stubs
  assay_types <- list(
    transcriptomics     = "SigRepo::addTranscriptomicsSignatureSet",
    proteomics          = "SigRepo::addProteomicsSignatureSet",
    metabolomics        = "SigRepo::addMetabolomicsSignatureSet",
    methylomics         = "SigRepo::addMethylomicsSignatureSet",
    genetic_variations  = "SigRepo::addGeneticVariationsSignatureSet",
    dna_binding_sites   = "SigRepo::addDNABindingSitesSignatureSet"
  )
  
  for (assay_type in names(assay_types)) {
    # --- MOCK conn_handler ---
    conn_handler <- list(host = "localhost", api_port = "8000") # need to be connected to sigrepo, not local host.
    
    # --- MOCK omic_signature ---
    OmicSignatureMock <- R6::R6Class(
      "OmicSignatureMock",
      public = list(
        signature = data.frame(feature_id = c("A", "B"), value = c(1.5, -0.3)),
        difexp = data.frame(feature_id = c("A", "B"), logFC = c(2.1, -1.2)),
        initialize = function() {}
      )
    )
    omic_signature <- OmicSignatureMock$new()
    
    # --- MOCK DATABASE + API BEHAVIOR ---
    mock_conn <- mock(TRUE)
    mock_db_disconnect <- mock(TRUE)
    mock_insert <- mock(TRUE)
    mock_lookup <- mock(
      data.frame(signature_id = "abc123", assay_type = assay_type, organism_id = 9606)
    )
    
    # Stub internal functions
    stub(addSignature, "SigRepo::conn_init", mock_conn)
    stub(addSignature, "SigRepo::checkPermissions", function(...) list(user_role = "editor", user = "testuser", api_key = "abc"))
    stub(addSignature, "SigRepo::createSignatureMetadata", function(...) data.frame(
      signature_name = "sig1",
      has_difexp = FALSE,
      signature_hashkey = "hash123"
    ))
    stub(addSignature, "SigRepo::lookup_table_sql", mock_lookup)
    stub(addSignature, "SigRepo::insert_table_sql", mock_insert)
    stub(addSignature, "SigRepo::checkTableInput", function(...) TRUE)
    stub(addSignature, "DBI::dbDisconnect", mock_db_disconnect)
    stub(addSignature, "SigRepo::addUserToSignature", function(...) TRUE)
    
    # Stub the correct add<Assay>SignatureSet function for this type
    stub(addSignature, assay_types[[assay_type]], function(...) TRUE)
    
    # --- RUN TEST ---
    result <- addSignature(
      conn_handler = conn_handler,
      omic_signature = omic_signature,
      return_signature_id = TRUE,
      verbose = FALSE
    )
    
    # --- ASSERTION ---
    expect_equal(result, "abc123", info = paste("Failed for assay_type:", assay_type))
  }
})
