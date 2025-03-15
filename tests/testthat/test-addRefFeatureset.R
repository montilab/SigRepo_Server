library(testthat)
library(DBI)
library(SigRepo)

# Mocked database connection and test data, This one was generated with chat gpt because I didnt know how to implement i it
mock_conn_handler <- "mock_connection_handler"
mock_feature_set <- data.frame(feature = c("Feature1", "Feature2"))

# Test if the function calls the correct function based on assay_type
test_that("addRefFeatureSet calls the correct function based on assay_type", {
  # Mock the functions that will be called for each assay type
  mock_add_transcriptomics <- mock()
  mock_add_proteomics <- mock()
  mock_add_metabolomics <- mock()
  mock_add_methylomics <- mock()
  mock_add_genetic_variations <- mock()
  mock_add_dna_binding_sites <- mock()
  
  # Stub the actual functions to simulate their behavior
  stub(SigRepo, "addTranscriptomicsFeatureSet", mock_add_transcriptomics)
  stub(SigRepo, "addProteomicsFeatureSet", mock_add_proteomics)
  stub(SigRepo, "addMetabolomicsFeatureSet", mock_add_metabolomics)
  stub(SigRepo, "addMethylomicsFeatureSet", mock_add_methylomics)
  stub(SigRepo, "addGeneticVariationsFeatureSet", mock_add_genetic_variations)
  stub(SigRepo, "addDNABindingSitesFeatureSet", mock_add_dna_binding_sites)
  
  # Call addRefFeatureSet with each assay_type and check the appropriate function is called
  
  # Test for 'transcriptomics'
  addRefFeatureSet(mock_conn_handler, assay_type = "transcriptomics", feature_set = mock_feature_set)
  expect_called(mock_add_transcriptomics, 1)
  expect_called(mock_add_proteomics, 0)
  expect_called(mock_add_metabolomics, 0)
  expect_called(mock_add_methylomics, 0)
  expect_called(mock_add_genetic_variations, 0)
  expect_called(mock_add_dna_binding_sites, 0)
  
  # Test for 'proteomics'
  addRefFeatureSet(mock_conn_handler, assay_type = "proteomics", feature_set = mock_feature_set)
  expect_called(mock_add_proteomics, 1)
  expect_called(mock_add_transcriptomics, 0)
  expect_called(mock_add_metabolomics, 0)
  expect_called(mock_add_methylomics, 0)
  expect_called(mock_add_genetic_variations, 0)
  expect_called(mock_add_dna_binding_sites, 0)
  
  # Test for 'metabolomics'
  addRefFeatureSet(mock_conn_handler, assay_type = "metabolomics", feature_set = mock_feature_set)
  expect_called(mock_add_metabolomics, 1)
  expect_called(mock_add_transcriptomics, 0)
  expect_called(mock_add_proteomics, 0)
  expect_called(mock_add_methylomics, 0)
  expect_called(mock_add_genetic_variations, 0)
  expect_called(mock_add_dna_binding_sites, 0)
  
  # Test for 'methylomics'
  addRefFeatureSet(mock_conn_handler, assay_type = "methylomics", feature_set = mock_feature_set)
  expect_called(mock_add_methylomics, 1)
  expect_called(mock_add_transcriptomics, 0)
  expect_called(mock_add_proteomics, 0)
  expect_called(mock_add_metabolomics, 0)
  expect_called(mock_add_genetic_variations, 0)
  expect_called(mock_add_dna_binding_sites, 0)
  
  # Test for 'genetic_variations'
  addRefFeatureSet(mock_conn_handler, assay_type = "genetic_variations", feature_set = mock_feature_set)
  expect_called(mock_add_genetic_variations, 1)
  expect_called(mock_add_transcriptomics, 0)
  expect_called(mock_add_proteomics, 0)
  expect_called(mock_add_metabolomics, 0)
  expect_called(mock_add_methylomics, 0)
  expect_called(mock_add_dna_binding_sites, 0)
  
  # Test for 'dna_binding_sites'
  addRefFeatureSet(mock_conn_handler, assay_type = "dna_binding_sites", feature_set = mock_feature_set)
  expect_called(mock_add_dna_binding_sites, 1)
  expect_called(mock_add_transcriptomics, 0)
  expect_called(mock_add_proteomics, 0)
  expect_called(mock_add_metabolomics, 0)
  expect_called(mock_add_methylomics, 0)
  expect_called(mock_add_genetic_variations, 0)
})

# Test if the function throws an error when an invalid assay_type is passed
test_that("addRefFeatureSet throws an error for invalid assay_type", {
  expect_error(addRefFeatureSet(mock_conn_handler, assay_type = "invalid_assay", feature_set = mock_feature_set),
               "object 'invalid_assay' is not a valid value for 'assay_type'")
})

# Test if the function runs without errors when the data is valid
test_that("addRefFeatureSet works without error for valid data", {
  expect_silent(addRefFeatureSet(mock_conn_handler, assay_type = "transcriptomics", feature_set = mock_feature_set))
})
