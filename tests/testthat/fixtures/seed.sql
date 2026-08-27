--
-- Minimal fixture rows for CI-only integration tests.
-- Not for local/production use; api_key values below are throwaway CI fixtures
-- (md5 hashes of fixed strings, chosen only to satisfy the VARCHAR(32) column width).
--
INSERT INTO `users`
  (`user_name`, `user_password_hashkey`, `user_email`, `user_first`, `user_last`,
   `user_affiliation`, `user_role`, `api_key`, `active`, `user_hashkey`)
VALUES
  ('ci_viewer', 'ci_test_password_hash', 'ci_viewer@example.com', 'CI', 'Viewer',
   'CI Fixtures', 'viewer', '0d5f1998a2cbbd765b80fdadffc6c0c2', 1, 'cc065f6b16aed464db7f6b785549cd87'),
  ('ci_admin', 'ci_test_password_hash', 'ci_admin@example.com', 'CI', 'Admin',
   'CI Fixtures', 'admin', '066b4903733734bb359a63c090c85a8b', 1, 'fd54931654fa54a22f1d8ab0f42b06aa');

-- Minimal reference rows + one signature so fetch_signature_context (api/lib/signature.R)
-- can be exercised against a real join across signatures/organisms/phenotypes/
-- sample_types/platforms/signature_feature_set.
INSERT INTO `organisms` (`organism`) VALUES ('CI Test Organism');
INSERT INTO `phenotypes` (`phenotype`) VALUES ('CI Test Phenotype');
INSERT INTO `platforms` (`platform_name`) VALUES ('CI Test Platform');
INSERT INTO `sample_types` (`sample_type`) VALUES ('CI Test Sample Type');

INSERT INTO `signatures`
  (`signature_name`, `organism_id`, `direction_type`, `assay_type`, `phenotype_id`,
   `platform_id`, `sample_type_id`, `user_name`, `visibility`, `signature_hashkey`)
SELECT
  'CI Test Signature',
  (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'),
  'uni-directional',
  'transcriptomics',
  (SELECT phenotype_id FROM phenotypes WHERE phenotype = 'CI Test Phenotype'),
  (SELECT platform_id FROM platforms WHERE platform_name = 'CI Test Platform'),
  (SELECT sample_type_id FROM sample_types WHERE sample_type = 'CI Test Sample Type'),
  'ci_viewer',
  1,
  'ci_test_signature_hashkey_0000';

INSERT INTO `signature_feature_set`
  (`signature_id`, `feature_id`, `probe_id`, `score`, `group_label`, `assay_type`, `sig_feature_hashkey`)
SELECT
  (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'),
  1, 'probe_1', 2.5, 'All Features', 'transcriptomics', 'ci_feature_hashkey_0000000000001'
UNION ALL SELECT
  (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'),
  2, 'probe_2', -1.2, 'All Features', 'transcriptomics', 'ci_feature_hashkey_0000000000002';

-- A second, visible signature (shares probe_1 with the first, for
-- compare_two_signatures/mcp compare_signatures happy-path tests) and a
-- third, hidden signature (visibility = 0, for search visibility tests).
INSERT INTO `signatures`
  (`signature_name`, `organism_id`, `direction_type`, `assay_type`, `phenotype_id`,
   `platform_id`, `sample_type_id`, `user_name`, `visibility`, `signature_hashkey`)
SELECT
  'CI Test Signature 2',
  (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'),
  'uni-directional',
  'transcriptomics',
  (SELECT phenotype_id FROM phenotypes WHERE phenotype = 'CI Test Phenotype'),
  (SELECT platform_id FROM platforms WHERE platform_name = 'CI Test Platform'),
  (SELECT sample_type_id FROM sample_types WHERE sample_type = 'CI Test Sample Type'),
  'ci_viewer',
  1,
  'ci_test_signature_hashkey_0001'
UNION ALL SELECT
  'CI Test Hidden Signature',
  (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'),
  'uni-directional',
  'transcriptomics',
  (SELECT phenotype_id FROM phenotypes WHERE phenotype = 'CI Test Phenotype'),
  (SELECT platform_id FROM platforms WHERE platform_name = 'CI Test Platform'),
  (SELECT sample_type_id FROM sample_types WHERE sample_type = 'CI Test Sample Type'),
  'ci_viewer',
  0,
  'ci_test_signature_hashkey_hidn';

INSERT INTO `signature_feature_set`
  (`signature_id`, `feature_id`, `probe_id`, `score`, `group_label`, `assay_type`, `sig_feature_hashkey`)
SELECT
  (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0001'),
  1, 'probe_1', 1.8, 'All Features', 'transcriptomics', 'ci_feature_hashkey_0000000000003'
UNION ALL SELECT
  (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0001'),
  3, 'probe_3', 0.9, 'All Features', 'transcriptomics', 'ci_feature_hashkey_0000000000004';

-- Collections (mcp search_collections): one visible holding both visible CI
-- signatures, one hidden for visibility tests.
INSERT INTO `collection`
  (`collection_name`, `description`, `user_name`, `visibility`, `collection_hashkey`)
VALUES
  ('CI Test Collection', 'A collection for MCP search_collections tests.', 'ci_viewer', 1, 'ci_collection_hashkey_0001'),
  ('CI Test Hidden Collection', 'Hidden, for visibility tests.', 'ci_viewer', 0, 'ci_collection_hashkey_0002');

INSERT INTO `signature_collection_access`
  (`collection_id`, `signature_id`, `signature_collection_hashkey`)
SELECT
  (SELECT collection_id FROM collection WHERE collection_hashkey = 'ci_collection_hashkey_0001'),
  (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0000'),
  'ci_sig_coll_hashkey_00001'
UNION ALL SELECT
  (SELECT collection_id FROM collection WHERE collection_hashkey = 'ci_collection_hashkey_0001'),
  (SELECT signature_id FROM signatures WHERE signature_hashkey = 'ci_test_signature_hashkey_0001'),
  'ci_sig_coll_hashkey_00002';

-- Gene-set catalog (mcp search_geneset_resources / search_geneset_entries):
-- one current resource with one entry, plus a superseded (is_current = 0)
-- resource so current_only filtering has something to exclude.
INSERT INTO `geneset_resources`
  (`source`, `species`, `collection`, `subcollection`, `version`, `format`, `storage_path`,
   `n_genesets`, `n_features`, `is_current`, `geneset_resource_hashkey`)
VALUES
  ('MSigDB', 'Homo sapiens', 'H', NULL, '2023.2', 'rds', '/genesets/ci_msigdb_h.rds',
   1, 2, 1, 'ci_geneset_resource_hk_01'),
  ('MSigDB', 'Homo sapiens', 'H', NULL, '2022.1', 'rds', '/genesets/ci_msigdb_h_old.rds',
   1, 2, 0, 'ci_geneset_resource_hk_02');

INSERT INTO `geneset_entries`
  (`geneset_resource_id`, `geneset_name`, `description`, `n_features`, `geneset_entry_hashkey`)
SELECT
  (SELECT geneset_resource_id FROM geneset_resources WHERE geneset_resource_hashkey = 'ci_geneset_resource_hk_01'),
  'CI_TEST_HALLMARK_SET', 'A fixture gene set.', 2, 'ci_geneset_entry_hk_0001';

-- Feature reference tables (mcp search_features), one row per supported
-- assay_type plus a metabolomics row reachable through both refmet and an
-- hmdb cross-reference.
INSERT INTO `transcriptomics_features`
  (`feature_name`, `organism_id`, `gene_symbol`, `is_current`, `version`, `feature_hashkey`)
SELECT
  'CI_TEST_GENE', (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'),
  'CITG', 1, 1, 'ci_feat_hashkey_transc_01';

INSERT INTO `proteomics_features`
  (`feature_name`, `organism_id`, `gene_symbol`, `is_current`, `feature_hashkey`)
SELECT
  'CI_TEST_PROTEIN', (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'),
  'CITP', 1, 'ci_feat_hashkey_prot_01';

INSERT INTO `genetic_variants_features`
  (`feature_name`, `chromosome`, `position`, `annotation`, `organism_id`, `is_current`, `version`, `feature_hashkey`)
SELECT
  'rs_ci_test_variant', '1', 12345, 'missense',
  (SELECT organism_id FROM organisms WHERE organism = 'CI Test Organism'), 1, 1, 'ci_feat_hashkey_snp_01';

INSERT INTO `metabolite_reference`
  (`refmet_id`, `refmet_name`, `hmdb_id`, `inchikey`, `smiles`, `is_current`, `version`, `metabolite_hashkey`)
VALUES
  ('RM_CI_0001', 'CI Test Metabolite', 'HMDB9999999', 'CITESTINCHIKEY0000000000', 'CCITESTSMILES',
   1, 1, 'ci_metabolite_hashkey_01');

INSERT INTO `metabolite_xref`
  (`metabolite_id`, `source_db`, `source_value`, `is_primary`, `xref_hashkey`)
SELECT
  (SELECT metabolite_id FROM metabolite_reference WHERE metabolite_hashkey = 'ci_metabolite_hashkey_01'),
  'hmdb', 'HMDB0000001', 1, 'ci_metabolite_xref_hk_01';
