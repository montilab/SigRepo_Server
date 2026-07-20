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
