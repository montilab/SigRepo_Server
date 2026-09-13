--
-- Table structure for `signature_feature_set`
--
-- Mirrors the production table (#78). There is deliberately no key on
-- (signature_id, group_label, probe_id): one array probe or aptamer can map to
-- several features, so the identity of a row is the feature under its probe.
CREATE TABLE `signature_feature_set` (
  `signature_id` INT UNSIGNED NOT NULL,
  `feature_id` INT UNSIGNED DEFAULT NULL,
  `probe_id` VARCHAR(255) DEFAULT NULL,
  `score` NUMERIC(12, 8) DEFAULT NULL,
  `group_label` VARCHAR(255) DEFAULT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variants") NOT NULL,
  `nomenclature_type` VARCHAR(64) DEFAULT NULL,
  `sig_feature_hashkey` VARCHAR(32) NOT NULL,
  `match_status` VARCHAR(32) DEFAULT NULL,
  UNIQUE KEY `sig_feature_assay_probe` (`signature_id`, `feature_id`, `assay_type`, `probe_id`),
  KEY `idx_signature_nomenclature` (`assay_type`, `nomenclature_type`),
  -- Every other key here leads with signature_id, which answers "what is in
  -- this signature?" but gives no way in from the feature side. Without this,
  -- "which signatures contain this gene?" has to walk every signature's
  -- features and filter at the end -- a full pass over this table, 1.35M rows
  -- on the production repository.
  KEY `sfs_feature_id` (`feature_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
