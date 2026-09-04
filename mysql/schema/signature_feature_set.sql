--
-- Table structure for `signature_feature_set`
--
CREATE TABLE `signature_feature_set` (
  `signature_id` INT UNSIGNED NOT NULL,
  `feature_id` INT UNSIGNED NOT NULL,
  `probe_id` VARCHAR(255) NOT NULL,
  `score` NUMERIC(12, 8) DEFAULT NULL,
  `group_label` VARCHAR(255) NOT NULL DEFAULT 'All Features',
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "snps") NOT NULL,
  `sig_feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`, `group_label`, `probe_id`),
  UNIQUE (`signature_id`, `feature_id`, `assay_type`, `group_label`, `probe_id`),
  -- Every other key here leads with signature_id, which answers "what is in
  -- this signature?" but gives no way in from the feature side. Without this,
  -- "which signatures contain this gene?" has to walk every signature's
  -- features and filter at the end -- a full pass over this table, 1.35M rows
  -- on the production repository.
  KEY `sfs_feature_id` (`feature_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
