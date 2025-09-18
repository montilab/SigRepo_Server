--
-- Table structure for `signature_feature_set`
--
CREATE TABLE `signature_feature_set` (
  `signature_id` INT UNSIGNED NOT NULL,
  `feature_id` INT UNSIGNED NOT NULL,
  `probe_id` VARCHAR(255) DEFAULT NULL,
  `score` NUMERIC(10, 8) DEFAULT NULL,
  `group_label` VARCHAR(255) DEFAULT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variations", "dna_binding_sites") NOT NULL,
  `sig_feature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`, `feature_id`),
  UNIQUE (`signature_id`, `feature_id`),
  FOREIGN KEY (`signature_id`) REFERENCES `signatures` (`signature_id`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
