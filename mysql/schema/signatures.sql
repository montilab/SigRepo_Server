--
-- Table structure for `signatures`
--
CREATE TABLE `signatures` (
  `signature_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `signature_name` VARCHAR(255) NOT NULL,
  `organism_id` INT UNSIGNED NOT NULL,
  `direction_type` SET("uni-directional", "bi-directional", "categorical") NOT NULL,
  `assay_type` SET("transcriptomics", "proteomics", "metabolomics", "methylomics", "genetic_variants") NOT NULL,
  -- The last member is 'genetic_variants', matching the deployed repository
  -- (SHOW CREATE TABLE on production, 2026-08-26) and the
  -- genetic_variants_features table this repo already ships. It read 'snps'
  -- here, so a database built from this file rejected the value production
  -- actually stores -- "Data truncated for column 'assay_type'" -- and could
  -- not hold a genetic-variants signature at all. The MCP layer's "snps"
  -- is a separate, deliberate API-facing alias (mcp/lib/queries.R) and is
  -- unaffected.
  `phenotype_id` INT UNSIGNED NOT NULL,
  `platform_id` INT UNSIGNED NOT NULL,
  `sample_type_id` INT UNSIGNED NOT NULL,
  `covariates` TEXT DEFAULT NULL,
  `description` TEXT DEFAULT NULL,
  `score_cutoff` NUMERIC(12, 8) DEFAULT NULL,
  `logfc_cutoff` NUMERIC(12, 8) DEFAULT NULL,
  `p_value_cutoff` NUMERIC(12, 8) DEFAULT NULL,
  `adj_p_cutoff` NUMERIC(12, 8) DEFAULT NULL,
  `cutoff_description` TEXT DEFAULT NULL,
  `keywords` TEXT DEFAULT NULL,
  `PMID` INT DEFAULT NULL,
  `year` INT DEFAULT NULL,
  `others` TEXT DEFAULT NULL,
  `has_difexp` BOOL DEFAULT 0,
  `num_of_difexp` INT DEFAULT NULL,
  `num_up_regulated` INT DEFAULT NULL,
  `num_down_regulated` INT DEFAULT NULL,
  `user_name` VARCHAR(255) NOT NULL,
  -- Where this signature came from: 'curated' for one deposited into SigRepo
  -- directly, or the name of the external resource it was pulled from
  -- ('rummagene', and whatever integrations follow).
  --
  -- VARCHAR rather than ENUM/SET on purpose. assay_type is a SET, and that is
  -- exactly what made a database built from this file reject the value
  -- production actually stored -- adding a member meant a schema change that
  -- nothing applied. Adding a source here is a write, not a migration.
  --
  -- NOT NULL DEFAULT 'curated' so every existing row and every future upload
  -- is correct without a caller having to remember to set it; only an
  -- integration that pulls from elsewhere has to say so.
  `signature_source` VARCHAR(64) NOT NULL DEFAULT 'curated',
  `date_created` DATETIME DEFAULT CURRENT_TIMESTAMP,  
  `visibility` BOOL DEFAULT 0,  
  `signature_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`signature_id`),
  UNIQUE (`signature_name`, `user_name`),
  FOREIGN KEY (`organism_id`) REFERENCES organisms (`organism_id`),
  FOREIGN KEY (`phenotype_id`) REFERENCES phenotypes (`phenotype_id`),
  FOREIGN KEY (`platform_id`) REFERENCES platforms (`platform_id`),
  FOREIGN KEY (`sample_type_id`) REFERENCES sample_types (`sample_type_id`),
  FOREIGN KEY (`user_name`) REFERENCES users (`user_name`),
  CHECK (`has_difexp` IN (0,1)),
  CHECK (`visibility` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
