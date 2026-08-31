--
-- Table structure for `rummagene_catalog`
--
-- Catalog of Rummagene gene sets that passed the full ingest gate: organism and
-- assay_type attested by PubMed MeSH, and every gene symbol resolving to a
-- feature_id in transcriptomics_features. A row is an OFFER, not a signature --
-- it grants no access and creates nothing until someone pulls it.
--
-- `organism` and `assay_type` are deliberately free text rather than foreign
-- keys: a catalog row must be storable before anything validates it against
-- SigRepo's controlled vocabularies. The pull path does that validation, via
-- the same create_signature.R lookups every other upload uses.
--
CREATE TABLE `rummagene_catalog` (
  `rummagene_catalog_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  -- Rummagene terms exceed signatures.signature_name's VARCHAR(255); the full
  -- term lives here and is truncated only on pull.
  `term` VARCHAR(512) NOT NULL,
  `pmcid` VARCHAR(32) NOT NULL,
  `pmid` VARCHAR(32) DEFAULT NULL,
  `title` TEXT DEFAULT NULL,
  `year` INT DEFAULT NULL,
  `doi` VARCHAR(255) DEFAULT NULL,
  `description` TEXT DEFAULT NULL,
  `organism` VARCHAR(128) NOT NULL,
  `assay_type` VARCHAR(64) NOT NULL,
  -- The MeSH descriptors that attested organism and assay_type, so a reader can
  -- re-check the claim against the PubMed record.
  `mesh_evidence` TEXT NOT NULL,
  `n_genes` INT UNSIGNED NOT NULL,
  -- What the paper published, verbatim -- shown in the UI.
  `gene_symbols` MEDIUMTEXT NOT NULL,
  -- The mapped, lowercased Ensembl IDs, so pull needs no mapping at request
  -- time and resolves straight through create_signature.R's feature lookup.
  `feature_names` MEDIUMTEXT NOT NULL,
  `gmt_version` VARCHAR(64) NOT NULL,
  `built_at` DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
  -- md5(tolower(term)). Uniqueness lives here because a 512-char unique index
  -- exceeds InnoDB's key length under utf8.
  `term_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`rummagene_catalog_id`),
  UNIQUE (`term_hashkey`),
  KEY (`organism`, `assay_type`),
  KEY (`year`),
  KEY (`n_genes`),
  KEY (`pmcid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
