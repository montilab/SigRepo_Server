--
-- Table structure for `gene_orthologs`
--
-- One row per ortholog pair between a non-human organism (`organism_id`, the
-- mouse side in the beta release) and human, as babelgene reports it. Human
-- is the implicit target side because babelgene only maps to and from human,
-- so a third organism later is a new organism_id, not a schema change.
--
-- The reference feature tables hold only an Ensembl id (transcriptomics) or a
-- UniProt accession (proteomics) beside a gene_symbol, so a cross-species
-- lookup resolves an id to its symbol through the reference table of its own
-- organism, joins this table on (organism_id, gene_symbol) or on human_symbol,
-- and reads the other side's symbol, Ensembl and Entrez ids. Both entry
-- points carry a leading index for that reason. The Ensembl and Entrez
-- columns are indexed so an Ensembl or Entrez id can also enter directly.
--
-- Loaded through SigRepo::updateOrthologFeatureSet() (API route
-- /update_orthologs), never from a committed file. `version` is the babelgene
-- package version that produced the row. Kept identical to the CREATE TABLE
-- in mysql/migrations/2026-11-02-gene-orthologs.sql.
CREATE TABLE `gene_orthologs` (
  `ortholog_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `organism_id` INT UNSIGNED NOT NULL,
  `gene_symbol` VARCHAR(255) NOT NULL,
  `ensembl_gene_id` VARCHAR(255) DEFAULT NULL,
  `entrez_id` INT UNSIGNED DEFAULT NULL,
  `human_symbol` VARCHAR(255) NOT NULL,
  `human_ensembl_gene_id` VARCHAR(255) DEFAULT NULL,
  `human_entrez_id` INT UNSIGNED DEFAULT NULL,
  `support` VARCHAR(255) DEFAULT NULL,
  `support_n` TINYINT UNSIGNED DEFAULT NULL,
  `is_current` BOOL DEFAULT 1,
  `version` VARCHAR(32) NOT NULL,
  `ortholog_hashkey` VARCHAR(32) NOT NULL,
  PRIMARY KEY (`ortholog_id`),
  UNIQUE KEY `uq_ortholog_hash` (`ortholog_hashkey`),
  KEY `go_organism_symbol` (`organism_id`, `gene_symbol`),
  KEY `go_ensembl` (`ensembl_gene_id`),
  KEY `go_entrez` (`entrez_id`),
  KEY `go_human_symbol` (`human_symbol`),
  KEY `go_human_ensembl` (`human_ensembl_gene_id`),
  KEY `go_human_entrez` (`human_entrez_id`),
  FOREIGN KEY (`organism_id`) REFERENCES `organisms` (`organism_id`),
  CHECK (`is_current` IN (0,1))
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
