# Rummagene catalog — browse and pull

**Date:** 2026-08-31
**Status:** approved, ready for implementation planning

## Problem

Rummagene (rummagene.com, Ma'ayan Lab) holds 1,000,506 gene sets scraped from
the supplementary tables of 188,249 PMC articles. SigRepo holds 294 curated
signatures. There is real value in the Rummagene corpus, and no way today to get
any of it into SigRepo.

Bulk ingest was considered and rejected. Two measurements settled it:

- **272,000 sets would qualify on metadata** (see *Measured evidence* below).
  Adding those to a 294-signature repository makes SigRepo 99.9% literature
  scrape.
- **MeSH describes the paper, not the table.** A supplementary table that is
  actually network graph nodes (`PMC10201598-mmc4.xlsx-GCN_Adipose_visceral-Nodes`)
  qualifies cleanly if its paper is human-transcriptomics-indexed. No automated
  filter can see this. A human looking at the row can.

So: a browse-and-pull catalog. A person searches the catalog, reads what a set
actually is, and pulls the ones they want. Pulled sets become ordinary
`OmicSignature`s in the `signatures` table, owned by the puller.

## Governing rule

**Nothing is invented.** A set is offered only when every mandatory
`OmicSignature` field is attested by an authoritative source, or is a true
statement about the data:

| Field | Source |
|---|---|
| `signature_name` | the Rummagene term |
| `organism` | PubMed MeSH descriptor, unambiguous |
| `assay_type` | PubMed MeSH descriptor, unambiguous |
| `phenotype` | `"unknown"` — a declaration of ignorance, not a guess |
| `direction_type` | `"uni-directional"` — accurate for an unordered gene list, and the only direction type whose `checkSignature()` requires no `group_label` |

Rummagene's own schema carries none of these: `GeneSet` is
term/geneIds/nGeneIds/created/description/hash/genes and `PmcInfo` is
pmcid/title/yr/doi. Organism and assay come from PubMed, reached via the PMC id
Rummagene does supply.

## Measured evidence

All figures measured 2026-08-31 against the live services.

**Metadata qualification** — 600 gene sets drawn at random offsets across the
corpus (535 distinct papers, so near-independent), run through
`rummagene_qualify_all()`. Reproduced exactly on a second run with the same seed:

```
qualified          163 / 600  = 27.2%   (95% CI 23.6-30.7%)
projected corpus   271,804              (236,193 - 307,415)

rejected  assay_type 166 · no_mesh 146 · organism 125
          no_pmc 0 · too_few_genes 0

organism  Homo sapiens 155 (95%) · Mus musculus 8 (5%)
assay     transcriptomics 101 · proteomics 37 · genetic_variants 19 · methylomics 6
size      median 37 genes, IQR 14-106, 82% have >= 10
```

**Uploadability** — the 10,085 distinct symbols from those transcriptomics sets,
matched against the reference table:

```
as feature_name (the column upload resolves on) :     0   (0.00%)
as gene_symbol  (a column upload ignores)       : 4,062  (81.24%)
```

`transcriptomics_features.feature_name` holds lowercased Ensembl gene IDs, not
symbols. `resolve_feature_ids()` matches on
`md5(tolower(feature_name + organism_id))` and is all-or-nothing. So without a
mapping stage, every pull fails on the first gene.

**Mapping recovers it** — symbols → Ensembl via `org.Hs.eg.db`:

```
per-set mapping rate   median 100%, mean 99.9%, min 96.2%
100% mappable          81 / 93 human transcriptomics sets  (87%)
unmapped are deprecated aliases: CCDC153, SOGA1, TRA, LGTN
```

**Reference coverage is complete.** `SigRepo::updateTranscriptomicsFeatureSet()`
calls `biomaRt::getBM(attributes = c("ensembl_gene_id", "hgnc_symbol"), mart =
ensembl)` with no `filters` argument — the full Ensembl build per organism. So
Ensembl IDs from `org.Hs.eg.db` are present, modulo version drift, which the
gate below absorbs.

**Funnel:** 1,000,506 → 272,000 metadata-qualified → ~155,000 human
transcriptomics → **~135,000 catalog rows**.

## Scope

Human transcriptomics only.

| Excluded | Why |
|---|---|
| Mouse (5% of qualified) | needs `org.Mm.eg.db`, not installed; and `updateTranscriptomicsFeatureSet()` populates `gene_symbol` from `hgnc_symbol`, which is empty for mouse — the known mouse-symbol gap must be fixed first |
| proteomics (23%) | `proteomics_features.feature_name` is UniProt accessions (`O89020`); needs symbol → UniProt, lossier |
| genetic_variants (12%) | needs rsIDs; these are `Polymorphism, Single Nucleotide` papers whose tables are gene lists — wrong assay |
| methylomics (4%) | `upload_reference_table()` returns NULL; SigRepo has no methylomics feature table |
| bulk pull | defeats the human-curation purpose |

## Architecture

### New table: `rummagene_catalog`

Catalog metadata only. Contains no signature and grants no access; a row is an
*offer*, and only a pull creates anything in `signatures`.

```sql
CREATE TABLE `rummagene_catalog` (
  `rummagene_catalog_id` INT UNSIGNED NOT NULL AUTO_INCREMENT,
  `term`            VARCHAR(512) NOT NULL,
  `pmcid`           VARCHAR(32)  NOT NULL,
  `pmid`            VARCHAR(32)  DEFAULT NULL,
  `title`           TEXT         DEFAULT NULL,
  `year`            INT          DEFAULT NULL,
  `doi`             VARCHAR(255) DEFAULT NULL,
  `description`     TEXT         DEFAULT NULL,
  `organism`        VARCHAR(128) NOT NULL,
  `assay_type`      VARCHAR(64)  NOT NULL,
  `mesh_evidence`   TEXT         NOT NULL,
  `n_genes`         INT UNSIGNED NOT NULL,
  `gene_symbols`    MEDIUMTEXT   NOT NULL,
  `feature_names`   MEDIUMTEXT   NOT NULL,
  `gmt_version`     VARCHAR(64)  NOT NULL,
  `built_at`        DATETIME     NOT NULL DEFAULT CURRENT_TIMESTAMP,
  `term_hashkey`    VARCHAR(32)  NOT NULL,
  PRIMARY KEY (`rummagene_catalog_id`),
  UNIQUE (`term_hashkey`),
  KEY (`organism`, `assay_type`),
  KEY (`year`),
  KEY (`n_genes`),
  KEY (`pmcid`)
) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci;
```

Notes:

- `organism` and `assay_type` are free text, deliberately **not** FKs. A catalog
  row is not a signature; it must be storable before anything validates it
  against SigRepo's controlled vocabularies. The pull path does that validation.
- `term` is `VARCHAR(512)` because Rummagene terms exceed 255 characters.
  `signature_name` is `VARCHAR(255)`, so pull truncates — see *Pull*.
- Uniqueness is on `term_hashkey` (`md5(tolower(term))`), not `term`, because a
  512-char unique index exceeds InnoDB's key length under `utf8`.
- Both gene columns are stored: `gene_symbols` is what the paper published and
  is what the UI shows; `feature_names` is the mapped, lowercased Ensembl IDs so
  pull needs no mapping at request time. ~135k rows × ~40 genes ≈ 40MB.

### Build job — `api/lib/rummagene_catalog_build.R`

Reuses [api/lib/rummagene_ingest.R](../api/lib/rummagene_ingest.R) for MeSH
resolution and qualification. That file and its 58 tests were written on
2026-08-31 and are currently **untracked** — they need a branch and a commit
before this work starts.

1. Download `https://rummagene.com/latest.gmt` (~700MB, refreshed weekly;
   `Last Updated Mon Aug 24 2026` at time of writing).
2. **Stream it line by line.** The droplet has 3GB RAM with ~1GB free and no
   swap; the file must never be materialized in memory. GMT format is
   `term \t description \t gene \t gene \t ...`.
3. Extract the PMC id from each term's `^PMC[0-9]+` prefix.
4. Batch-resolve distinct PMC ids → PMIDs → MeSH descriptors
   (`rummagene_fetch_mesh_by_pmcid()`). ~188k papers ÷ 100 per batch × 2
   endpoints ≈ 3,800 requests at NCBI's 3/sec limit ≈ 21 minutes.
5. Qualify each set (`rummagene_qualify()`): unambiguous organism, unambiguous
   assay type, ≥ 2 genes.
6. Map symbols → Ensembl with `org.Hs.eg.db`.
7. **Gate:** keep the set only if *every* symbol resolves to a `feature_id` in
   `transcriptomics_features` for that organism. Checked against the live table,
   not against `org.Hs.eg.db`, so Ensembl version drift moves a set out of the
   catalog rather than producing a row that fails on pull.
8. Upsert into `rummagene_catalog` keyed by `term_hashkey`; rows absent from the
   current GMT are deleted so the catalog tracks the source. Deleting a catalog
   row does **not** affect a signature already pulled from it — the signature
   lives in `signatures` with its provenance in `others`, and only the offer to
   pull it again disappears. There is no FK between the two tables, deliberately.

Idempotent — a second run over the same GMT is a no-op. Runs manually or
weekly, and should run off the droplet, shipping the table.

### API routes

Both added to `api/api.R` (route annotations are only parsed there; helpers live
in `api/lib/`).

```
GET  /rummagene/catalog   api_key, q, organism, assay_type, year_min, year_max,
                          n_genes_min, n_genes_max, sort, order, limit, offset
                          -> { total, rows[] }   server-side paged and sorted

POST /rummagene/pull      api_key, term
                          -> the created signature's hashkey
```

`/rummagene/catalog` returns catalog metadata only — never `gene_symbols` or
`feature_names`, which are large and only needed on pull. A detail view fetches
one row's genes on demand.

### Pull

1. Look up the catalog row by `term_hashkey`.
2. Build the `OmicSignature`: metadata per *Governing rule*, `signature` a
   data frame whose `feature_name` column is the stored Ensembl IDs.
3. Hand it to the existing `upload_signature()` in
   [api/lib/create_signature.R](../api/lib/create_signature.R). No new insert
   logic and no new permission path — the same feature resolution, rollback, and
   access-grant code every other upload uses.

Resulting signature: owner is the puller, `visibility = 0` (private), and
`others` records provenance in the format `rummagene_build_signature()` already
produces:

```
source=rummagene; pmcid=PMC7202592; organism and assay_type from
PubMed MeSH (Humans, Transcriptome); phenotype not stated by source
```

Two people pulling the same term each get their own signature —
`UNIQUE(signature_name, user_name)` permits it, and the same person pulling
twice gets the existing duplicate error.

### Prerequisites

1. **`signature_name` truncation.** *Blocking.* Rummagene terms exceed
   `signature_name`'s `VARCHAR(255)` — e.g.
   `PMC11590809-jitc-12-11-s007.xls-DEG_cluster...`. Truncate to 255 on a UTF-8
   boundary, keeping the full term in `rummagene_catalog.term` and in `others`,
   so the source is never lost. Note `UNIQUE(signature_name, user_name)`:
   truncation can collide two long terms from the same paper, so the truncated
   name must stay unique — append a short hash of the full term when it would.

2. **Platform resolution — *not* a blocker.** An earlier draft of this spec
   claimed `platforms` has no `unknown` row and that every pull would fail.
   That was wrong. `mysql/data/platforms.csv` ships `Unknown`, `lookup_id()`
   compares with a plain `=`, and the column's `utf8_unicode_ci` collation is
   case-insensitive — so `create_signature.R:411`'s `"unknown"` default resolves
   against `Unknown` on any normally seeded database, production included.
   Verified 2026-08-31 by inserting `Unknown` and selecting `unknown`.

   What *is* worth doing: `.local-data/rebuild_local_db.R` seeds `platforms`
   only with the values the basket signatures use, which is why the row appeared
   missing locally. The dev stack should seed the full vocabulary so local
   behaviour matches production. This is a dev-stack fix, not a prerequisite for
   this feature.

## Web UI

New page at `/rummagene`, registered in `web/src/App.tsx` alongside the existing
routes and given its own nav entry — deliberately separate from Signatures,
because the catalog is not the repository.

- `DataTable` with server-side paging and sorting, reusing the sortable-column
  work from PR #52.
- Filters: search text, organism, assay type, year range, gene-count range.
- Columns: term (tidied for display, as `RummagenePanel.tsx` already does),
  paper title, year, gene count, and a PMC link.
- Row expands to show the paper's abstract-level metadata, the MeSH evidence
  that attested organism and assay, and the gene list — so the person can see
  what the table actually is before pulling.
- A **Pull** action per row; on success, a link to the created signature.

The page states plainly that these are literature-mined sets whose metadata is
attested by MeSH but whose biological meaning is unverified.

## Testing

`rummagene_ingest.R`'s qualification logic already has 58 tests. New coverage:

- GMT streaming parse: well-formed line, description-only line, a line with no
  genes, and a term with no `PMC` prefix.
- The 100%-mappable gate rejects a set with a single unmappable symbol, and
  accepts the otherwise-identical set without it.
- Catalog upsert is idempotent, and drops rows absent from the new GMT.
- `/rummagene/catalog` paging, sorting, and each filter.
- Pull produces a signature whose feature set matches the source term exactly,
  with no genes added or dropped.
- Pull of an unknown term returns 404; a second pull by the same user returns
  the duplicate error.

Network-touching functions follow the existing convention in
`tests/testthat/test-rummagene.R`: parsers are tested against trimmed real
payloads, HTTP calls are not mocked.

## Consequences

**Good.** Every catalog row is guaranteed to pull cleanly — the gate runs at
build time, so the browse page cannot offer something that fails. Stored
signatures match the published gene list exactly, with nothing dropped. A human
decides what enters the repository. The `signatures` table grows only by
deliberate action.

**Costs.** A weekly 700MB download and a ~21-minute NCBI pass. One new table
(~40MB). The catalog silently omits ~13% of metadata-qualified sets — those with
a single deprecated alias — which is the price of the guarantee.

**Unresolved.** MeSH describes the paper, so a set that is not a differential
contrast can still appear in the catalog. This design does not solve that; it
puts a person in front of it. Whether the catalog should also record a
*curation* signal — a flag, a rating, or a review step — is deferred until there
is usage to learn from.
