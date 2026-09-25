# Rename `direction_type` to `type` and `platform_name` to `platform`

Date: 2026-09-25
Status: approved design, not yet implemented
Repos affected: OmicSignature, SigRepo, SigRepo_Server

## Goal

Establish one vocabulary for two metadata fields across the OmicSignature
package, the SigRepo R client, the MySQL schema, and the HTTP API:

* the signature direction field is named `type`
* the platform field is named `platform`

Today these fields carry three different names depending on the layer, which
forces translation code at every boundary and has already produced defensive
coalescing in the client.

## Non-goals

* No change to any stored value. `uni-directional`, `bi-directional` and
  `categorical` stay exactly as written, and platform strings are untouched.
* No change to any table name. Only two column names change.
* No renaming of `assay_type` or `sample_type`, which are already consistent.
* No new migration framework. This ships one migration script plus its
  reverse, not a general mechanism.

## Current state

| Layer | Direction field | Platform field |
| --- | --- | --- |
| OmicSignature metadata | `direction_type` | `platform` |
| SigRepo client arguments | n/a | `platform_name` |
| SigRepo exported symbols | `direction_types`, `checkDirectionType()` | n/a |
| `signatures` table | `direction_type` | joined via `platform_id` |
| `platforms` table | n/a | `platform_name` |
| API JSON and `sort_by` | `direction_type` | `platform_name` |

OmicSignature already calls the field `platform`, so the platform half of this
work is entirely in SigRepo and SigRepo_Server. OmicSignature changes only
`direction_type`.

Two observations that motivate the chosen direction rather than the reverse:

1. Every other vocabulary table uses the bare noun: `organisms.organism`,
   `phenotypes.phenotype`, `sample_types.sample_type`, `keywords.keyword`.
   Only `platforms.platform_name` breaks the pattern.
2. `getSignature.R:165` and `createOmicCollection.R:118` both construct
   `c("organism", "phenotype", "sample_type", "platform_name")`, where three
   entries are bare and one is not, inside the same expression.

So renaming `platform_name` to `platform` removes existing inconsistency
rather than introducing new inconsistency.

## Target state

| Layer | Direction field | Platform field |
| --- | --- | --- |
| OmicSignature metadata | `type` | `platform` |
| SigRepo client arguments | n/a | `platform` |
| SigRepo exported symbols | `signature_types`, `checkSignatureType()` | n/a |
| `signatures` table | `type` | joined via `platform_id` |
| `platforms` table | n/a | `platform` |
| API JSON and `sort_by` | `type` | `platform` |

The exported symbols keep a `signature_` qualifier because a bare `types`
object and a bare `checkType()` function are too generic to read at a call
site, even when namespaced.

## Compatibility decisions

These were settled explicitly and the implementation should not quietly
revisit them.

* **OmicSignature metadata input is tolerant.** A metadata list or JSON file
  carrying `direction_type` is accepted, normalized to `type`, and warned
  about. A list carrying both keys is an error rather than a guess.
* **SigRepo client arguments are a hard rename.** No deprecated
  `platform_name` argument. Callers passing the old name get R's own unused
  argument error, which is immediate and unambiguous. The audience is the lab,
  not a CRAN userbase.
* **The API is strict out, tolerant in.** Responses emit only `type` and
  `platform`. The upload endpoint accepts either spelling on incoming JSON,
  because users will upload files written by older OmicSignature versions.
* **No dual keys in API responses.** One vocabulary on the wire.

## Phase 1: OmicSignature

Independently mergeable and releasable. The tolerance shim is what makes the
phase boundary safe, because SigRepo keeps working unchanged in the gap.

1. `createMetadata()` takes `type`. It gains a deprecated
   `direction_type = NULL` argument that warns once and forwards when supplied.
2. `checkMetadata()` gains a normalization step at its top: when the incoming
   list has `direction_type` and no `type`, rename the key in place preserving
   position, and warn. When it has both, stop.
3. `checkMetadata()` is the only choke point needed. `readJson()` constructs
   through `OmicSignature$new()` (`readwriteJson.R:119`), and the `metadata<-`
   active binding calls `checkMetadata()` as well, so construction, assignment
   and JSON reads are all covered by the single normalization.
4. Refactor `OmicSignature.R:92-95`, which currently reads
   `metadata$direction_type` three times *before* `checkMetadata()` runs and
   would therefore bypass the shim. Assign `private$.metadata` from
   `checkMetadata()` first, then read `signatureType` from the normalized
   result. This also removes the existing triplication.
5. Rename remaining references: `OmicSigFromDifexp.R`,
   `compare_omic_signatures.R`, the required metadata field list, the four
   `data-raw` builders, five vignettes, and roxygen documentation.
6. Rewrite the four `inst/extdata/*.json` fixtures to `type`, and add one new
   fixture that deliberately keeps `direction_type` so the shim has a
   regression test rather than only a unit test.

## Phase 2: SigRepo and SigRepo_Server

The DB, client, API and frontends form one contract and move together.
Splitting them only creates a window where the stack is inconsistent.

### Migration

New file `mysql/migrations/2026-09-25-rename-type-platform.sql`:

```sql
ALTER TABLE `signatures` RENAME COLUMN `direction_type` TO `type`;
ALTER TABLE `platforms`  RENAME COLUMN `platform_name` TO `platform`;
```

`RENAME COLUMN` is chosen deliberately over `CHANGE COLUMN`.
`signatures.direction_type` is a `SET(...)` column, and `CHANGE COLUMN`
requires restating the full type definition. The repo schema file and the
deployed schema have already drifted once on exactly such a definition, and
the comment at `mysql/schema/signatures.sql:9-17` records the resulting
truncated data. `RENAME COLUMN` needs no type restatement, so that class of
error is impossible.

Properties:

* Metadata only and in place. No table rebuild, no row copy, and duration is
  independent of row count.
* Indexes follow the column automatically, so `UNIQUE (platform_name)` needs
  no separate statement.
* No foreign key references either column. Foreign keys point at
  `platform_id`.
* Requires MySQL 8.0 or later. The `mysql/Dockerfile` default is
  `mysql:8.0-bookworm`. The version requirement is stated in the script for
  whoever runs it elsewhere.

Ships alongside a reverse script performing the inverse renames, also
metadata only.

Rollout: applied to the local stack as part of this work. BUMC
(`montilab.bu.edu`) and production (`sigrepo.org`) are left for a deliberate
manual run, consistent with the standing rule that the production database is
not written to from this workflow. Take a `mysqldump` of the affected tables
before running in either of those environments.

### Schema and seed data

* `mysql/schema/signatures.sql` and `mysql/schema/platforms.sql` updated so
  fresh databases are created with the new names.
* `mysql/data/platforms.csv` header changes from `platform_name` to
  `platform`. This is coupled across repos: the header is read by `read.csv`
  and passed directly into `SigRepo::addPlatform()`
  (`api/lib/database_admin.R:126-127`), whose required column name is
  changing in the same release. If only one side moves, a fresh install
  breaks.
* `tests/testthat/fixtures/seed.sql` updated.

### SigRepo client

* Argument renames: `searchSignature()`, `searchPlatform()`,
  `deletePlatform()`.
* `addPlatform()` required input column becomes `platform`, and its roxygen
  example updated.
* `direction_types` becomes `signature_types`; `checkDirectionType()` becomes
  `checkSignatureType()`, with its argument renamed to `type`.
* The `c("organism", "phenotype", "sample_type", "platform_name")` vectors in
  `getSignature.R:165` and `createOmicCollection.R:118` become uniform.
* `lookup_table_sql` column name strings updated, including the
  `filter_coln_var` and `return_var` call sites.
* `createOmicSignature.R` emits `type =` and `platform =` when building
  OmicSignature metadata; `createSignatureMetadata.R` reads `metadata$type`.
* The defensive coalesce at `hypeR_examples.R:1249`
  (`metadata$platform_name %||% metadata$platform`) collapses to one key.
* Roxygen regenerated; the `omic_signature_1` through `omic_signature_4`,
  `LLFS_Aging_Gene_2023` and `Myc_reduce_mice_liver_24m` data object docs list
  metadata fields and need updating.

**Stale object guard.** The hard argument rename leaves one gap: an
OmicSignature object saved to RDS under the old package never passes through
phase 1's normalization, so `metadata$type` is `NULL`. Rather than letting
that surface as a confusing downstream failure, `checkOmicSignature()` detects
`direction_type` present with `type` absent and stops with a message telling
the user to reinstall OmicSignature and rebuild the object.

### API

* `api/lib/signature.R:62-65` column allow list, the `SELECT` aliases at line
  139, and `return_var` at line 221.
* `api/lib/create_signature.R`: the insert column list, the `meta_str()`
  reads, the `lookup_id()` platform call, and `platform_field`.
  `REQUIRED_UPLOAD_METADATA_FIELDS` accepts either `direction_type` or `type`
  per the tolerant input decision.
* `api/lib/compare.R`: the `sig_meta` structure and its comment.
* `api/lib/vocabulary.R` and `mcp/lib/queries.R` platform queries.
* `api/api.R:1294-1295` `sort_by` documentation.

### Frontends

* `web/src/api/client.ts` interface fields and the `sort_by` union type;
  `SignaturesPage.tsx` column definitions; `SignatureDetailPage.tsx` field
  labels. The web image is baked rather than mounted, so it needs a rebuild.
* The legacy Shiny app's five affected files matter more for live behavior
  than the React app does, because nginx has served Shiny at `/` since mid
  September with `sigrepo-web` stopped. Verify that routing at deploy time
  rather than assuming it.

## Verification

Per repo:

* OmicSignature: full testthat suite, plus the new old name JSON fixture as a
  shim regression test.
* SigRepo client: `test_platforms.R`, `test_signatures.R`,
  `test_lookup_table_sql_db.R`, `test_checkFunctions.R`,
  `test_client_regressions_db.R`, `test_hypergem_client.R`.
* SigRepo_Server: the testthat suite including the updated `seed.sql`
  fixture.

Three hazards specific to these repos that the implementation must respect:

1. `SIGREPO_TEST_*` must point at the local stack before any `R CMD check` or
   `devtools::check()`, because check builds vignettes that add and delete
   signatures through a connection handler defaulting to `sigrepo.org`.
2. Server `testServer()` calls need the `later::with_temp_loop` wrapper, or the
   suite hangs on leaked pools.
3. An aborted `test_dir` run leaves state that makes the next run report
   failures that are not real. A clean rerun is required before treating any
   red as a genuine regression.

End to end: use the repo's `validate-and-ship` skill rather than re-deriving
the procedure. It stands up a throwaway stack (MySQL, the Plumber API, the MCP
server), bootstraps it, runs the `local_validation` harness, and tears it down.
A throwaway stack is the right shape here because it builds the database from
`mysql/schema/*.sql`, which exercises the renamed schema and the changed
`platforms.csv` header on a fresh install path.

That does not cover the migration itself, since a fresh database never runs it.
So additionally, against the persistent local stack: apply the migration to the
existing populated database, restart the API, exercise search, get, add and
compare through the client, then drive the Shiny Annotate and signature tabs
headless. Fresh install and migrated install are two distinct paths and both
need to pass.

## Data safety

The migration does not modify data. Confirmed specifically:

* `signature_hashkey` is md5 of `paste0(signature_name, user_name)` lowercased
  (`addSignature.R:156-157` feeding `checkFunctions.R:900`). Neither column is
  renamed, so no hashkey changes. This matters because hashkeys are the
  filenames of the stored difexp `.RDS` files, which must keep resolving.
* The `others` TEXT column serializes only the user supplied `metadata$others`
  sublist (`createSignatureMetadata.R:300`), not the whole metadata list, so
  no stored text embeds the old field names.
* No views, triggers or generated columns reference either column.

The residual risk is version skew, not corruption: code deployed against an
unmigrated database produces erroring queries and a down service, with no bad
rows written.

## Branching

One issue per repo created on the remote, branches via `gh issue develop`, no
`fix/` or `feat/` prefixes. Worktrees at `SigRepo/wt/<issue>` and
`SigRepo_Server/wt/<issue>`. OmicSignature is a single checkout currently on
the already merged branch `fix/difexp-null-probe-id`, so it returns to `main`
and pulls before branching.

Merge order: OmicSignature first and released, then SigRepo and
SigRepo_Server together with the migration.

## Note for planning

This spec covers two phases that are deliberately independent, so it should
produce two implementation plans rather than one. Phase 1 is a single repo with
a compatibility shim and is testable on its own. Phase 2 spans two repos, a
schema migration, and two frontends, and its tasks are coupled by a shared
contract. Combining them into one plan would obscure the merge boundary that
makes phase 1 safe to ship first.
