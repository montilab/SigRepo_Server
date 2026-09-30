# mysql/migrations

Schema migrations for the SigRepo database: the changes that take a database
built by an older version to the schema the current code expects, keeping its
data.

Every `.sql` file directly in this directory is a forward migration, applied in
filename order. `scripts/migrate.sh` applies the ones a database has not had
and records each in the `schema_migrations` table. Rollback scripts live in
`rollback/` and are never applied by the runner.

Requires MySQL 8.0 or later.

## Applying migrations

An installed instance does not need this section: `update_sigrepo.sh` takes a
backup, runs the migrations that ship in the release it is moving to, and
checks the result.

Everywhere else (the local stack, montilab.bu.edu, sigrepo.org), from the
repository root:

```bash
scripts/migrate.sh --container sigrepo-mysql --status   # what is pending
scripts/migrate.sh --container sigrepo-mysql            # apply it
```

Name the MySQL container of the stack in question (`sigrepo-local-mysql` for
the local stack). The runner uses the mysql client inside that container and
the root password the container already holds, so no credential is typed or
printed. For a MySQL that is not in a container, give `--host`, `--user` and
`--database`, and put the password in `MYSQL_PWD`.

The run stops at the first migration that fails and records nothing for it.
Migrations before it stay applied and recorded.

### Before running on montilab.bu.edu or sigrepo.org

These two hosts hold data that cannot be regenerated. Before applying anything
here to either one:

1. Take a `mysqldump` of the whole database and store it outside the database
   container.
2. Confirm the dump completed and is not empty.

### A database with no `schema_migrations` table

The first run on such a database applies every file. That is safe and
intended: each migration checks `information_schema` and does only what is
missing, so one whose change is already present does nothing and is then
recorded. This is how a database of unknown age is brought forward without
anyone having to know which changes it has.

`generate_db_schema()`, which `/init_db` calls, builds the current schema from
`mysql/schema/` and records every migration as applied, so a fresh database is
never pending.

## Code and schema move together

The API checks at startup that every migration it ships is recorded, and
refuses to start if one is not (`assert_schema_migrated()` in
`api/lib/schema_migrations.R`). So on a host that bind mounts this checkout,
the order of a deploy that includes a migration is:

1. Back up.
2. `git pull`, which brings the new migration file.
3. `scripts/migrate.sh --container <mysql container>`.
4. Recreate the API, Shiny and MCP containers.

Between steps 2 and 4 the running API still holds the old code and keeps
serving; a restart in that window, before step 3, is what the check refuses.

If the migration needs code that lives in the image and not in this checkout
(the `SigRepo` client, `OmicSignature`), pull the rebuilt image before step 4.
A `git pull` and a restart do not update either package: both are installed
into the image at build time, and nothing loads them from a bind mount.

## Writing a migration

Add a file named `YYYY-MM-DD-what-it-does.sql` and change `mysql/schema/` in
the same commit. The rules, each of which a test enforces:

- **Idempotent.** Guard every statement on `information_schema`, so the file
  can run against a database that already has the change, or has half of it.
  DDL in MySQL commits statement by statement and cannot be rolled back as a
  group; re-running the same file is the recovery from an interrupted run.
- **Starts by checking a database is selected.** The guards filter on
  `TABLE_SCHEMA = DATABASE()`. With no default database, `DATABASE()` is NULL,
  every guard matches nothing, and the file would exit 0 having done nothing.
- **Ends by checking its own result**, and fails if the intended state is not
  there.
- **Never drops a value a row is using.** To change a `SET` or `ENUM`, add the
  new member to the list the table has now (read it from
  `information_schema`; do not restate it from memory), rewrite the rows, then
  narrow. `2026-08-26-assay-type-genetic-variants.sql` is the pattern. A
  restated `SET` that disagreed with the deployed table has truncated data in
  this project before.
- **Stops, without changing anything, when the data cannot be carried over.**
  The way to stop from a plain SQL script is to select from a table that does
  not exist, with the message as the column name; every file here does that
  for its pre- and postconditions.

Then regenerate the expected schema and run the tests:

```bash
bash tests/shell/test_expected_schema.sh --container <disposable mysql> --write
bash tests/shell/test_history_upgrade.sh --container <disposable mysql>
```

`tests/shell/testlib.sh` says how to start a disposable MySQL.
`test_history_upgrade.sh` builds every schema this repository has ever shipped,
runs all the migrations, and compares the result with a database built from
`mysql/schema/` today. A schema change with no migration fails there, because
the commit before it no longer converges.

## When a migration stops on the data

Two migrations can refuse a database because of what is stored in it. In both
cases nothing has been changed by that migration, and the fix is in the data.

**signature_feature_set duplicates.**
`2026-09-13-signature-feature-set-keys.sql` stops with "signature_feature_set
repeats a feature under one probe" when one signature holds the same feature
under the same probe more than once, in different groups. The production key
does not allow that. Find the rows:

```sql
SELECT signature_id, feature_id, assay_type, probe_id, COUNT(*) AS copies
  FROM signature_feature_set
 WHERE feature_id IS NOT NULL AND probe_id IS NOT NULL
 GROUP BY signature_id, feature_id, assay_type, probe_id
HAVING COUNT(*) > 1;
```

Decide per signature which row is right, remove the others, and run the
migrations again.

**An assay type with no equivalent.**
`2026-08-26-assay-type-genetic-variants.sql` stops with "holds an assay_type
with no current equivalent" when a row's `assay_type` is `dna_binding_sites`,
which the first schema allowed and no later one does. Those signatures cannot
be represented in the current schema; they have to be removed or re-uploaded
under a current assay type before the migration can finish.

## Rolling back

`rollback/` holds the inverse of a migration where one was written. Apply it
by hand with the mysql client, then delete the migration's row from
`schema_migrations`, and run the code that matches the rolled back schema. The
runner never applies a file named `*-rollback.sql`, wherever it is.

For an installed instance the way back is `update_sigrepo.sh --restore`, which
reloads the dump taken before the update.

## Migrations in this directory

| File | Change |
| --- | --- |
| `2025-12-11-organisms-reference-columns.sql` | Adds the BioMart and proteomics reference columns to `organisms` |
| `2025-12-11-proteomics-unique-index-name.sql` | Names the unique index on `proteomics_features` `feature_organism` |
| `2026-01-29-widen-score-and-cutoff-precision.sql` | `NUMERIC(10,8)` to `NUMERIC(12,8)` for scores and cutoffs |
| `2026-02-25-create-genetic-variants-features.sql` | Creates `genetic_variants_features` |
| `2026-06-23-create-geneset-tables.sql` | Creates `geneset_resources` and `geneset_entries` |
| `2026-07-06-create-metabolite-tables.sql` | Creates `metabolite_reference`, `metabolite_xref`, `signature_feature_set_ambiguity` |
| `2026-08-19-gene-search-indexes.sql` | Adds the three indexes gene-content search needs |
| `2026-08-25-metabolite-reference-columns.sql` | Adds `refmet_id` and `hmdb_id`, drops the unused `chemical_name` |
| `2026-08-26-assay-type-genetic-variants.sql` | Makes `genetic_variants` the fifth `assay_type`, rewriting `snps` and `genetic_variations` |
| `2026-09-13-signature-feature-set-keys.sql` | Gives `signature_feature_set` the production keys, nullability and two new columns |
| `2026-09-25-rename-type-platform.sql` | Renames `signatures.direction_type` to `type` and `platforms.platform_name` to `platform` |

All but the last were written on 2026-09-30, after the fact, for schema
changes that had shipped without a migration. Each is dated by the commit that
changed `mysql/schema/`, so they apply in the order the changes were made.

Schema history in this repository starts on 2025-09-18. A database built
before that, or from one of the commits between 2025-12-11 and 2026-06-23
whose schema files were not valid SQL, has a schema there is no record of.
`update_sigrepo.sh` detects that by comparing the migrated schema with
`mysql/expected_schema.tsv` and stops.
