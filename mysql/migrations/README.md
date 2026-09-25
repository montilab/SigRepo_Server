# mysql/migrations

Hand-applied schema migrations for the SigRepo database. There is no migration
runner and no migration-tracking table in this project: each file is a plain
SQL script, applied in order, by hand, once per environment, and the
`information_schema` query in "Verifying a run" below is the only way to know
whether a given environment has been migrated.

Every migration here is idempotent (guarded on `information_schema`), so
re-running one that has already been applied is a safe no-op. Each script
also checks, as its first action, that a default database was actually
selected on the connection, and, as its last action, that the columns it was
supposed to produce actually exist; either check aborts the script loudly
rather than let it exit 0 having silently changed nothing. See the comments
at the top of each `.sql` file for why that matters.

Requires MySQL 8.0 or later. These migrations use `RENAME COLUMN`, which does
not exist before 8.0. Do not run them against an older server.

## Applying a migration to the local stack

From the repo root, with the local stack running:

```bash
set -a; . /path/to/sigrepo-local/.env.local; set +a
docker exec -i sigrepo-local-mysql mysql -u"$MYSQL_USER" -p"$MYSQL_PASSWORD" sigrepo \
  < mysql/migrations/2026-09-25-rename-type-platform.sql
```

Substitute the actual path to the local stack's env file (it defines
`MYSQL_USER`, `MYSQL_PASSWORD`, `MYSQL_DATABASE`, and the root credentials).
Never hardcode or print these values; always source them from the env file.

To roll a migration back, run its `-rollback.sql` counterpart the same way.

## Before running on montilab.bu.edu or sigrepo.org

These two hosts hold data that cannot be regenerated locally. Before applying
any migration in this directory to either one:

1. Take a `mysqldump` of at least the `signatures` and `platforms` tables
   (schema and data) and store it somewhere outside the database itself.
2. Confirm the dump completed and is non-empty before proceeding.

Only after that backup exists should the migration be applied, using the same
`mysql < file` invocation shown above, adjusted for that host's container
name and credentials.

## Verifying a run

After applying a migration to any environment, before doing anything else
(in particular, before restarting the API), confirm it actually took effect.
The migration's own postcondition check already aborts the script if it did
not, but an operator relying on visual confirmation, or checking a run that
happened earlier, should run this directly:

```bash
docker exec -i sigrepo-local-mysql mysql -u"$MYSQL_USER" -p"$MYSQL_PASSWORD" sigrepo -e \
  "SELECT COLUMN_NAME FROM information_schema.COLUMNS
   WHERE TABLE_SCHEMA='sigrepo' AND TABLE_NAME IN ('signatures','platforms')
     AND COLUMN_NAME IN ('type','platform','direction_type','platform_name');"
```

(Substitute the container name and database name for BUMC or production.)

Expected output after the forward migration is exactly two rows, `type` and
`platform`, with neither `direction_type` nor `platform_name` present.
Expected output after the rollback is exactly two rows, `direction_type` and
`platform_name`, with neither `type` nor `platform` present. Anything else,
including only one of the two renamed columns present, means the database is
in a mixed state; see "Recovering from a failure partway through" below
before doing anything further.

## Restart the API and Shiny after renaming

Renaming these columns immediately breaks any running code that still
queries the old column names: the Plumber API, the MCP server, and the
legacy Shiny app in this repo (`SigRepo_Server`), all of which live under
`api/`, `mcp/`, and `legacy_app/`, plus the R client package in the separate
`SigRepo` repo. The migration and the corresponding code change in both
repos must be deployed together:

1. Apply the migration and verify it per "Verifying a run" above.
2. Deploy/restart the API, MCP, and Shiny processes so they are running code
   that queries the new column names, and deploy the matching `SigRepo`
   client release wherever it is installed.

Do not leave a migrated database pointed at old code, and do not deploy new
code against an unmigrated database: the two must move together, per
environment. A given environment (local, BUMC, production) is migrated and
running matching code, or it is unmigrated and running old code; it should
never sit in between.

## Recovering from a failure partway through

DDL statements in MySQL auto-commit individually and cannot be rolled back
as a group. If a script fails between its two `ALTER TABLE` statements (for
example, the `signatures` rename succeeds but something interrupts the
connection before the `platforms` rename runs), the database is left in a
genuine mixed state: one column renamed, the other not.

This is expected to be recoverable, and the correct recovery is simply to
re-run the same script. Each of the two guarded blocks checks the current
state of its own column independently, so a block whose rename already
happened takes the `DO 0` no-op branch and a block whose rename did not
happen still performs it. Re-running is safe and idempotent even from a
partial failure; it does not need to be run from a fresh database or undone
first.

Do **not** reach for a hand-written `CHANGE COLUMN` to patch up a mixed
state. `signatures.direction_type` (mid-migration, still under either name)
is a `SET(...)` column, and `CHANGE COLUMN` requires restating that
definition. Restating it by hand from memory or from the schema file is
exactly the mistake that has already truncated data once in this project's
history (see the comment in `mysql/schema/signatures.sql`), which is the
whole reason these migrations use `RENAME COLUMN` in the first place.
Re-running the provided script carries none of that risk.

## Migrations in this directory

- `2026-09-25-rename-type-platform.sql` /
  `2026-09-25-rename-type-platform-rollback.sql`: renames
  `signatures.direction_type` to `signatures.type` and
  `platforms.platform_name` to `platforms.platform`. Metadata-only
  (`RENAME COLUMN`, not `CHANGE COLUMN`); see the comments in the forward
  script for why `CHANGE COLUMN` is avoided for the `SET(...)` column.
