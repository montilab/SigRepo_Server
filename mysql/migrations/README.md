# mysql/migrations

Hand-applied schema migrations for the SigRepo database. There is no migration
runner: each file is a plain SQL script, applied in order, by hand, once per
environment. Every migration here is idempotent (guarded on
`information_schema`), so re-running one that has already been applied is a
safe no-op.

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

## Restart the API after renaming

Renaming these columns immediately breaks any running API (and Shiny)
process whose code still queries the old column names. The migration and the
corresponding application code change must be deployed together:

1. Apply the migration.
2. Deploy/restart the API (and Shiny, where applicable) container so it is
   running code that queries the new column names.

Do not leave a migrated database pointed at old code, and do not deploy new
code against an unmigrated database: the two must move together, per
environment. A given environment (local, BUMC, production) is migrated and
running matching code, or it is unmigrated and running old code; it should
never sit in between.

## Migrations in this directory

- `2026-09-25-rename-type-platform.sql` /
  `2026-09-25-rename-type-platform-rollback.sql`: renames
  `signatures.direction_type` to `signatures.type` and
  `platforms.platform_name` to `platforms.platform`. Metadata-only
  (`RENAME COLUMN`, not `CHANGE COLUMN`); see the comments in the forward
  script for why `CHANGE COLUMN` is avoided for the `SET(...)` column.
