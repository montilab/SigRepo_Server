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

## Deploying this migration: rebuild the image, don't just restart it

Renaming these columns immediately breaks any running code that still
queries the old column names: the Plumber API, the MCP server, and the
legacy Shiny app in this repo (`SigRepo_Server`), plus the R client package
in the separate `SigRepo` repo, which now also requires `OmicSignature (>=
1.4.0)` (see `SigRepo/DESCRIPTION`) because building an `OmicSignature`
object with `type =` metadata is a hard validation error on any older
`OmicSignature`. The database migration and the code changes in both repos
must be deployed together -- but "deploy the code changes" is **not** the
same as "restart the containers", and getting that distinction wrong is
exactly what breaks `/signatures/compare`, the GEM enrichment route, and
Annotate on this rename.

**What actually determines which client code runs**, verified directly
against `docker-compose-vm.yml` and the current `montilab/sigrepo:latest`
image (do not take this on faith -- re-check both if either changes):

- `sigrepo-api`, `sigrepo-mcp`, and `sigrepo-shiny` all run
  `image: montilab/sigrepo:latest`. That image's `Dockerfile` installs
  `SigRepo` and `OmicSignature` with `remotes::install_github()` at build
  time -- they are ordinary installed R packages baked into the image, not
  loaded from a working copy by default.
- `docker-compose-vm.yml` bind-mounts `/SigRepo` (the `sigrepo-volume`) into
  all three of those services, and `/OmicSignature`
  (`omic-signature-volume`) into **only `sigrepo-shiny`** -- it appears
  exactly once in the compose file, at the `sigrepo-shiny` service. Neither
  `sigrepo-api` nor `sigrepo-mcp` mount `/OmicSignature` at all.
- A bind mount only changes which code runs if something actually loads
  from it. `legacy_app/app_src/bootstrap.R` (Shiny) calls
  `load_repo_package("OMICSIG_DIR", "OmicSignature")`, so Shiny at least
  attempts to load OmicSignature from the mounted source. `api/api.R` and
  `mcp/run_sigrepo_mcp.R` never call `load_repo_package` for OmicSignature
  at all -- only for `SigRepo` -- so the API and MCP always run the
  image-installed OmicSignature regardless of any mount.
- `load_repo_package()` itself only loads from a mounted source path if
  `pkgload` or `devtools` is importable; otherwise it silently falls back to
  `library(package_name)`, i.e. the image-installed version, with no
  warning. Confirmed directly against the running `montilab/sigrepo:latest`
  image: **neither `pkgload` nor `devtools` is installed.**
  `SigRepo_Server/DESCRIPTION`'s `Imports` (what `install_r_packages.R`
  installs) lists neither, `devtools` is `Suggests`-only, and the
  `Dockerfile`'s explicit `remotes::install_github()` calls for
  `SigRepo`/`OmicSignature`/`hypeR`/`hypeR-GEM` all pass
  `dependencies = c('Depends','Imports','LinkingTo')`, which excludes
  `Suggests`.
- The practical consequence: **the `/SigRepo` bind mount is inert in this
  image for all three services, and the `/OmicSignature` bind mount is
  inert too, including on `sigrepo-shiny`, which is the one service that
  actually tries to use it.** A `git pull` on the host followed by only
  restarting containers changes none of the R package code that
  `sigrepo-api`, `sigrepo-mcp`, or `sigrepo-shiny` execute. (This is
  different from `legacy_app/`, `api/`, and `mcp/` themselves, which
  `shiny-server.sh`/`api-server.sh`/`mcp-server.sh` run directly off the
  bind-mounted `/SigRepo_Server` tree as plain scripts, not as an installed
  package -- `git pull` plus restart genuinely does update those.)
- Checked directly: the `montilab/sigrepo:latest` image pulled for this
  repo's local stack currently has **OmicSignature 1.3.0** installed --
  already below the 1.4.0 floor this rename requires. Restarting that image
  against a migrated database, with no rebuild, reproduces the exact
  production incident recorded in the comment at the top of
  `api/lib/omic_signature.R`: `build_omic_signature()` passes `type =` into
  `OmicSignature$new()`, which is a hard validation error against
  OmicSignature < 1.4.0, so `/signatures/compare` and the GEM enrichment
  route fail on every call, and Annotate breaks wherever it builds a
  signature the same way.

**So the deploy step is:**

1. Apply the migration and verify it per "Verifying a run" above.
2. Rebuild the `montilab/sigrepo` image from this branch's `Dockerfile` (or
   re-pull it once CI has published a rebuilt image), so the image's
   installed `SigRepo` and `OmicSignature` are both current -- `SigRepo` at
   a release containing this rename's client changes, `OmicSignature` at
   `>= 1.4.0`. Recreate `sigrepo-api`, `sigrepo-mcp`, and `sigrepo-shiny`
   from that image (`docker compose up -d --force-recreate` or equivalent);
   a plain `restart` reuses the already-running container's already-loaded
   packages and, on `sigrepo-shiny`, its already-started R process, so it is
   not sufficient even after the image is rebuilt.
3. As a backstop, not a substitute for the above: this branch adds a
   boot-time check (`assert_omic_signature_version()` in `api/api.R` and
   `mcp/run_sigrepo_mcp.R`) that refuses to start `sigrepo-api` or
   `sigrepo-mcp` at all if the installed `OmicSignature` is older than
   1.4.0. This turns a silent per-request 500 into an immediate, loud
   startup failure, but it does not make the mismatched image work -- the
   fix is still to rebuild it.

Do not leave a migrated database pointed at old code, and do not deploy new
code against an unmigrated database: the two must move together, per
environment. A given environment (local, BUMC, production) is migrated and
running a rebuilt image with matching code, or it is unmigrated and running
the old image; it should never sit in between.

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
  `platforms.platform_name` to `platforms.platform`, and also renames the
  `platforms` unique index from `platform_name` to `platform` so a migrated
  database's index name matches what a fresh install from
  `mysql/schema/platforms.sql` produces. Metadata-only (`RENAME COLUMN` and
  `RENAME INDEX`, not `CHANGE COLUMN`); see the comments in the forward
  script for why `CHANGE COLUMN` is avoided for the `SET(...)` column, and
  for why the index needs its own explicit rename.
