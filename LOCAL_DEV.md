# Local development stack

A SigRepo instance on your own machine, shaped to match the production VM so
that what works here works there.

```bash
docker compose -f docker-compose-local.yml -p sigrepo_local up -d
```

| | URL |
|---|---|
| Web interface | http://127.0.0.1:8050 |
| REST API | http://127.0.0.1:8020 |
| MCP | http://127.0.0.1:8021 |
| MySQL | `127.0.0.1:3306` |

## Why not `docker-compose.yml`?

That file is the simple "just give me a SigRepo" arrangement. Production runs
`docker-compose-vm.yml`, which differs in one way that matters: it bind-mounts
the **source** over the image's copy, so the API and MCP run the host checkout
and a deploy is `git pull` + restart, not an image pull.

`docker-compose-local.yml` mirrors that. Editing `api/` on your machine changes
what the container runs — restart it to pick the change up:

```bash
docker compose -f docker-compose-local.yml -p sigrepo_local restart sigrepo-api
```

## Deliberate differences from production

- **Ports bind to `127.0.0.1`.** Production publishes 8020/8050/3306 on
  `0.0.0.0`, which is why those answer directly from the internet bypassing
  nginx and TLS. No reason to reproduce that locally.
- **`SENDMAIL_KEY` is a dummy.** Registration and password reset will report
  that the notification step failed. That is correct: this stack must never
  send real mail.
- **Data lives in `./.local-data/`**, gitignored. Delete it to start clean.
- **`SIGREPO_DIR` points at the bind-mounted `../SigRepo` checkout**, so the API
  loads the client with `pkgload::load_all()` from your working copy. Production
  leaves `SIGREPO_DIR` unset, which makes `load_repo_package()` fall through to
  the *installed* package built from `montilab/SigRepo@master`.

  **These are different clients, and they have diverged.** A function whose
  signature changed in an uncommitted working copy will work here and fail on
  production. That is not hypothetical: `createOmicSignature()` gained
  `difexp`/`fetch_difexp` arguments locally that are not on master, and the
  hypeR-GEM feature passed every local test while failing on all three of
  production's metabolomics signatures.

  To exercise the production path, load the installed client explicitly instead
  of letting `api.R` pick:

  ```r
  # In a container Rscript -- library(), NOT pkgload::load_all("/SigRepo")
  suppressMessages(library(SigRepo))
  cat(paste(names(formals(SigRepo:::createOmicSignature)), collapse = ", "), "\n")
  # 2 arguments  -> you are on the production path
  # 4 arguments  -> you are on the working-copy path
  ```

## First run

The database starts empty. Load the schema:

```bash
{ echo "SET FOREIGN_KEY_CHECKS=0;"; for f in mysql/schema/*.sql; do cat "$f"; echo ";"; done; \
  echo "SET FOREIGN_KEY_CHECKS=1;"; } > /tmp/schema.sql
docker cp /tmp/schema.sql sigrepo-local-mysql:/tmp/schema.sql
docker exec sigrepo-local-mysql sh -c 'mysql -uroot -p"$MYSQL_ROOT_PASSWORD" sigrepo < /tmp/schema.sql'
```

For test fixtures instead of your own data, `tests/testthat/fixtures/seed.sql`
loads the same way.

Schema alone leaves you with no signatures, no accounts, and no reference
features — enough to boot, not enough to exercise anything. A repeatable
alternative is a seed script kept in `.local-data/` (gitignored, so it does not
ship here): it loads the schema, creates the dev accounts, and imports a
signature basket downloaded from a real repository via `/signatures/export`.
That gives you genuine data across all four assay types rather than invented
fixtures, which matters — synthetic transcriptomics signatures will not exercise
the metabolomics or genetic-variants paths at all.

## Accounts

Login authenticates against MySQL, not a stored hash — a SigRepo account is a
**MySQL account plus a `users` row**, and both halves are required:

```sql
CREATE USER 'you'@'%' IDENTIFIED BY '<password>';
GRANT ALL PRIVILEGES ON sigrepo.* TO 'you'@'%' WITH GRANT OPTION;
INSERT INTO users (user_name, user_password_hashkey, user_email, user_role,
                   api_key, active, user_hashkey)
VALUES ('you', MD5(LOWER('<password>')), 'you@local.test', 'admin',
        '<32 chars>', 1, '<32 chars>');
```

A `users` row on its own produces an account that can never sign in.

## When the stack comes up with no environment

`.Renviron` is bind-mounted as a **single file**, not a directory:

```
./.Renviron.local-stack  ->  /SigRepo_Server/.Renviron
```

If that host file is ever missing when a container starts — deleted, renamed, or
removed by a branch switch — Docker **silently recreates the mount source as an
empty directory**. The container then comes up with an unreadable `.Renviron`,
every `Sys.getenv()` returns `""`, and `DB_LOCAL_HOST` being empty makes RMySQL
fall back to a local socket:

```
Failed to connect to database: Error: Can't connect to local MySQL server
through socket '/var/run/mysqld/mysqld.sock' (2)
```

That error names MySQL, but MySQL is fine — the environment is missing. Check
the type of the host path before anything else, because `[ -e ]` is true for the
directory Docker left behind:

```bash
[ -f .Renviron.local-stack ] && echo file || echo "NOT A FILE -- this is the bug"
```

To fix: stop the containers that mount it, `rmdir` the empty directory, restore
the real file, and start again. Restarting without removing the directory will
not help — the mount is re-resolved from the host path each time.

## Resetting

```bash
docker compose -f docker-compose-local.yml -p sigrepo_local down
rm -rf .local-data/database/*
docker compose -f docker-compose-local.yml -p sigrepo_local up -d
```
