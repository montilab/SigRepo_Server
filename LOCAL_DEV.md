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

## Resetting

```bash
docker compose -f docker-compose-local.yml -p sigrepo_local down
rm -rf .local-data/database/*
docker compose -f docker-compose-local.yml -p sigrepo_local up -d
```
