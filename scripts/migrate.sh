#!/usr/bin/env bash
# Apply the schema migrations a database has not had yet, and record them.
#
#   scripts/migrate.sh --container sigrepo-mysql
#   scripts/migrate.sh --container sigrepo-mysql --status
#   MYSQL_PWD=... scripts/migrate.sh --host 127.0.0.1 --database sigrepo
#
# Options: --container NAME | --host HOST [--port P] [--user U]
#          --database DB    (container mode defaults to the container's own)
#          --dir DIR        migration files (default: mysql/migrations beside this script)
#          --status         list what is pending and change nothing
#
# Every *.sql file directly in the migrations directory is a forward
# migration, applied in filename order. Rollback scripts live in the rollback/
# subfolder; a file named *-rollback.sql is skipped wherever it is, so a stray
# copy can never be run as if it were a migration. Each one is recorded in the
# schema_migrations table when the mysql client exits 0 for it, and the run
# stops at the first one that does not. A file already recorded is skipped.
#
# A database with no schema_migrations table has every file run against it.
# That is safe because every migration is idempotent: one whose change is
# already present takes its no-op branch and is then recorded. See
# mysql/migrations/README.md.
#
# Exit status: 0 nothing left pending, 1 a migration failed, 2 usage.

set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=scripts/lib/mysql_conn.sh
. "$HERE/lib/mysql_conn.sh"

DIR="$HERE/../mysql/migrations"
STATUS_ONLY=0

while [ $# -gt 0 ]; do
  if conn_parse_arg "$@"; then shift "$CONN_SHIFT"; continue; fi
  case "$1" in
    --dir)     DIR=$2; shift 2 ;;
    --status)  STATUS_ONLY=1; shift ;;
    -h|--help) sed -n '2,26p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

conn_require || exit 2

if [ ! -d "$DIR" ]; then
  echo "no migrations directory at $DIR" >&2
  exit 2
fi

if ! conn_query "SELECT 1" >/dev/null; then
  echo "cannot reach the database; nothing was changed" >&2
  exit 1
fi

if [ "$STATUS_ONLY" -eq 0 ]; then
  conn_query "CREATE TABLE IF NOT EXISTS schema_migrations (
      name VARCHAR(255) NOT NULL,
      applied_at DATETIME NOT NULL DEFAULT CURRENT_TIMESTAMP,
      PRIMARY KEY (name)
    ) ENGINE=InnoDB DEFAULT CHARSET=utf8 COLLATE=utf8_unicode_ci" || exit 1
fi

# An absent table (only possible with --status) means nothing is recorded.
RECORDED=$(conn_query "SELECT name FROM schema_migrations" 2>/dev/null || true)

applied=0
skipped=0
pending=0

while IFS= read -r file; do
  [ -n "$file" ] || continue
  name=$(basename "$file")

  # The name goes into an SQL string below, so refuse anything unusual.
  case "$name" in
    *[!A-Za-z0-9._-]*) echo "refusing migration with an unexpected name: $name" >&2; exit 1 ;;
  esac

  if printf '%s\n' "$RECORDED" | grep -qxF "$name"; then
    skipped=$((skipped + 1))
    continue
  fi

  if [ "$STATUS_ONLY" -eq 1 ]; then
    echo "pending  $name"
    pending=$((pending + 1))
    continue
  fi

  if ! conn_mysql < "$file"; then
    echo "FAILED   $name" >&2
    echo "Nothing was recorded for it. Fix the cause and run this again:" >&2
    echo "migrations already applied are skipped, and this one is safe to repeat." >&2
    exit 1
  fi
  conn_query "INSERT INTO schema_migrations (name) VALUES ('$name')" || exit 1
  echo "applied  $name"
  applied=$((applied + 1))
done <<EOF
$(find "$DIR" -maxdepth 1 -type f -name '*.sql' ! -name '*-rollback.sql' | LC_ALL=C sort)
EOF

if [ "$STATUS_ONLY" -eq 1 ]; then
  echo "$pending pending, $skipped already recorded"
else
  echo "$applied applied, $skipped already recorded"
fi
