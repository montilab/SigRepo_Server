#!/usr/bin/env bash
# Shared by the shell tests in this directory. Sourced, not run.
#
# The database tests create and drop scratch databases whose names start with
# sigrepo_test_, on whatever server the connection options name. Point them at
# a disposable MySQL: CI's service container, or a throwaway one started with
#
#   docker run -d --name sigrepo-test-mysql \
#     -e MYSQL_ROOT_PASSWORD=scratch_pw_123 -e MYSQL_DATABASE=sigrepo_test mysql:8.0
#
# and then `bash tests/shell/test_migrate.sh --container sigrepo-test-mysql`.

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)
FAILURES=0

ok()  { echo "PASS  $1"; }
bad() { echo "FAIL  $1"; FAILURES=$((FAILURES + 1)); }

finish() {
  echo
  if [ "$FAILURES" -eq 0 ]; then echo "all passed"; exit 0; fi
  echo "$FAILURES failed"; exit 1
}

# shellcheck source=scripts/lib/mysql_conn.sh
. "$ROOT/scripts/lib/mysql_conn.sh"

# The options the test was started with, passed on to the scripts under test.
CONN_ARGS=()

parse_conn_args() {
  while [ $# -gt 0 ]; do
    if conn_parse_arg "$@"; then
      CONN_ARGS+=("${@:1:$CONN_SHIFT}")
      shift "$CONN_SHIFT"
    else
      echo "unknown argument: $1" >&2; exit 2
    fi
  done
  if [ -z "$CONN_CONTAINER" ] && [ -z "$CONN_HOST" ]; then
    echo "give --container NAME or --host HOST (see tests/shell/testlib.sh)" >&2; exit 2
  fi
  # The server's default database is only used to issue CREATE/DROP DATABASE.
  ADMIN_DB=${CONN_DB:-}
  if [ -n "$CONN_HOST" ] && [ -z "$ADMIN_DB" ]; then ADMIN_DB=mysql; fi
}

# admin_sql "SQL": run against the server, not a scratch database.
admin_sql() {
  CONN_DB=$ADMIN_DB conn_query "$1"
}

# fresh_db NAME: drop and recreate an empty scratch database.
fresh_db() {
  case "$1" in
    sigrepo_test_*) ;;
    *) echo "refusing to drop '$1': scratch databases are named sigrepo_test_*" >&2; exit 2 ;;
  esac
  admin_sql "DROP DATABASE IF EXISTS \`$1\`" && admin_sql "CREATE DATABASE \`$1\`"
}

# db_sql NAME "SQL": one statement against a scratch database.
db_sql() {
  CONN_DB=$1 conn_query "$2"
}

# db_load NAME: SQL on stdin into a scratch database.
db_load() {
  CONN_DB=$1 conn_mysql
}

# script_args NAME: the connection options for a script under test, aimed at
# a scratch database.
script_args() {
  local i skip=0
  SCRIPT_ARGS=()
  for i in "${CONN_ARGS[@]}"; do
    if [ "$skip" -eq 1 ]; then skip=0; continue; fi
    if [ "$i" = "--database" ]; then skip=1; continue; fi
    SCRIPT_ARGS+=("$i")
  done
  SCRIPT_ARGS+=(--database "$1")
}

# schema_at COMMIT: the CREATE TABLE statements mysql/schema/ held at that
# commit, as one script. WORKTREE means the files on disk now.
# snps_features.sql is skipped: generate_db_schema() never created that table.
schema_at() {
  local f
  echo "SET FOREIGN_KEY_CHECKS=0;"
  if [ "$1" = "WORKTREE" ]; then
    for f in "$ROOT"/mysql/schema/*.sql; do cat "$f"; echo ";"; done
  else
    git -C "$ROOT" ls-tree --name-only "$1" mysql/schema/ | grep -v 'snps_features\.sql$' |
      while IFS= read -r f; do git -C "$ROOT" show "$1:$f"; echo ";"; done
  fi
  echo "SET FOREIGN_KEY_CHECKS=1;"
}
