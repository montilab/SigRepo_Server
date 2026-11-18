#!/usr/bin/env bash
# Check that mysql/expected_schema.tsv is the schema mysql/schema/ builds.
#
#   bash tests/shell/test_expected_schema.sh --container NAME
#   MYSQL_PWD=... bash tests/shell/test_expected_schema.sh --host HOST --user root
#
# Needs a disposable MySQL; see tests/shell/testlib.sh.
#
# Add --write to regenerate the file instead of checking it.
#
# update_sigrepo.sh compares a migrated database with that file and stops the
# update when they differ, so a stale file would stop every update. When this
# fails after a schema change, regenerate the file with the command it prints
# and commit it with the change.
set -uo pipefail

. "$(dirname "$0")/testlib.sh"

# --write regenerates the file from mysql/schema/ in place of checking it.
WRITE=0
ARGS=()
for arg in "$@"; do
  if [ "$arg" = "--write" ]; then WRITE=1; else ARGS+=("$arg"); fi
done
parse_conn_args "${ARGS[@]}"

DB=sigrepo_test_expected
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"; admin_sql "DROP DATABASE IF EXISTS \`$DB\`" >/dev/null 2>&1' EXIT

fresh_db "$DB"
schema_at WORKTREE > "$WORK/schema.sql"
db_load "$DB" < "$WORK/schema.sql" || { bad "mysql/schema/ does not load"; finish; }
script_args "$DB"

if [ "$WRITE" -eq 1 ]; then
  bash "$ROOT/scripts/schema_fingerprint.sh" "${SCRIPT_ARGS[@]}" > "$ROOT/mysql/expected_schema.tsv"
  echo "wrote mysql/expected_schema.tsv ($(wc -l < "$ROOT/mysql/expected_schema.tsv" | tr -d ' ') lines)"
  exit 0
fi

if bash "$ROOT/scripts/schema_fingerprint.sh" "${SCRIPT_ARGS[@]}" --verify "$ROOT/mysql/expected_schema.tsv" > "$WORK/out.txt" 2>&1; then
  ok "mysql/expected_schema.tsv matches mysql/schema/"
else
  bad "mysql/expected_schema.tsv is stale"
  cat "$WORK/out.txt"
  echo
  echo "Regenerate it against a database built from mysql/schema/, for example:"
  echo "  bash tests/shell/test_expected_schema.sh ${CONN_ARGS[*]} --write"
fi

# A table added to mysql/schema/ but missing from the saved copy is only a
# "note" to --verify, which is right for an instance and wrong here.
bash "$ROOT/scripts/schema_fingerprint.sh" "${SCRIPT_ARGS[@]}" > "$WORK/live.tsv"
if diff -q "$ROOT/mysql/expected_schema.tsv" "$WORK/live.tsv" >/dev/null; then
  ok "and it names every table mysql/schema/ creates"
else
  bad "mysql/schema/ creates something mysql/expected_schema.tsv does not list"
  diff "$ROOT/mysql/expected_schema.tsv" "$WORK/live.tsv" | head -20
fi

finish
