#!/usr/bin/env bash
# Exercise scripts/migrate.sh: what it records, what it skips, and what it does
# when a migration fails.
#
#   bash tests/shell/test_migrate.sh --container NAME
#   MYSQL_PWD=... bash tests/shell/test_migrate.sh --host HOST --user root
#
# Needs a disposable MySQL; see tests/shell/testlib.sh. The migrations here are
# fixtures written to a temporary directory, so this test says nothing about
# the real ones. tests/shell/test_history_upgrade.sh covers those.
set -uo pipefail

. "$(dirname "$0")/testlib.sh"
parse_conn_args "$@"

DB=sigrepo_test_runner
MIGRATE="$ROOT/scripts/migrate.sh"
DIR=$(mktemp -d)
trap 'rm -rf "$DIR"; admin_sql "DROP DATABASE IF EXISTS \`$DB\`" >/dev/null 2>&1' EXIT

script_args "$DB"

recorded() { db_sql "$DB" "SELECT name FROM schema_migrations ORDER BY name" | tr '\n' ' '; }

echo "--- a first run applies everything, in filename order ---"

fresh_db "$DB"
echo "CREATE TABLE t (id INT);"              > "$DIR/2026-01-01-create.sql"
echo "ALTER TABLE t ADD COLUMN added INT;"   > "$DIR/2026-01-02-alter.sql"
mkdir "$DIR/rollback"
echo "DROP TABLE t;"                         > "$DIR/rollback/2026-01-01-create-rollback.sql"
# A rollback script left beside the forward migrations, where it sorts last.
echo "DROP TABLE t;"                         > "$DIR/2026-01-02-alter-rollback.sql"

OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" 2>&1); RC=$?
if [ "$RC" -eq 0 ] && [ "$(recorded)" = "2026-01-01-create.sql 2026-01-02-alter.sql " ]; then
  ok "both migrations applied and recorded"
else
  bad "first run: rc=$RC recorded='$(recorded)'"; printf '%s\n' "$OUT"
fi
if [ "$(db_sql "$DB" "SELECT COUNT(*) FROM information_schema.COLUMNS WHERE TABLE_SCHEMA='$DB' AND TABLE_NAME='t'")" = "2" ]; then
  ok "the ALTER ran after the CREATE, and no rollback script was run"
else
  bad "table t does not have its two columns"
fi

echo "--- a second run changes nothing ---"

OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" 2>&1); RC=$?
if [ "$RC" -eq 0 ] && printf '%s' "$OUT" | grep -q "0 applied, 2 already recorded"; then
  ok "nothing is applied twice"
else
  bad "second run: rc=$RC"; printf '%s\n' "$OUT"
fi

echo "--- a failing migration stops the run and is not recorded ---"

echo "ALTER TABLE no_such_table ADD COLUMN x INT;" > "$DIR/2026-01-03-broken.sql"
echo "ALTER TABLE t ADD COLUMN later INT;"         > "$DIR/2026-01-04-after.sql"

OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" 2>&1); RC=$?
if [ "$RC" -eq 1 ] && printf '%s' "$OUT" | grep -q "FAILED   2026-01-03-broken.sql"; then
  ok "the failure is reported by name with exit 1"
else
  bad "failing run: rc=$RC"; printf '%s\n' "$OUT"
fi
if [ "$(recorded)" = "2026-01-01-create.sql 2026-01-02-alter.sql " ]; then
  ok "neither the failed migration nor the one after it is recorded"
else
  bad "recorded after failure: '$(recorded)'"
fi

echo "--- --status lists what is pending and changes nothing ---"

OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" --status 2>&1); RC=$?
if [ "$RC" -eq 0 ] && printf '%s' "$OUT" | grep -q "pending  2026-01-03-broken.sql" \
   && printf '%s' "$OUT" | grep -q "2 pending, 2 already recorded"; then
  ok "--status names the pending migrations"
else
  bad "--status: rc=$RC"; printf '%s\n' "$OUT"
fi

echo "--- after the cause is fixed, a re-run finishes the job ---"

echo "ALTER TABLE t ADD COLUMN x INT;" > "$DIR/2026-01-03-broken.sql"
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" 2>&1); RC=$?
if [ "$RC" -eq 0 ] && printf '%s' "$OUT" | grep -q "2 applied, 2 already recorded"; then
  ok "the re-run applies only what was left"
else
  bad "re-run: rc=$RC"; printf '%s\n' "$OUT"
fi

echo "--- --status on a database with no schema_migrations table ---"

fresh_db "$DB"
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" --status 2>&1); RC=$?
TABLES=$(db_sql "$DB" "SELECT COUNT(*) FROM information_schema.TABLES WHERE TABLE_SCHEMA='$DB'")
if [ "$RC" -eq 0 ] && printf '%s' "$OUT" | grep -q "4 pending, 0 already recorded" && [ "$TABLES" = "0" ]; then
  ok "everything is pending, and --status created no table"
else
  bad "--status on an untracked database: rc=$RC tables=$TABLES"; printf '%s\n' "$OUT"
fi

echo "--- a migration file with an unexpected name is refused ---"

echo "SELECT 1;" > "$DIR/2026-01-05-it's.sql"
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" 2>&1); RC=$?
if [ "$RC" -eq 1 ] && printf '%s' "$OUT" | grep -q "unexpected name"; then
  ok "a quote in a file name never reaches SQL"
else
  bad "odd file name: rc=$RC"; printf '%s\n' "$OUT"
fi
rm -f "$DIR/2026-01-05-it's.sql"

echo "--- an unreachable database is a clean failure ---"

script_args sigrepo_test_does_not_exist
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" --dir "$DIR" 2>&1); RC=$?
if [ "$RC" -eq 1 ] && printf '%s' "$OUT" | grep -q "cannot reach the database"; then
  ok "a missing database is reported, not half-run"
else
  bad "missing database: rc=$RC"; printf '%s\n' "$OUT"
fi

finish
