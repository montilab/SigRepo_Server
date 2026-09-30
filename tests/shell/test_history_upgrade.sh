#!/usr/bin/env bash
# Prove that a database built from any schema this repository has ever shipped
# ends up, after the migrations, with the schema a fresh install has today.
#
#   bash tests/shell/test_history_upgrade.sh --container NAME
#   MYSQL_PWD=... bash tests/shell/test_history_upgrade.sh --host HOST --user root
#
# Needs a disposable MySQL (see tests/shell/testlib.sh) and full git history:
# in CI, check out with fetch-depth 0.
#
# This is the test that stands behind updating an install nobody has a record
# of. For every commit that changed mysql/schema/, it builds the schema as it
# was, runs every migration, and compares the result with a database built
# from mysql/schema/ as it is now. A schema change that ships without a
# migration fails here, because the commit before it no longer converges.
set -uo pipefail

. "$(dirname "$0")/testlib.sh"
parse_conn_args "$@"

OLD=sigrepo_test_history
NEW=sigrepo_test_fresh
MIGRATE="$ROOT/scripts/migrate.sh"
FINGERPRINT="$ROOT/scripts/schema_fingerprint.sh"
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"; admin_sql "DROP DATABASE IF EXISTS \`$OLD\`" >/dev/null 2>&1; admin_sql "DROP DATABASE IF EXISTS \`$NEW\`" >/dev/null 2>&1' EXIT

# Commits whose mysql/schema/ cannot be loaded at all, so no install can have
# that schema. Each needs a reason. A commit that fails to load and is NOT
# listed here fails the test.
#
#   5a22eacb               organisms.sql says "DATE DEFAULT CURRENT_DATE", which
#                          MySQL 8.0 rejects. Replaced the same day by abd4611d.
#   756844a6 .. 3c88d943   proteomics_features.sql says "ADD CONSTRAINT" inside
#                          CREATE TABLE, which is a syntax error. Fixed by
#                          05bebc6d on 2026-06-23. d85a25d2 is a merge commit
#                          inside that range.
UNBUILDABLE="5a22eacb 756844a6 7bb9077e 4fb3dc28 ff6e1bab d85a25d2 3c88d943"

echo "--- the schema a fresh install has today ---"

fresh_db "$NEW"
schema_at WORKTREE > "$WORK/schema.sql"
if db_load "$NEW" < "$WORK/schema.sql"; then
  ok "mysql/schema/ loads"
else
  bad "mysql/schema/ as it is now does not load"; finish
fi
script_args "$NEW"
bash "$FINGERPRINT" "${SCRIPT_ARGS[@]}" > "$WORK/fresh.tsv"
if [ -s "$WORK/fresh.tsv" ]; then ok "fingerprinted"; else bad "empty fingerprint"; finish; fi

echo "--- on that schema, every migration is a no-op ---"

OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); RC=$?
bash "$FINGERPRINT" "${SCRIPT_ARGS[@]}" > "$WORK/fresh_after.tsv"
if [ "$RC" -eq 0 ] && diff -q "$WORK/fresh.tsv" "$WORK/fresh_after.tsv" >/dev/null; then
  ok "migrations leave a current schema unchanged"
else
  bad "migrations changed a current schema (rc=$RC)"; printf '%s\n' "$OUT"
  diff "$WORK/fresh.tsv" "$WORK/fresh_after.tsv"
fi

echo "--- every schema this repository has shipped ---"

COMMITS=$(git -C "$ROOT" log --reverse --format=%h --abbrev=8 -- mysql/schema)
if [ -z "$COMMITS" ]; then bad "no schema history found; is this a shallow clone?"; finish; fi

script_args "$OLD"
for c in $COMMITS; do
  when=$(git -C "$ROOT" log -1 --format=%ad --date=short "$c")
  fresh_db "$OLD"

  schema_at "$c" > "$WORK/schema.sql"
  if ! db_load "$OLD" < "$WORK/schema.sql" 2>"$WORK/load.err"; then
    if printf '%s\n' $UNBUILDABLE | grep -qx "$c"; then
      echo "SKIP  $c $when  (schema files are not valid SQL; listed in UNBUILDABLE)"
    else
      bad "$c $when  schema does not load and is not listed in UNBUILDABLE"
      head -3 "$WORK/load.err"
    fi
    continue
  fi
  if printf '%s\n' $UNBUILDABLE | grep -qx "$c"; then
    bad "$c $when  is listed in UNBUILDABLE but loads; remove it from the list"
    continue
  fi

  if ! OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); then
    bad "$c $when  a migration failed"; printf '%s\n' "$OUT" | tail -5
    continue
  fi
  bash "$FINGERPRINT" "${SCRIPT_ARGS[@]}" > "$WORK/old.tsv"
  if diff "$WORK/fresh.tsv" "$WORK/old.tsv" > "$WORK/diff.txt"; then
    ok "$c $when  converges"
  else
    bad "$c $when  differs from a fresh schema (< fresh, > migrated)"; cat "$WORK/diff.txt"
  fi
done

echo "--- rows survive, and old assay types are rewritten ---"

# The oldest schema: PRIMARY KEY (signature_id, feature_id), NUMERIC(10,8)
# scores, and an assay_type list ending in genetic_variations and
# dna_binding_sites.
FIRST=$(printf '%s\n' $COMMITS | head -1)
fresh_db "$OLD"
schema_at "$FIRST" > "$WORK/schema.sql"
db_load "$OLD" < "$WORK/schema.sql"
db_load "$OLD" <<'SQL'
SET FOREIGN_KEY_CHECKS=0;
INSERT INTO signature_feature_set
  (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
VALUES
  (1, 10, 'p10', 1.50000000, NULL, 'transcriptomics',    'h1'),
  (1, 11, 'p11', -2.25000000, NULL, 'proteomics',        'h2'),
  (2, 20, 'p20', 0.12345678, NULL, 'genetic_variations', 'h3');
SQL
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); RC=$?
ROWS=$(db_sql "$OLD" "SELECT CONCAT_WS('|', signature_id, feature_id, probe_id, score, assay_type) FROM signature_feature_set ORDER BY sig_feature_hashkey" | tr '\n' ' ')
WANT="1|10|p10|1.50000000|transcriptomics 1|11|p11|-2.25000000|proteomics 2|20|p20|0.12345678|genetic_variants "
if [ "$RC" -eq 0 ] && [ "$ROWS" = "$WANT" ]; then
  ok "three rows kept their values; genetic_variations became genetic_variants"
else
  bad "rows after migrating (rc=$RC): $ROWS"; printf '%s\n' "$OUT" | tail -5
fi

echo "--- an assay type with no equivalent stops the migration ---"

fresh_db "$OLD"
schema_at "$FIRST" > "$WORK/schema.sql"
db_load "$OLD" < "$WORK/schema.sql"
db_load "$OLD" <<'SQL'
SET FOREIGN_KEY_CHECKS=0;
INSERT INTO signature_feature_set
  (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
VALUES (1, 10, 'p10', 1.0, NULL, 'dna_binding_sites', 'h1');
SQL
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); RC=$?
KEPT=$(db_sql "$OLD" "SELECT assay_type FROM signature_feature_set")
if [ "$RC" -eq 1 ] && printf '%s' "$OUT" | grep -q "FAILED   2026-08-26-assay-type-genetic-variants.sql" \
   && [ "$KEPT" = "dna_binding_sites" ]; then
  ok "the run stops and the row still holds its value"
else
  bad "dna_binding_sites row (rc=$RC, value now '$KEPT')"; printf '%s\n' "$OUT" | tail -5
fi

echo "--- a row holding two assay types stops the migration ---"

# SET columns can hold several members. Nothing in SigRepo writes more than
# one, so such a row is not something to guess a meaning for.
fresh_db "$OLD"
schema_at "$FIRST" > "$WORK/schema.sql"
db_load "$OLD" < "$WORK/schema.sql"
db_load "$OLD" <<'SQL'
SET FOREIGN_KEY_CHECKS=0;
INSERT INTO signature_feature_set
  (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
VALUES (1, 10, 'p10', 1.0, NULL, 'transcriptomics,proteomics', 'h1');
SQL
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); RC=$?
KEPT=$(db_sql "$OLD" "SELECT assay_type FROM signature_feature_set")
if [ "$RC" -eq 1 ] && printf '%s' "$OUT" | grep -q "FAILED   2026-08-26-assay-type-genetic-variants.sql" \
   && [ "$KEPT" = "transcriptomics,proteomics" ]; then
  ok "the run stops and the row still holds both"
else
  bad "two-member row (rc=$RC, value now '$KEPT')"; printf '%s\n' "$OUT" | tail -5
fi

echo "--- a key change interrupted half way finishes on a re-run ---"

# DDL commits statement by statement, so a dropped connection can leave the
# new unique key added and the old primary key still there.
fresh_db "$OLD"
schema_at "$FIRST" > "$WORK/schema.sql"
db_load "$OLD" < "$WORK/schema.sql"
db_sql "$OLD" "ALTER TABLE signature_feature_set ADD UNIQUE KEY sig_feature_assay_probe (signature_id, feature_id, assay_type, probe_id)"
db_sql "$OLD" "ALTER TABLE signature_feature_set ADD COLUMN nomenclature_type VARCHAR(64) DEFAULT NULL"
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); RC=$?
bash "$FINGERPRINT" "${SCRIPT_ARGS[@]}" > "$WORK/old.tsv"
if [ "$RC" -eq 0 ] && diff "$WORK/fresh.tsv" "$WORK/old.tsv" > "$WORK/diff.txt"; then
  ok "the half-applied database converges"
else
  bad "half-applied key change (rc=$RC)"; printf '%s\n' "$OUT" | tail -5; cat "$WORK/diff.txt"
fi

echo "--- a feature repeated under one probe stops the key change ---"

# The 2026-07-07 shape: PRIMARY KEY (signature_id, group_label, probe_id).
GROUPED=$(git -C "$ROOT" log --format=%h --abbrev=8 -1 12ed4be1 2>/dev/null)
fresh_db "$OLD"
schema_at "$GROUPED" > "$WORK/schema.sql"
db_load "$OLD" < "$WORK/schema.sql"
db_load "$OLD" <<'SQL'
SET FOREIGN_KEY_CHECKS=0;
INSERT INTO signature_feature_set
  (signature_id, feature_id, probe_id, score, group_label, assay_type, sig_feature_hashkey)
VALUES
  (1, 10, 'p10', 1.0, 'group A', 'transcriptomics', 'h1'),
  (1, 10, 'p10', 2.0, 'group B', 'transcriptomics', 'h2');
SQL
OUT=$(bash "$MIGRATE" "${SCRIPT_ARGS[@]}" 2>&1); RC=$?
COUNT=$(db_sql "$OLD" "SELECT COUNT(*) FROM signature_feature_set")
PK=$(db_sql "$OLD" "SELECT COUNT(*) FROM information_schema.STATISTICS WHERE TABLE_SCHEMA='$OLD' AND TABLE_NAME='signature_feature_set' AND INDEX_NAME='PRIMARY'")
if [ "$RC" -eq 1 ] && printf '%s' "$OUT" | grep -q "FAILED   2026-09-13-signature-feature-set-keys.sql" \
   && [ "$COUNT" = "2" ] && [ "$PK" != "0" ]; then
  ok "the run stops with both rows and the old primary key intact"
else
  bad "duplicate rows (rc=$RC, rows=$COUNT, pk columns=$PK)"; printf '%s\n' "$OUT" | tail -5
fi

finish
