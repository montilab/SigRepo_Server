#!/usr/bin/env bash
# Print a database's schema as sorted, tab separated lines, or compare it with
# a saved copy.
#
#   scripts/schema_fingerprint.sh --container sigrepo-mysql > mysql/expected_schema.tsv
#   scripts/schema_fingerprint.sh --container sigrepo-mysql --verify mysql/expected_schema.tsv
#
# Connection options are the same as scripts/migrate.sh.
#
# What is recorded, one line each:
#   column  table  column  type  nullable  default  extra
#   index   table  index   non_unique  columns
#   fk      table  column  referenced_table  referenced_column
#
# What is deliberately left out: column order, character sets, CHECK
# constraints, and foreign key names. None of them change what a query sees,
# and all of them differ between a database that was migrated and one that was
# built fresh. The schema_migrations table is left out as well.
#
# --verify compares only the tables the saved copy names. A table the saved
# copy does not know about is reported and is not a failure: an instance may
# hold tables of its own. Exit 0 when they match, 1 when they differ.

set -uo pipefail

HERE=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
# shellcheck source=scripts/lib/mysql_conn.sh
. "$HERE/lib/mysql_conn.sh"

VERIFY=""

while [ $# -gt 0 ]; do
  if conn_parse_arg "$@"; then shift "$CONN_SHIFT"; continue; fi
  case "$1" in
    --verify)  VERIFY=$2; shift 2 ;;
    -h|--help) sed -n '2,22p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

conn_require || exit 2

fingerprint() {
  {
    conn_query "SELECT 'column', TABLE_NAME, COLUMN_NAME, COLUMN_TYPE, IS_NULLABLE,
                       IFNULL(COLUMN_DEFAULT, 'NULL'), EXTRA
                  FROM information_schema.COLUMNS
                 WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME <> 'schema_migrations'" &&
    conn_query "SELECT 'index', TABLE_NAME, INDEX_NAME, NON_UNIQUE,
                       GROUP_CONCAT(CONCAT(COLUMN_NAME, IFNULL(CONCAT('(', SUB_PART, ')'), ''))
                                    ORDER BY SEQ_IN_INDEX SEPARATOR ',')
                  FROM information_schema.STATISTICS
                 WHERE TABLE_SCHEMA = DATABASE() AND TABLE_NAME <> 'schema_migrations'
                 GROUP BY TABLE_NAME, INDEX_NAME, NON_UNIQUE" &&
    conn_query "SELECT 'fk', TABLE_NAME, COLUMN_NAME, REFERENCED_TABLE_NAME, REFERENCED_COLUMN_NAME
                  FROM information_schema.KEY_COLUMN_USAGE
                 WHERE TABLE_SCHEMA = DATABASE() AND REFERENCED_TABLE_NAME IS NOT NULL"
  } | LC_ALL=C sort
}

if [ -z "$VERIFY" ]; then
  fingerprint
  exit $?
fi

if [ ! -s "$VERIFY" ]; then
  echo "no saved schema at $VERIFY" >&2
  exit 2
fi

LIVE=$(mktemp)
KNOWN=$(mktemp)
trap 'rm -f "$LIVE" "$KNOWN" "$LIVE.same"' EXIT

if ! fingerprint > "$LIVE" || [ ! -s "$LIVE" ]; then
  echo "could not read the schema" >&2
  exit 1
fi

# Column 2 is the table name on every kind of line.
cut -f2 "$VERIFY" | LC_ALL=C sort -u > "$KNOWN"

awk -F'\t' 'NR == FNR { known[$1] = 1; next } ($2 in known)' "$KNOWN" "$LIVE" > "$LIVE.same"
awk -F'\t' 'NR == FNR { known[$1] = 1; next } !($2 in known) { print $2 }' "$KNOWN" "$LIVE" \
  | LC_ALL=C sort -u | while IFS= read -r extra; do
      echo "note: table '$extra' is not part of SigRepo's schema; left alone"
    done

if diff "$VERIFY" "$LIVE.same" > /dev/null; then
  echo "schema matches"
  exit 0
fi

echo "schema differs from what this version expects (< expected, > found):" >&2
diff "$VERIFY" "$LIVE.same" >&2
exit 1
