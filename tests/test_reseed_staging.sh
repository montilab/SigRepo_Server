#!/usr/bin/env bash
# Exercise reseed_staging.sh's refusals, and the exit-status handling that only
# shows up on a SUCCESSFUL restore.
#
#   bash tests/test_reseed_staging.sh [path to reseed_staging.sh]
#
# Run by hand. Needs docker only for the "container is not running" case; every
# other case needs nothing.
#
# What matters about this script is what it refuses, because it drops a
# database and production MySQL is called "sigrepo-mysql", one character away
# from the "sigrepo-local-mysql" it is meant for. The success-path cases matter
# for the opposite reason: a restore that WORKS must report that it worked.
set -uo pipefail

SCRIPT=${1:-$(cd "$(dirname "$0")/.." && pwd)/scripts/reseed_staging.sh}
FAILURES=0
TMP=$(mktemp -d)
trap 'rm -rf "$TMP"' EXIT

ok()  { echo "PASS  $1"; }
bad() { echo "FAIL  $1"; FAILURES=$((FAILURES+1)); }

expect_abort() {
  local label=$1 want=$2 out=$3 rc=$4
  if [ "$rc" -eq 0 ]; then bad "$label (exited 0, expected an abort)"; return; fi
  if printf '%s' "$out" | grep -q "$want"; then ok "$label"; else
    bad "$label (no '$want' in output)"; printf '%s\n' "$out" | tail -3
  fi
}

echo "--- refusals ---"

for c in sigrepo-mysql mysql sigrepo_db sigrepo-local-mysql-backup; do
  OUT=$(CONTAINER="$c" bash "$SCRIPT" restore "$TMP/nothing.sql.gz" 2>&1); RC=$?
  expect_abort "container '$c' is refused" "refusing to operate on container" "$OUT" "$RC"
done

OUT=$(bash "$SCRIPT" frobnicate "$TMP/x.sql.gz" 2>&1); RC=$?
expect_abort "unknown mode is refused" "unknown mode" "$OUT" "$RC"

OUT=$(bash "$SCRIPT" restore 2>&1); RC=$?
expect_abort "missing argument is refused" "usage:" "$OUT" "$RC"

echo "not gzip" > "$TMP/corrupt.sql.gz"
OUT=$(bash "$SCRIPT" restore "$TMP/corrupt.sql.gz" 2>&1); RC=$?
expect_abort "corrupt dump is refused" "is corrupt" "$OUT" "$RC"

OUT=$(bash "$SCRIPT" restore "$TMP/not-here.sql.gz" 2>&1); RC=$?
expect_abort "missing file is refused" "no such file" "$OUT" "$RC"

echo "--- exit-status handling on a SUCCESSFUL restore ---"
#
# The restore pipeline is:
#
#   gunzip -c FILE | docker exec -i CONTAINER mysql 2>&1 | grep -v "Using a password"
#
# On success mysql exits 0 and prints only its password warning, grep filters
# every line and exits 1, so the pipeline exits 1. Whatever swallows that must
# not also destroy PIPESTATUS, because PIPESTATUS[1] is how mysql's own status
# is read. A trailing `|| true` DOES destroy it: `true` runs, and PIPESTATUS
# becomes its one-element status. Under `set -u` the next line is then fatal —
# on a restore that actually worked.
#
# These two cases drive the real construct, extracted from the script, with a
# stand-in for mysql that succeeds and prints only the warning.

WARN='mysql: [Warning] Using a password on the command line interface can be insecure.'

trailing_or_true() (
  set -uo pipefail
  echo "$WARN" | cat | grep -v "Using a password" || true
  status=${PIPESTATUS[1]}
  echo "status=$status"
)

guarded_or_true() (
  set -uo pipefail
  echo "$WARN" | cat | { grep -v "Using a password" || true; }
  status=${PIPESTATUS[1]}
  echo "status=$status"
)

OUT=$(trailing_or_true 2>&1); RC=$?
if [ "$RC" -ne 0 ] || ! printf '%s' "$OUT" | grep -q "status=0"; then
  ok "reproduced the trap: a trailing '|| true' makes PIPESTATUS[1] fatal"
else
  ok "this bash keeps PIPESTATUS after '|| true'; the guarded form is still correct"
fi

OUT=$(guarded_or_true 2>&1); RC=$?
if [ "$RC" -eq 0 ] && printf '%s' "$OUT" | grep -q "status=0"; then
  ok "the guarded form reads mysql's own status as 0"
else
  bad "the guarded form did not survive (rc=$RC): $OUT"
fi

# And the script itself must use the guarded form, not the trailing one.
if grep -qE '\|[[:space:]]*grep -v "Using a password"[[:space:]]*\|\|[[:space:]]*true' "$SCRIPT"; then
  bad "the script still ends its restore pipeline with a bare '|| true' before reading PIPESTATUS"
else
  ok "the script does not end the restore pipeline with a bare '|| true'"
fi

# The pre-restore backup is the only copy of whatever was in staging. gzip
# writes a valid empty archive when its input fails, so an unchecked backup can
# be two bytes of nothing and the script would drop the database anyway.
if grep -q 'gunzip -t "\$backup"' "$SCRIPT"; then
  ok "the pre-restore backup is verified before the drop"
else
  bad "the pre-restore backup is written but never verified before the drop"
fi

echo "=== $FAILURES failure(s) ==="
[ "$FAILURES" -eq 0 ]
