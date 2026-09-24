#!/usr/bin/env bash
# Exercise deploy_montilab.sh's refusal paths.
#
#   bash tests/test_deploy_montilab_guards.sh [path to deploy_montilab.sh]
#
# Run by hand, not in CI: it clones both repositories, so it needs network and
# takes about a minute. It is here because deploy_montilab.sh is the most
# safety-critical script in the staging workflow, and what matters about it is
# what it REFUSES to do.
#
# The property under test: when anything is wrong, it must abort BEFORE it
# touches a container, so staging is left running the old, working code rather
# than a half-deployed mixture of client and server.
set -uo pipefail

SCRIPT=${1:-$(cd "$(dirname "$0")/.." && pwd)/scripts/deploy_montilab.sh}
WORK=$(mktemp -d)
trap 'rm -rf "$WORK"' EXIT
FAILURES=0

expect_abort() {
  local label=$1 want=$2 out=$3 rc=$4
  if [ "$rc" -eq 0 ]; then
    echo "FAIL  $label (exited 0, expected an abort)"; FAILURES=$((FAILURES+1)); return
  fi
  if printf '%s' "$out" | grep -q "$want"; then
    echo "PASS  $label"
  else
    echo "FAIL  $label (no '$want' in output)"
    printf '%s\n' "$out" | tail -5
    FAILURES=$((FAILURES+1))
  fi
  # Nothing may have run docker.
  if printf '%s' "$out" | grep -qE "^===== [3-7]\."; then
    echo "FAIL  $label reached a mutating step"; FAILURES=$((FAILURES+1))
  fi
}

echo "--- setting up scratch clones ---"
git clone -q --depth 50 https://github.com/montilab/SigRepo_Server.git "$WORK/server"
git clone -q --depth 50 https://github.com/montilab/SigRepo.git "$WORK/client"
for d in "$WORK/server" "$WORK/client"; do
  git -C "$d" fetch -q --depth 50 origin "+refs/heads/dev:refs/remotes/origin/dev"
  git -C "$d" checkout -q -B dev origin/dev
  git -C "$d" config user.email t@t.test
  git -C "$d" config user.name test
done

run() { SERVER_DIR=$WORK/server CLIENT_DIR=$WORK/client LOG=$WORK/deploy.log bash "$SCRIPT" --dry-run 2>&1; }

# 1. No .env at all.
OUT=$(run); RC=$?
expect_abort "missing .env is refused" "does not set SIGREPO_IMAGE_TAG" "$OUT" "$RC"

echo "SIGREPO_IMAGE_TAG=latest" > "$WORK/server/.env"

# 2. A local commit origin/dev does not have.
git -C "$WORK/server" commit -q --allow-empty -m "local commit origin/dev lacks"
OUT=$(run); RC=$?
expect_abort "diverged checkout is refused" "not fast-forwardable" "$OUT" "$RC"
git -C "$WORK/server" reset -q --hard origin/dev

# 3. Uncommitted tracked changes.
echo "scribble" >> "$WORK/client/DESCRIPTION"
OUT=$(run); RC=$?
expect_abort "uncommitted changes are refused" "refusing to merge over them" "$OUT" "$RC"
git -C "$WORK/client" checkout -q -- DESCRIPTION

# 4. An image tag that was never published.
echo "SIGREPO_IMAGE_TAG=no-such-tag-xyz" > "$WORK/server/.env"
OUT=$(run); RC=$?
expect_abort "unpublished image tag is refused" "is not published" "$OUT" "$RC"

# 5. A clean pair with a real tag: dry run reaches the stop point and exits 0.
echo "SIGREPO_IMAGE_TAG=latest" > "$WORK/server/.env"
OUT=$(run); RC=$?
if [ "$RC" -eq 0 ] && printf '%s' "$OUT" | grep -q "DRY RUN: stopping before any change"; then
  echo "PASS  clean pair dry-runs to the stop point"
else
  echo "FAIL  clean pair dry run (rc=$RC)"; printf '%s\n' "$OUT" | tail -8; FAILURES=$((FAILURES+1))
fi
if printf '%s' "$OUT" | grep -qE "^===== [3-7]\."; then
  echo "FAIL  dry run reached a mutating step"; FAILURES=$((FAILURES+1))
else
  echo "PASS  dry run touched no container"
fi

echo "=== $FAILURES failure(s) ==="
[ "$FAILURES" -eq 0 ]
