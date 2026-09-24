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

# Restore a publishable tag: the previous case deliberately left an unpublished
# one, and every case below needs to get PAST the tag check to test its own guard.
echo "SIGREPO_IMAGE_TAG=latest" > "$WORK/server/.env"

# 5. STAGED but uncommitted changes. `git diff --quiet` compares the working
#    tree to the INDEX, so `git add` with no commit sails past it -- and then
#    `git merge --ff-only` fails. Because the merge loop does the server first
#    and the client second, that leaves staging running a NEW server against an
#    OLD client: exactly the half-deployed state this script exists to prevent.
echo "staged but not committed" >> "$WORK/client/DESCRIPTION"
git -C "$WORK/client" add DESCRIPTION
OUT=$(run); RC=$?
expect_abort "staged-but-uncommitted changes are refused" "refusing to merge over them" "$OUT" "$RC"
git -C "$WORK/client" reset -q --hard origin/dev

# 6a. A harmless untracked file must NOT block a deploy. montilab legitimately
#     carries helper scripts that are not in the repo, and a guard that refuses
#     every deploy over one is a guard people route around.
echo "helper" > "$WORK/client/run_something_local.sh"
OUT=$(run); RC=$?
if [ "$RC" -eq 0 ]; then
  echo "PASS  a harmless untracked file does not block a deploy"
else
  echo "FAIL  a harmless untracked file blocked the deploy"; FAILURES=$((FAILURES+1))
  printf '%s\n' "$OUT" | tail -3
fi
rm -f "$WORK/client/run_something_local.sh"

# 6b. An untracked file that origin/dev is ABOUT TO ADD does break --ff-only,
#     and must be refused.
git -C "$WORK/client" reset -q --hard origin/dev~5
INCOMING=$(git -C "$WORK/client" diff --name-only --diff-filter=A HEAD origin/dev | head -1)
if [ -n "$INCOMING" ]; then
  mkdir -p "$(dirname "$WORK/client/$INCOMING")"
  echo "in the way" > "$WORK/client/$INCOMING"
  OUT=$(run); RC=$?
  expect_abort "an untracked file blocking an incoming add is refused" "in the way of origin/dev" "$OUT" "$RC"
  rm -f "$WORK/client/$INCOMING"
else
  echo "SKIP  no incoming file additions in the last 5 commits to collide with"
fi
git -C "$WORK/client" reset -q --hard origin/dev

# 7. A checkout sitting on the wrong branch. Every comparison here is against
#    origin/dev, and `git merge --ff-only origin/dev` fast forwards whatever
#    HEAD points at -- so on a checkout still on master it would move master to
#    dev's tip, which is the direct-to-master push that branch protection and
#    the hotfix rule exist to prevent.
git -C "$WORK/server" checkout -q -B master origin/dev
OUT=$(run); RC=$?
expect_abort "a checkout not on dev is refused" "not on the dev branch" "$OUT" "$RC"
git -C "$WORK/server" checkout -q -B dev origin/dev

# 8. An unrecognised argument must not silently become a real deploy.
OUT=$(SERVER_DIR=$WORK/server CLIENT_DIR=$WORK/client LOG=$WORK/deploy.log bash "$SCRIPT" --dryrun 2>&1); RC=$?
expect_abort "a mistyped flag is refused, not treated as a real run" "unknown argument" "$OUT" "$RC"

# 9. A clean pair with a real tag: dry run reaches the stop point and exits 0.
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
