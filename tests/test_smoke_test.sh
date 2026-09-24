#!/usr/bin/env bash
# Exercise smoke_test.sh against the failures it exists to catch.
#
#   bash tests/test_smoke_test.sh [path to smoke_test.sh]
#
# Run by hand. Needs python3 for the fixture; needs no docker and no stack.
#
# The cases here are the ones where a smoke test can be WRONG in the dangerous
# direction: reporting green on a broken instance. A smoke test that can do that
# is worse than no smoke test, because it converts an outage into a green line.
set -uo pipefail

SCRIPT=${1:-$(cd "$(dirname "$0")/.." && pwd)/scripts/smoke_test.sh}
FIXTURE=$(cd "$(dirname "$0")" && pwd)/fixtures/fake_api.py
FAILURES=0

ok()  { echo "PASS  $1"; }
bad() { echo "FAIL  $1"; FAILURES=$((FAILURES+1)); }

start_fixture() {
  python3 "$FIXTURE" "$1" "$2" &
  FIXTURE_PID=$!
  for _ in $(seq 1 20); do
    curl -s -o /dev/null --max-time 1 "http://127.0.0.1:$1/__docs__/" && return 0
    sleep 0.5
  done
  return 1
}
stop_fixture() { kill "$FIXTURE_PID" 2>/dev/null; wait "$FIXTURE_PID" 2>/dev/null; }

echo "--- an API whose compare route is broken ---"
#
# The point of the compare check is an image predating ComplexHeatmap, circlize,
# cba and fgsea. That failure arrives as HTTP 500 with the body
# [{"MESSAGES": "Signature comparison failed: ..."}], which is a NON-EMPTY JSON
# list. A check testing the body for truthiness passes on it.

if start_fixture 18094 broken-compare; then
  OUT=$(bash "$SCRIPT" --host 127.0.0.1 --port 18094 --api-key none --timeout 5 --long-timeout 5 2>&1)
  RC=$?
  stop_fixture
  if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q "FAIL  signature compare"; then
    ok "a 500 from compare is a FAIL, not a truthy pass"
  else
    bad "a broken compare route was reported as healthy (rc=$RC)"
    printf '%s\n' "$OUT"
  fi
  if printf '%s' "$OUT" | grep -q "PASS  signature search"; then
    ok "the other checks still pass, so the compare failure is isolated"
  else
    bad "the broken-compare fixture broke an unrelated check"
  fi
else
  bad "could not start the broken-compare fixture"
fi

echo "--- an API serving a valid but empty repository ---"

if start_fixture 18093 empty; then
  OUT=$(bash "$SCRIPT" --host 127.0.0.1 --port 18093 --api-key none --timeout 5 --long-timeout 5 2>&1)
  RC=$?
  stop_fixture
  if [ "$RC" -ne 0 ] && printf '%s' "$OUT" | grep -q "the repository is empty"; then
    ok "an empty repository is a clean FAIL"
  else
    bad "an empty repository was not caught (rc=$RC)"
  fi
  if printf '%s' "$OUT" | grep -qiE "traceback|File \""; then
    bad "a Python traceback reached the output"
  else
    ok "no traceback in the output"
  fi
else
  bad "could not start the empty fixture"
fi

echo "--- a host that does not answer ---"

START=$(date +%s)
OUT=$(bash "$SCRIPT" --host 127.0.0.1 --port 9 --api-key none 2>&1); RC=$?
ELAPSED=$(( $(date +%s) - START ))
if [ "$RC" -ne 0 ]; then ok "a dead port fails"; else bad "a dead port did not fail"; fi
# This must hold at the DEFAULT timeouts, not only when a short one is passed by
# hand: deploy_montilab.sh calls the script with no --timeout at all.
if [ "$ELAPSED" -le 30 ]; then
  ok "a dead port fails fast at default timeouts (${ELAPSED}s)"
else
  bad "a dead port took ${ELAPSED}s at default timeouts"
fi

echo "--- containment, against every binding format ---"
#
# The shared BU host is the highest-stakes constraint in the design, and the
# dangerous formats are awkward to produce on demand. DOCKER_PS makes the check
# testable rather than leaving them untested.

# Feed the listing through a real FILE rather than an inline printf. An inline
# command string mangles the tabs and newlines and word-splits on the ports,
# which makes every case pass or fail for reasons unrelated to what it claims to
# test -- the first version of this harness did exactly that.
FAKE_PS=$(mktemp)
containment_case() {
  local label=$1 want=$2 fake=$3
  printf '%b\n' "$fake" > "$FAKE_PS"
  OUT=$(DOCKER_PS="cat $FAKE_PS" bash "$SCRIPT" \
          --host 127.0.0.1 --port 9 --api-key none --containment 2>&1)
  if printf '%s' "$OUT" | grep -q "$want"; then ok "$label"; else
    bad "$label"; printf '%s\n' "$OUT" | grep -i containment
  fi
}

containment_case "0.0.0.0 wildcard is caught"      "FAIL  containment" 'sigrepo-api\t0.0.0.0:8020->3838/tcp'
containment_case "routable address is caught"      "FAIL  containment" 'sigrepo-api\t128.197.229.35:8020->3838/tcp'
containment_case "IPv6 wildcard is caught"         "FAIL  containment" 'sigrepo-api\t[::]:8020->3838/tcp'
containment_case "a vm stack alongside is caught"  "FAIL  containment" 'sigrepo-local-api\t127.0.0.1:8020->3838/tcp\nsigrepo-mysql\t0.0.0.0:3306->3306/tcp'
containment_case "all-loopback passes"             "PASS  containment" 'sigrepo-local-api\t127.0.0.1:8020->3838/tcp\nsigrepo-local-shiny\t127.0.0.1:8051->3838/tcp'
containment_case "no containers is a failure"      "FAIL  containment" ""

# The real montilab listing, from the first staging deploy on 2026-09-24.
# sigrepo-activate-user and sigrepo-rstudio are OTHER lab services on that
# shared host. They merely share the name prefix, they have been internet-open
# since long before SigRepo staging existed, and they are not ours to change.
# Failing on them makes every deploy report "not healthy" for a condition nobody
# can fix, and a check that cries wolf every run is one people learn to ignore.
MONTILAB_REAL='sigrepo-local-shiny\t8021/tcp, 127.0.0.1:8051->3838/tcp\nsigrepo-local-mcp\t3838/tcp, 8021/tcp\nsigrepo-local-api\t8021/tcp, 127.0.0.1:8020->3838/tcp\nsigrepo-local-web\t127.0.0.1:8050->80/tcp\nsigrepo-activate-user\t0.0.0.0:8080->3838/tcp, [::]:8080->3838/tcp\nsigrepo-local-mcp-proxy\t80/tcp, 127.0.0.1:8021->8021/tcp\nsigrepo-local-mysql\t127.0.0.1:3306->3306/tcp, 33060/tcp\nsigrepo-rstudio\t0.0.0.0:7878->8787/tcp, [::]:7878->8787/tcp'
containment_case "other lab containers sharing the prefix are ignored" "PASS  containment" "$MONTILAB_REAL"

# But a vm-compose stack IS ours, and an exposed one beside a local stack is the
# case the prefix match was broadened for in the first place.
containment_case "an exposed vm-compose stack of ours is still caught" "FAIL  containment" 'sigrepo-local-api\t127.0.0.1:8020->3838/tcp\nsigrepo-api\t0.0.0.0:8020->3838/tcp'
containment_case "an exposed production mysql of ours is still caught" "FAIL  containment" 'sigrepo-local-api\t127.0.0.1:8020->3838/tcp\nsigrepo-mysql\t0.0.0.0:3306->3306/tcp'

# A container anyone could start on a shared host must not be able to fail our
# deploys, nor to hide behind the allow-list.
containment_case "an unrelated sigrepo-* container cannot fail the deploy" "PASS  containment" 'sigrepo-local-api\t127.0.0.1:8020->3838/tcp\nsigrepo-someone-elses-app\t0.0.0.0:9999->80/tcp'

echo "=== $FAILURES failure(s) ==="
[ "$FAILURES" -eq 0 ]
