#!/usr/bin/env bash
# API level verification of a SigRepo stack.
#
#   scripts/smoke_test.sh --host 127.0.0.1 --port 8020 --api-key KEY [--containment]
#
# No R, no browser and (by default) no docker, so this runs on montilab.bu.edu,
# which has no browser installed, as well as from a laptop through an SSH
# tunnel. The Shiny sign-in check lives in scripts/smoke_test_ui.R for that
# reason.
#
# What it is for: the things that break silently and at a distance. Gene sets
# stop resolving because the source bind mount masks the cache baked into the
# image; Compare stops working because the image predates the ComplexHeatmap,
# circlize, cba and fgsea addition. Neither shows up as a failed container or a
# non-200 response.
#
# Exits 0 only if every check passed.

set -uo pipefail

HOST=127.0.0.1
PORT=8020
API_KEY=""
CONTAINMENT=0
TIMEOUT=120

while [ $# -gt 0 ]; do
  case "$1" in
    --host)        HOST=$2; shift 2 ;;
    --port)        PORT=$2; shift 2 ;;
    --api-key)     API_KEY=$2; shift 2 ;;
    --timeout)     TIMEOUT=$2; shift 2 ;;
    --containment) CONTAINMENT=1; shift ;;
    -h|--help)     sed -n '2,20p' "$0"; exit 0 ;;
    *) echo "unknown argument: $1" >&2; exit 2 ;;
  esac
done

BASE="http://${HOST}:${PORT}"
FAILED=0

pass() { printf 'PASS  %s\n' "$1"; }
fail() { printf 'FAIL  %s: %s\n' "$1" "$2"; FAILED=$((FAILED + 1)); }

# Read one field out of a JSON document on stdin. Prints nothing and returns
# non-zero when the document is not JSON or the field is absent, so a caller can
# tell "missing" apart from "zero" -- the difference between a broken API and an
# empty database.
json_field() {
  python3 -c '
import json, sys
try:
    doc = json.load(sys.stdin)
except Exception:
    sys.exit(1)
cur = doc
for key in sys.argv[1].split("."):
    if isinstance(cur, dict) and key in cur:
        cur = cur[key]
    else:
        sys.exit(1)
print(cur)
' "$1"
}

# Every request is bounded. A staging instance that hangs has to fail, because a
# stalled deploy looks exactly like a slow one.
get()  { curl -s --max-time "$TIMEOUT" "$1"; }
post() { curl -s --max-time "$TIMEOUT" -X POST -H 'Content-Type: application/json' -d "$2" "$1"; }

echo "=== SigRepo smoke test against ${BASE} ==="

# 1. Is the API alive at all.
code=$(curl -s -o /dev/null -w '%{http_code}' --max-time "$TIMEOUT" "${BASE}/__docs__/")
if [ "$code" = "200" ]; then
  pass "api docs reachable"
else
  fail "api docs reachable" "HTTP $code"
fi

# 2. Is the database attached and populated. An empty repository is a failure,
#    not a pass: a stack whose database failed to attach still answers 200 on
#    every route, and that is the case worth catching.
search=$(get "${BASE}/signatures/search?api_key=${API_KEY}")
count=$(printf '%s' "$search" | json_field count)
if [ -z "$count" ]; then
  fail "signature search" "no count field in the response: $(printf '%s' "$search" | head -c 120)"
elif ! printf '%s' "$count" | grep -qE '^[0-9]+$'; then
  fail "signature search" "count is not a number: $count"
elif [ "$count" -gt 0 ]; then
  pass "signature search ($count signatures)"
else
  fail "signature search" "count is 0, the repository is empty"
fi

# 3. Do gene sets resolve. /annotate/* reads the on-disk cache, NOT the
#    geneset_resources table, and the compose bind mount of the source hides the
#    cache baked into the image. data/msigdb_genesets is gitignored, so a fresh
#    clone has nothing to resolve against and every enrichment fails.
gs=$(post "${BASE}/annotate/genesets" \
  "{\"api_key\":\"${API_KEY}\",\"species\":\"Homo sapiens\",\"collection\":\"H\"}")
n_gs=$(printf '%s' "$gs" | json_field n_genesets)
src=$(printf '%s' "$gs" | json_field source)
if [ -n "$n_gs" ] && printf '%s' "$n_gs" | grep -qE '^[0-9]+$' && [ "$n_gs" -gt 0 ]; then
  pass "gene sets resolve ($n_gs Hallmark sets, source=$src)"
else
  fail "gene sets resolve" "$(printf '%s' "$gs" | head -c 200)"
fi

# The remaining checks need real signatures to work on.
read -r first second <<EOF
$(printf '%s' "$search" | python3 -c '
import json, sys
try:
    sigs = json.load(sys.stdin).get("signatures", [])
except Exception:
    sigs = []
human = [s for s in sigs if s.get("organism") == "Homo sapiens" and s.get("signature_hashkey")]
print(" ".join(s["signature_hashkey"] for s in human[:2]))
')
EOF

if [ -z "${first:-}" ]; then
  fail "enrichment run"   "no human signature available to test with"
  fail "signature compare" "no human signature available to test with"
else
  # 4. Does hypeR work end to end.
  enr=$(post "${BASE}/annotate/run" \
    "{\"api_key\":\"${API_KEY}\",\"signature_hashkeys\":[\"${first}\"],\"test\":\"hypergeometric\",\"species\":\"Homo sapiens\",\"collection\":\"H\",\"fdr\":0.25}")
  if printf '%s' "$enr" | json_field geneset_source > /dev/null; then
    pass "enrichment run"
  else
    fail "enrichment run" "$(printf '%s' "$enr" | head -c 200)"
  fi

  # 5. Does compare work. This is what ComplexHeatmap, circlize, cba and fgsea
  #    are in the image for; an older image fails here and nowhere else.
  if [ -z "${second:-}" ]; then
    fail "signature compare" "need two human signatures, found one"
  else
    cmp=$(post "${BASE}/signatures/compare" \
      "{\"api_key\":\"${API_KEY}\",\"signature_hashkeys\":[\"${first}\",\"${second}\"],\"method\":\"overlap\"}")
    if printf '%s' "$cmp" | python3 -c 'import json,sys; sys.exit(0 if json.load(sys.stdin) else 1)' 2>/dev/null; then
      pass "signature compare"
    else
      fail "signature compare" "$(printf '%s' "$cmp" | head -c 200)"
    fi
  fi
fi

# 6. Containment. Opt-in rather than silently skipped: this is only meaningful
#    where docker is usable, and a check that quietly does nothing is worse than
#    one that is absent.
if [ "$CONTAINMENT" -eq 1 ]; then
  published=$(docker ps --format '{{.Names}}	{{.Ports}}' 2>/dev/null | grep sigrepo-local)
  if [ -z "$published" ]; then
    fail "containment" "no sigrepo-local containers visible to docker ps"
  elif printf '%s' "$published" | grep -q '0\.0\.0\.0'; then
    fail "containment" "a sigrepo-local port is published on 0.0.0.0"
    printf '%s\n' "$published"
  else
    pass "containment (every published port on 127.0.0.1)"
  fi
fi

echo "=== $FAILED failure(s) ==="
[ "$FAILED" -eq 0 ]
