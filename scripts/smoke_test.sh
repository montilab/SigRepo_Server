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
TIMEOUT=30
# Enrichment and compare are genuinely slow on montilab: 2 CPUs, ~3.4 GB free
# against other people's workloads, and a single R worker that serialises every
# request. A short budget there turns "slow" into a red deploy.
LONG_TIMEOUT=${LONG_TIMEOUT:-420}

while [ $# -gt 0 ]; do
  case "$1" in
    --host)        HOST=$2; shift 2 ;;
    --port)        PORT=$2; shift 2 ;;
    --api-key)     API_KEY=$2; shift 2 ;;
    --timeout)     TIMEOUT=$2; shift 2 ;;
    --long-timeout) LONG_TIMEOUT=$2; shift 2 ;;
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

# Every request is bounded twice: --connect-timeout so a filtered or black-holed
# host fails in seconds rather than burning the whole budget, and --max-time so a
# stalled instance fails at all. A deploy that hangs looks exactly like a slow
# one, and the difference has to be visible.
CONNECT_TIMEOUT=${CONNECT_TIMEOUT:-5}

get()  { curl -s --connect-timeout "$CONNECT_TIMEOUT" --max-time "$TIMEOUT" "$1"; }

# POST, writing the body to $BODY and printing the HTTP status. The status is
# the point: the API answers failures with json_error(), which serialises to
# [{"MESSAGES": "..."}] -- a NON-EMPTY JSON list. Anything that tests the body
# for truthiness rather than for success passes on a 500.
BODY=$(mktemp)
trap 'rm -f "$BODY"' EXIT
post_code() {
  local url=$1 data=$2 budget=${3:-$TIMEOUT}
  curl -s -o "$BODY" -w '%{http_code}' \
    --connect-timeout "$CONNECT_TIMEOUT" --max-time "$budget" \
    -X POST -H 'Content-Type: application/json' -d "$data" "$url"
}

# True when the body is an API error payload rather than a result.
is_api_error() {
  python3 -c '
import json, sys
try:
    doc = json.load(open(sys.argv[1]))
except Exception:
    sys.exit(0)          # unparseable is not a result either
if isinstance(doc, list) and doc and isinstance(doc[0], dict) and "MESSAGES" in doc[0]:
    sys.exit(0)
if isinstance(doc, dict) and "MESSAGES" in doc:
    sys.exit(0)
sys.exit(1)
' "$BODY"
}

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
gs_code=$(post_code "${BASE}/annotate/genesets" \
  "{\"api_key\":\"${API_KEY}\",\"species\":\"Homo sapiens\",\"collection\":\"H\"}")
gs=$(cat "$BODY")
n_gs=$(printf '%s' "$gs" | json_field n_genesets)
src=$(printf '%s' "$gs" | json_field source)
if [ "$gs_code" != "200" ]; then
  fail "gene sets resolve" "HTTP $gs_code: $(printf '%s' "$gs" | head -c 200)"
elif [ -n "$n_gs" ] && printf '%s' "$n_gs" | grep -qE '^[0-9]+$' && [ "$n_gs" -gt 0 ]; then
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
  enr_code=$(post_code "${BASE}/annotate/run" \
    "{\"api_key\":\"${API_KEY}\",\"signature_hashkeys\":[\"${first}\"],\"test\":\"hypergeometric\",\"species\":\"Homo sapiens\",\"collection\":\"H\",\"fdr\":0.25}" \
    "$LONG_TIMEOUT")
  if [ "$enr_code" != "200" ] || is_api_error; then
    fail "enrichment run" "HTTP $enr_code: $(head -c 200 "$BODY")"
  elif json_field geneset_source < "$BODY" > /dev/null; then
    pass "enrichment run"
  else
    fail "enrichment run" "no geneset_source in the response: $(head -c 200 "$BODY")"
  fi

  # 5. Does compare work. This is what ComplexHeatmap, circlize, cba and fgsea
  #    are in the image for; an older image fails here and nowhere else.
  if [ -z "${second:-}" ]; then
    fail "signature compare" "need two human signatures, found one"
  else
    # The whole reason this check exists is an image predating ComplexHeatmap,
    # circlize, cba and fgsea. That failure arrives as a 500 whose body is
    # [{"MESSAGES": "Signature comparison failed: there is no package called
    # 'ComplexHeatmap'"}] -- non-empty, and therefore truthy. Test the status
    # and the payload shape, never the body's truthiness.
    cmp_code=$(post_code "${BASE}/signatures/compare" \
      "{\"api_key\":\"${API_KEY}\",\"signature_hashkeys\":[\"${first}\",\"${second}\"],\"method\":\"overlap\"}" \
      "$LONG_TIMEOUT")
    if [ "$cmp_code" != "200" ] || is_api_error; then
      fail "signature compare" "HTTP $cmp_code: $(head -c 200 "$BODY")"
    elif [ -s "$BODY" ]; then
      pass "signature compare"
    else
      fail "signature compare" "empty response"
    fi
  fi
fi

# 6. Containment. Opt-in rather than silently skipped: this is only meaningful
#    where docker is usable, and a check that quietly does nothing is worse than
#    one that is absent.
if [ "$CONTAINMENT" -eq 1 ]; then
  # Match OUR container names exactly, not a "sigrepo" prefix.
  #
  # Both halves of this are load-bearing. Matching only sigrepo-local-* would
  # hide a docker-compose-vm.yml or docker-compose.yml stack (sigrepo-api,
  # sigrepo-mysql) published on 0.0.0.0 right beside a local one, which is the
  # exact thing being checked. But matching the bare prefix sweeps in containers
  # that are not ours: montilab runs sigrepo-activate-user and sigrepo-rstudio,
  # other lab services that have been internet-open since long before SigRepo
  # staging existed and are not ours to change. Failing on those made every
  # deploy report "not healthy" for a condition nobody can fix, and a check that
  # cries wolf on every run is one people learn to ignore.
  #
  # The names are enumerable and all three compose files agree on them.
  #
  # DOCKER_PS is overridable so the check is testable with synthetic output: the
  # formats that matter (a routable address, the IPv6 wildcard) are awkward to
  # produce on demand and must not go untested for that reason.
  OURS='^sigrepo(-local)?-(mysql|api|web|shiny|mcp|mcp-proxy)[[:space:]]'
  published=$(${DOCKER_PS:-docker ps --format '{{.Names}}	{{.Ports}}'} 2>/dev/null | grep -E "$OURS")
  if [ -z "$published" ]; then
    fail "containment" "no sigrepo containers visible to docker ps"
  else
    # Assert every publish is ON loopback, rather than looking for the single
    # string 0.0.0.0. A bind to a routable address (128.197.x.x:8020->3838/tcp)
    # or to the IPv6 wildcard ([::]:8020->) contains no 0.0.0.0 at all.
    nonloopback=$(printf '%s\n' "$published" \
      | grep -oE '[][0-9a-fA-F.:*]+:[0-9]+->' \
      | grep -vE '^127\.0\.0\.1:' || true)
    if [ -n "$nonloopback" ]; then
      fail "containment" "published off 127.0.0.1: $(printf '%s' "$nonloopback" | tr '\n' ' ')"
      printf '%s\n' "$published"
    else
      pass "containment (every published port on 127.0.0.1)"
    fi
  fi
fi

echo "=== $FAILED failure(s) ==="
[ "$FAILED" -eq 0 ]
