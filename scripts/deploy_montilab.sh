#!/usr/bin/env bash
# Deploy the dev line to the montilab.bu.edu staging instance.
#
#   sudo bash scripts/deploy_montilab.sh [--dry-run]
#
# Run as root because camv is deliberately not in the docker group: montilab is
# a shared, BU-managed host and the lab keeps no standing docker access on it.
# So a deploy is one sudo line, and everything else is in here and in the log.
#
# What this never does:
#   - restart the Docker daemon (it is shared; that would bounce every other
#     lab member's containers)
#   - touch sigrepo-local-mysql (the seeded 287-signature replica is the
#     expensive thing here and a code deploy has no business restarting it)
#   - publish a port anywhere but 127.0.0.1 (checked, not assumed)
#
# Paths are overridable so this is not a single-machine artifact.

set -uo pipefail

SERVER_DIR=${SERVER_DIR:-/home/camv/SigRepo_Server}
CLIENT_DIR=${CLIENT_DIR:-/home/camv/SigRepo}
LOG=${LOG:-/home/camv/montilab_deploy.log}
COMPOSE_FILE=${COMPOSE_FILE:-docker-compose-local.yml}
API_URL=${API_URL:-http://127.0.0.1:8020}
SHINY_TUNNEL_HINT=${SHINY_TUNNEL_HINT:-http://127.0.0.1:9051}
SERVICES="sigrepo-api sigrepo-shiny sigrepo-mcp"

DRY_RUN=0
[ "${1:-}" = "--dry-run" ] && DRY_RUN=1

exec > >(tee "$LOG") 2>&1
step() { echo; echo "===== $* ====="; date '+%F %T'; }
die()  { echo "ABORT: $*"; exit 1; }

step "0. Preflight"
[ -d "$SERVER_DIR" ] || die "no server checkout at $SERVER_DIR"
[ -d "$CLIENT_DIR" ] || die "no client checkout at $CLIENT_DIR"

# Without this the host silently deploys the production image and every result
# after it is misleading.
grep -q '^SIGREPO_IMAGE_TAG=' "$SERVER_DIR/.env" 2>/dev/null \
  || die "$SERVER_DIR/.env does not set SIGREPO_IMAGE_TAG; this host would pull :latest"
TAG=$(grep '^SIGREPO_IMAGE_TAG=' "$SERVER_DIR/.env" | head -1 | cut -d= -f2- | tr -d '"'"'"' \t\r')
echo "image tag: $TAG"

# Check BOTH fast forwards are possible before performing EITHER. Updating the
# server and then failing on the client leaves staging running a mismatched
# pair, which is worse than not deploying at all.
step "1. Check both checkouts can fast forward to origin/dev"
for d in "$SERVER_DIR" "$CLIENT_DIR"; do
  git -C "$d" fetch origin dev || die "fetch failed in $d"
  git -C "$d" rev-parse --verify -q origin/dev > /dev/null \
    || die "$d has no origin/dev ref after fetching; is the remote right?"
  behind=$(git -C "$d" rev-list --count HEAD..origin/dev) \
    || die "could not count commits behind origin/dev in $d"
  ahead=$(git -C "$d" rev-list --count origin/dev..HEAD) \
    || die "could not count commits ahead of origin/dev in $d"
  echo "$d: behind origin/dev by $behind, ahead by $ahead"
  # An empty count means rev-list failed silently; treat that as unsafe rather
  # than printing "has  local commit(s)" and guessing.
  [ -n "$ahead" ] && [ -n "$behind" ] || die "could not compare $d against origin/dev"
  [ "$ahead" = "0" ] || die "$d has $ahead local commit(s) origin/dev lacks; not fast-forwardable"
  git -C "$d" diff --quiet || die "$d has uncommitted tracked changes; refusing to merge over them"
done

# Turn "manifest unknown" halfway through a deploy into a clear message now.
step "2. Check the image tag exists before touching anything"
if ! docker manifest inspect "montilab/sigrepo:${TAG}" > /dev/null 2>&1; then
  die "montilab/sigrepo:${TAG} is not published. Push a change to Dockerfile,
  install_r_packages.R or DESCRIPTION on dev, or run the SigRepo Docker Build
  workflow by hand (workflow_dispatch), then try again."
fi
echo "montilab/sigrepo:${TAG} exists"

if [ "$DRY_RUN" = "1" ]; then
  step "DRY RUN: stopping before any change"
  exit 0
fi

step "3. Fast forward both checkouts"
for d in "$SERVER_DIR" "$CLIENT_DIR"; do
  git -C "$d" merge --ff-only origin/dev || die "fast forward failed in $d"
  echo "$d now at $(git -C "$d" log -1 --format='%h %s')"
done

step "4. Pull the image"
cd "$SERVER_DIR" || die "cannot enter $SERVER_DIR"
# shellcheck disable=SC2086
docker compose -f "$COMPOSE_FILE" pull $SERVICES || die "image pull failed"

step "5. Recreate the code-running services only"
# --no-deps and naming the services keeps MySQL out of it.
# shellcheck disable=SC2086
docker compose -f "$COMPOSE_FILE" up -d --no-deps $SERVICES || die "compose up failed"

step "6. Wait for the API"
up=0
for i in $(seq 1 60); do
  code=$(curl -s -o /dev/null -w '%{http_code}' --max-time 5 "${API_URL}/__docs__/")
  if [ "$code" = "200" ]; then echo "API up after $i attempt(s)"; up=1; break; fi
  sleep 5
done
[ "$up" = "1" ] || die "API never answered at ${API_URL}"

step "7. Smoke test"
KEY=$(docker exec sigrepo-local-mysql sh -c \
  'mysql -uroot -p"$MYSQL_ROOT_PASSWORD" -N -e "select api_key from sigrepo.users where user_name=\"devadmin\";"' \
  2>/dev/null | tr -d '[:space:]')
[ -n "$KEY" ] || die "could not read the devadmin api key from the database"
bash "$SERVER_DIR/scripts/smoke_test.sh" \
  --host 127.0.0.1 --port "${API_URL##*:}" --api-key "$KEY" --containment
SMOKE=$?

step "DONE"
echo "deployed: server $(git -C "$SERVER_DIR" log -1 --format='%h'), client $(git -C "$CLIENT_DIR" log -1 --format='%h')"
if [ "$SMOKE" -eq 0 ]; then
  echo "smoke test PASSED"
else
  echo "smoke test FAILED: staging is running the new code but is not healthy"
fi
echo
echo "The Shiny sign-in check does not run here: montilab has no browser."
echo "From a laptop, with the tunnel up:"
echo "  Rscript scripts/smoke_test_ui.R ${SHINY_TUNNEL_HINT} devadmin devadmin"
echo "Full log: $LOG"
exit "$SMOKE"
