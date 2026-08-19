#!/usr/bin/env bash
# Stands up a throwaway local SigRepo stack (MySQL -> API -> MCP server),
# bootstraps it, runs SigRepo/local_validation's harness against it, and
# tears everything down again -- success, failure, or interrupted halfway.
#
# Requires SigRepo and SigRepo_Server checked out as sibling directories.
# Run from anywhere; paths below are resolved relative to this script.
#
# Env vars (all optional, sensible defaults below):
#   MYSQL_PORT, MCP_PORT   -- host ports for the throwaway stack
#   KEEP_STACK=1           -- skip teardown, leave everything running for
#                             manual poking afterward (you're on your own
#                             for cleanup if you set this)
#
# api/run_sigrepo_api.R binds port 3838 unconditionally -- it is NOT
# configurable via an env var, unlike MCP_PORT. This script fails fast if
# 3838 is already in use rather than silently colliding with it.

set -uo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SIGREPO_SERVER_DIR="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"
SIGREPO_DIR="$(cd "${SIGREPO_SERVER_DIR}/../SigRepo" && pwd 2>/dev/null)"

if [[ -z "${SIGREPO_DIR}" || ! -d "${SIGREPO_DIR}/local_validation" ]]; then
  echo "[validate] Could not find a sibling SigRepo checkout with local_validation/ next to ${SIGREPO_SERVER_DIR}." >&2
  echo "[validate] This workflow needs SigRepo and SigRepo_Server checked out as siblings." >&2
  exit 1
fi

MYSQL_PORT="${MYSQL_PORT:-13380}"
API_PORT=3838
MCP_PORT="${MCP_PORT:-18030}"
DB_PW="validate_$(date +%s)"
ADMIN_KEY="validate_admin_key"
WORKDIR=$(mktemp -d)
CONTAINER_NAME="sigrepo-validate-$$"

echo "[validate] workdir: ${WORKDIR}"
echo "[validate] SigRepo_Server: ${SIGREPO_SERVER_DIR}"
echo "[validate] SigRepo:        ${SIGREPO_DIR}"

if lsof -nP -iTCP:"${API_PORT}" -sTCP:LISTEN >/dev/null 2>&1; then
  echo "[validate] Port ${API_PORT} is already in use, and api/run_sigrepo_api.R can't be told to use a different one." >&2
  echo "[validate] Find and stop whatever's on it first (lsof -nP -iTCP:${API_PORT} -sTCP:LISTEN), then re-run." >&2
  exit 1
fi

API_PID=""
MCP_PID=""

cleanup() {
  if [[ "${KEEP_STACK:-0}" == "1" ]]; then
    echo "[validate] KEEP_STACK=1 set -- leaving MySQL (${CONTAINER_NAME}), API (pid ${API_PID}), and MCP (pid ${MCP_PID}) running."
    echo "[validate] Clean up yourself later with: kill ${API_PID} ${MCP_PID}; docker rm -f ${CONTAINER_NAME}"
    return
  fi
  echo "[validate] tearing down..."
  [[ -n "${API_PID}" ]] && kill "${API_PID}" 2>/dev/null
  [[ -n "${MCP_PID}" ]] && kill "${MCP_PID}" 2>/dev/null
  sleep 1
  docker rm -f "${CONTAINER_NAME}" >/dev/null 2>&1
  rm -rf "${WORKDIR}"
}
trap cleanup EXIT

echo "[validate] [1/6] starting MySQL 8 on port ${MYSQL_PORT}"
docker run -d --name "${CONTAINER_NAME}" \
  -e MYSQL_ROOT_PASSWORD="${DB_PW}" -e MYSQL_DATABASE=sigrepo \
  -p "${MYSQL_PORT}:3306" \
  mysql:8.0 --default-authentication-plugin=mysql_native_password >/dev/null

for i in $(seq 1 40); do
  docker exec "${CONTAINER_NAME}" mysqladmin ping -uroot -p"${DB_PW}" --silent 2>/dev/null && break
  sleep 2
done
sleep 3

echo "[validate] [2/6] starting the Plumber API on port ${API_PORT}"
cd "${SIGREPO_SERVER_DIR}"
# --no-environ is load-bearing: R auto-loads a .Renviron file from the
# current directory at startup, which silently overrides every one of
# these env vars with SigRepo_Server's real deployment values (container-
# internal hosts, production IPs) if you don't suppress it.
#
# MSIGDB_CACHE_DIR here is just as load-bearing as --no-environ, in a
# different way: without it, /init_db's geneset-population step falls back
# to data/msigdb_genesets -- the checkout's own cache directory
# -- and overwrites it with this throwaway run's fetch. Caught this by
# actually running the script and checking `git status` afterward; the
# manifest files came back modified. Never skip that check when touching
# this script.
DB_NAME=sigrepo DB_LOCAL_HOST=127.0.0.1 DB_PORT="${MYSQL_PORT}" \
DB_USER=root DB_PASSWORD="${DB_PW}" \
ADMIN_KEY="${ADMIN_KEY}" \
DIFEXP_DIR="${WORKDIR}/difexp" \
SIGREPO_SERVER_DIR="${SIGREPO_SERVER_DIR}" \
SIGREPO_DIR="${SIGREPO_DIR}" \
MSIGDB_CACHE_DIR="${WORKDIR}/msigdb_cache" \
nohup Rscript --no-environ api/run_sigrepo_api.R > "${WORKDIR}/api.log" 2>&1 &
API_PID=$!

for i in $(seq 1 30); do
  curl -fsS "http://127.0.0.1:${API_PORT}/__docs__/" >/dev/null 2>&1 && break
  sleep 2
done
if ! curl -fsS "http://127.0.0.1:${API_PORT}/__docs__/" >/dev/null 2>&1; then
  echo "[validate] API failed to start -- log tail:" >&2
  tail -50 "${WORKDIR}/api.log" >&2
  exit 1
fi

echo "[validate] [3/6] bootstrapping the database via /init_db (schema + reference tables + curated MSigDB gene sets -- this now takes ~3-4 minutes, not the ~1-2 it used to, since geneset population runs as part of it)"
INIT_RESPONSE=$(curl -sS -X POST "http://127.0.0.1:${API_PORT}/init_db?admin_key=${ADMIN_KEY}" --max-time 400)
echo "[validate] /init_db response: ${INIT_RESPONSE}"
if [[ "${INIT_RESPONSE}" != *"Finish initialized"* ]]; then
  echo "[validate] /init_db did not report success -- check the response above and ${WORKDIR}/api.log" >&2
  exit 1
fi

echo "[validate] [4/6] starting the MCP server on port ${MCP_PORT}"
# API_LOCAL_HOST/API_LOCAL_PORT matter here too: SigRepo::runHypeR() (used
# by the run_enrichment MCP tool) round-trips through the REST API for the
# difexp table on every call. Without this, the MCP server's connection
# handler falls back to newConnHandler()'s own default, which points at
# production -- silently, since a rejected auth call still "succeeds" as
# an HTTP round-trip, it just returns a confusing 404 from run_enrichment.
DB_NAME=sigrepo DB_LOCAL_HOST=127.0.0.1 DB_PORT="${MYSQL_PORT}" \
DB_USER=root DB_PASSWORD="${DB_PW}" \
API_LOCAL_HOST="http://127.0.0.1" API_LOCAL_PORT="${API_PORT}" \
SIGREPO_SERVER_DIR="${SIGREPO_SERVER_DIR}" \
SIGREPO_DIR="${SIGREPO_DIR}" \
MCP_PORT="${MCP_PORT}" \
MSIGDB_CACHE_DIR="${WORKDIR}/msigdb_cache" \
nohup Rscript --no-environ mcp/run_sigrepo_mcp.R > "${WORKDIR}/mcp.log" 2>&1 &
MCP_PID=$!

for i in $(seq 1 30); do
  curl -fsS -X POST "http://127.0.0.1:${MCP_PORT}" -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' >/dev/null 2>&1 && break
  sleep 2
done
if ! curl -fsS -X POST "http://127.0.0.1:${MCP_PORT}" -H "Content-Type: application/json" \
    -d '{"jsonrpc":"2.0","id":1,"method":"tools/list","params":{}}' >/dev/null 2>&1; then
  echo "[validate] MCP server failed to start -- log tail:" >&2
  tail -60 "${WORKDIR}/mcp.log" >&2
  exit 1
fi

echo "[validate] [5/6] preloading the LLFS_Aging_Gene_2023 example signature (03_r_client_read.R expects to find it)"
cd "${SIGREPO_DIR}"
Rscript --no-environ -e "
pkgload::load_all('.', quiet = TRUE, export_all = FALSE, helpers = FALSE)
conn_handler <- SigRepo::newConnHandler(
  dbname = 'sigrepo', host = '127.0.0.1', port = ${MYSQL_PORT},
  user = 'root', password = '${DB_PW}',
  api_host = 'http://127.0.0.1', api_port = ${API_PORT}
)
data(LLFS_Aging_Gene_2023, package = 'SigRepo')
SigRepo::addSignature(conn_handler = conn_handler, omic_signature = LLFS_Aging_Gene_2023, visibility = TRUE, verbose = FALSE)
" > "${WORKDIR}/preload.log" 2>&1
if ! grep -q "Success" "${WORKDIR}/preload.log"; then
  echo "[validate] preloading the example signature didn't report success -- continuing anyway, but r_client_read.R may fail:" >&2
  tail -20 "${WORKDIR}/preload.log" >&2
fi

cat > "${WORKDIR}/local_validation.env" <<EOF
SIGREPO_LOCAL_DB_NAME=sigrepo
SIGREPO_LOCAL_DB_HOST=127.0.0.1
SIGREPO_LOCAL_DB_PORT=${MYSQL_PORT}

SIGREPO_LOCAL_API_HOST=http://127.0.0.1
SIGREPO_LOCAL_API_PORT=${API_PORT}

SIGREPO_LOCAL_MCP_HOST=http://127.0.0.1
SIGREPO_LOCAL_MCP_PORT=${MCP_PORT}

SIGREPO_LOCAL_DB_ADMIN_USER=root
SIGREPO_LOCAL_DB_ADMIN_PASSWORD=${DB_PW}
SIGREPO_LOCAL_READ_USER=root
SIGREPO_LOCAL_READ_PASSWORD=${DB_PW}
SIGREPO_LOCAL_WRITE_USER=root
SIGREPO_LOCAL_WRITE_PASSWORD=${DB_PW}

SIGREPO_LOCAL_EXPECT_GENESETS=1
SIGREPO_LOCAL_EXPECT_METABOLITE_REFERENCE=0
EOF

echo "[validate] [6/6] running SigRepo/local_validation's harness"
cd "${SIGREPO_DIR}"
SIGREPO_LOCAL_ENV_FILE="${WORKDIR}/local_validation.env" ./local_validation/run_local_validation.sh
HARNESS_EXIT=$?

echo
echo "[validate] ================================================================"
if [[ ${HARNESS_EXIT} -eq 0 ]]; then
  echo "[validate] Harness exited 0 (no failures)."
else
  echo "[validate] Harness reported failures (exit ${HARNESS_EXIT}) -- see [fail] lines above."
  echo "[validate] Known/expected: signature_crud and collection_crud both currently fail with"
  echo "[validate] \"the condition has length > 1\" -- a documented, external OmicSignature package"
  echo "[validate] bug (see SigRepo/local_validation/SETUP_GUIDE.md), not something this stack broke."
  echo "[validate] Anything else failing is new and worth investigating before shipping."
fi
echo "[validate] ================================================================"

exit ${HARNESS_EXIT}
