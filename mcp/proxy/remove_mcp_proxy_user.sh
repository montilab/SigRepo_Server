#!/bin/bash
# Remove one person's HTTP basic-auth credential for the sigrepo-mcp
# reverse proxy.
#
# Usage: mcp/proxy/remove_mcp_proxy_user.sh <username>

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HTPASSWD_FILE="${SCRIPT_DIR}/.htpasswd"

username="${1:-}"

if [[ -z "$username" ]]; then
  echo "Usage: $0 <username>" >&2
  exit 1
fi

if [[ ! -f "$HTPASSWD_FILE" ]] || ! grep -q "^${username}:" "$HTPASSWD_FILE"; then
  echo "No proxy credential found for '$username'." >&2
  exit 1
fi

grep -v "^${username}:" "$HTPASSWD_FILE" > "${HTPASSWD_FILE}.tmp"
mv "${HTPASSWD_FILE}.tmp" "$HTPASSWD_FILE"

echo "Removed proxy credential for '$username'."
echo "Reload the proxy for it to take effect:"
echo "  docker exec sigrepo-mcp-proxy nginx -s reload"
