#!/bin/bash
# Add (or rotate) one person's HTTP basic-auth credential for the
# sigrepo-mcp reverse proxy. This is a credential distinct from that
# person's SigRepo api_key -- see mcp/proxy/.htpasswd.example for why they
# aren't the same value.
#
# Usage: mcp/proxy/add_mcp_proxy_user.sh <username>
#
# Prints the generated plaintext password once, to stdout, and nowhere
# else -- relay it to the person out-of-band. Nothing here stores it.

set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HTPASSWD_FILE="${SCRIPT_DIR}/.htpasswd"

username="${1:-}"

if [[ -z "$username" ]]; then
  echo "Usage: $0 <username>" >&2
  exit 1
fi

if [[ ! "$username" =~ ^[A-Za-z0-9._-]+$ ]]; then
  echo "Username must contain only letters, numbers, '.', '_', or '-'." >&2
  exit 1
fi

touch "$HTPASSWD_FILE"

if grep -q "^${username}:" "$HTPASSWD_FILE"; then
  read -r -p "'$username' already has a proxy credential. Rotate it now? [y/N] " confirm
  if [[ ! "$confirm" =~ ^[Yy]$ ]]; then
    echo "Aborted." >&2
    exit 1
  fi
  grep -v "^${username}:" "$HTPASSWD_FILE" > "${HTPASSWD_FILE}.tmp"
  mv "${HTPASSWD_FILE}.tmp" "$HTPASSWD_FILE"
fi

password="$(openssl rand -base64 24)"
hash="$(openssl passwd -apr1 "$password")"
printf '%s:%s\n' "$username" "$hash" >> "$HTPASSWD_FILE"

echo "Added proxy credential for '$username'."
echo
echo "Password (shown once -- not stored anywhere in plaintext):"
echo "  $password"
echo
echo "Relay this to them out-of-band, then reload the proxy for it to take effect:"
echo "  docker exec sigrepo-mcp-proxy nginx -s reload"
