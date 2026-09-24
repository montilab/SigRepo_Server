#!/usr/bin/env python3
"""A deliberately empty SigRepo API, for testing smoke_test.sh's failure paths.

This serves a valid, well-formed, completely empty repository: /__docs__/
answers 200 so the smoke test gets past its first check, and
/signatures/search answers with count 0 and no signatures.

That combination is the interesting one. A smoke test that only knows how to
report "the API did not answer" will sail straight through this and declare the
stack healthy, which is exactly the failure worth catching: a staging instance
whose database failed to attach still serves HTTP 200 on every route.

Usage: python3 fake_api.py <port>
"""
import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer


class Handler(BaseHTTPRequestHandler):
    def _send(self, code, payload):
        body = json.dumps(payload).encode()
        self.send_response(code)
        self.send_header("Content-Type", "application/json")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        self.wfile.write(body)

    def do_GET(self):
        if self.path.startswith("/__docs__"):
            self._send(200, {"ok": True})
        elif self.path.startswith("/signatures/search"):
            self._send(200, {"count": 0, "limit": 20, "offset": 0, "signatures": []})
        else:
            self._send(404, {"error": "not found"})

    def do_POST(self):
        # An enrichment or gene set call against an empty repository: shaped
        # like a real response, carrying nothing.
        self._send(200, {"n_genesets": 0, "source": "none"})

    def log_message(self, *_):
        pass


if __name__ == "__main__":
    HTTPServer(("127.0.0.1", int(sys.argv[1])), Handler).serve_forever()
