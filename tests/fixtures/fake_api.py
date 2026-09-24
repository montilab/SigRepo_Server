#!/usr/bin/env python3
"""A deliberately broken SigRepo API, for testing smoke_test.sh's failure paths.

Usage: python3 fake_api.py <port> [scenario]

Scenarios:

  empty (default)
      A valid, well-formed, completely empty repository. /__docs__/ answers 200
      so the smoke test gets past its first check, and /signatures/search
      answers with count 0 and no signatures.

      This is the interesting one, because a stack whose database failed to
      attach still serves HTTP 200 on every route. A smoke test that only knows
      how to report "the API did not answer" sails through it and declares the
      instance healthy.

  broken-compare
      A populated repository whose compare route fails the way a real one does
      when the image predates ComplexHeatmap, circlize, cba and fgsea: the
      plumber route's tryCatch returns json_error(), which serialises to
      [{"MESSAGES": "Signature comparison failed: ..."}] with status 500.

      That body is a NON-EMPTY JSON list. Any check that tests the response for
      truthiness rather than for success passes on it, which turns the exact
      failure the check exists to catch into a green line.
"""
import json
import sys
from http.server import BaseHTTPRequestHandler, HTTPServer

SCENARIO = "empty"

SIGNATURES = [
    {"signature_hashkey": "aaaa1111", "signature_name": "Fake_Hs_One", "organism": "Homo sapiens"},
    {"signature_hashkey": "bbbb2222", "signature_name": "Fake_Hs_Two", "organism": "Homo sapiens"},
]


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
            if SCENARIO == "broken-compare":
                self._send(200, {"count": 2, "limit": 20, "offset": 0, "signatures": SIGNATURES})
            else:
                self._send(200, {"count": 0, "limit": 20, "offset": 0, "signatures": []})
        else:
            self._send(404, {"error": "not found"})

    def do_POST(self):
        if SCENARIO == "broken-compare":
            if self.path.startswith("/signatures/compare"):
                # Exactly what api/lib/common.R's json_error() produces.
                self._send(500, [{"MESSAGES": "Signature comparison failed: there is no package called 'ComplexHeatmap'"}])
            elif self.path.startswith("/annotate/genesets"):
                self._send(200, {"n_genesets": 50, "source": "cache"})
            else:
                self._send(200, {"geneset_source": "cache", "signatures": []})
        else:
            self._send(200, {"n_genesets": 0, "source": "none"})

    def log_message(self, *_):
        pass


if __name__ == "__main__":
    port = int(sys.argv[1])
    if len(sys.argv) > 2:
        SCENARIO = sys.argv[2]
    HTTPServer(("127.0.0.1", port), Handler).serve_forever()
