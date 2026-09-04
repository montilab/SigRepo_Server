#!/usr/bin/env python3
"""Does an enrichment block everyone else?

Fires N back-to-back /annotate/run requests and, at the same time, polls
GET /signatures/search every 100 ms, recording how long each browse request
took. If enrichment runs inline in the API process, every browse request that
lands during the window waits for the enrichment in front of it; if it runs
in a worker, browse latency during the window matches idle latency.

Run against a LOCAL stack only. Required environment:
  SIGREPO_BASE            e.g. http://localhost:8050/api
  SIGREPO_API_KEY         a key for a throwaway local user -- never a real one
  SIGREPO_SIGNATURE       a signature_hashkey that has stored features
Optional: SIGREPO_BENCH_RUNS (default 10), SIGREPO_BENCH_COLLECTION (default H)

Reference numbers, local Docker stack, Hallmark, one CI fixture signature:
  synchronous (before)      idle 11 ms   during enrichment median 376 ms, 10 browse reqs served
  future_promise, I(1)      idle 13 ms   during enrichment median  15 ms, 109 browse reqs served
"""
import json, os, statistics, sys, threading, time, urllib.request

BASE = os.environ.get("SIGREPO_BASE")
KEY  = os.environ.get("SIGREPO_API_KEY")
HK   = os.environ.get("SIGREPO_SIGNATURE")
N    = int(os.environ.get("SIGREPO_BENCH_RUNS", "10"))
COLL = os.environ.get("SIGREPO_BENCH_COLLECTION", "H")
if not (BASE and KEY and HK):
    sys.exit("set SIGREPO_BASE, SIGREPO_API_KEY and SIGREPO_SIGNATURE (see docstring)")
if "localhost" not in BASE and "127.0.0.1" not in BASE:
    sys.exit("refusing: SIGREPO_BASE is not a local stack")

def browse():
    t0 = time.perf_counter()
    with urllib.request.urlopen(f"{BASE}/signatures/search?api_key={KEY}&limit=5", timeout=120) as r:
        r.read()
    return time.perf_counter() - t0

def annotate():
    body = json.dumps({"api_key": KEY, "signature_hashkeys": [HK], "collection": COLL,
                       "test": "hypergeometric"}).encode()
    req = urllib.request.Request(f"{BASE}/annotate/run", data=body,
                                 headers={"Content-Type": "application/json"})
    t0 = time.perf_counter()
    with urllib.request.urlopen(req, timeout=600) as r:
        r.read()
    return time.perf_counter() - t0

idle = [browse() for _ in range(10)]
print(f"IDLE browse:            median {statistics.median(idle)*1000:6.1f} ms  max {max(idle)*1000:6.1f} ms")

ann, samples, stop = [], [], threading.Event()
def ann_worker():
    for _ in range(N):
        ann.append(annotate())
def browse_worker(t_start):
    while not stop.is_set():
        off = time.perf_counter() - t_start
        try:
            samples.append((off, browse()))
        except Exception:
            samples.append((off, None))
        time.sleep(0.10)

t_start = time.perf_counter()
a = threading.Thread(target=ann_worker); b = threading.Thread(target=browse_worker, args=(t_start,))
a.start(); b.start(); a.join(); window = time.perf_counter() - t_start; stop.set(); b.join()

during = [d for off, d in samples if d is not None and off < window]
print(f"ENRICHMENTS:            {N} x {statistics.median(ann):.2f} s  (window {window:.1f} s)")
print(f"BROWSE during window:   median {statistics.median(during)*1000:6.1f} ms  max {max(during)*1000:6.1f} ms  "
      f"served {len(during)} ({len(during)/window:.1f}/s)")
verdict = "NOT BLOCKED" if statistics.median(during) < 3 * statistics.median(idle) else "BLOCKED"
print(f"VERDICT:                browse is {verdict} by enrichment")
