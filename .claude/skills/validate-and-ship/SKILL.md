---
name: validate-and-ship
description: Stands up a throwaway local SigRepo stack (MySQL, the Plumber API, and the MCP server), bootstraps it, runs SigRepo/local_validation's harness against it, tears everything down, and — if it looks clean — helps commit and open a pull request. Use this whenever the user asks to "run validation," "validate and ship," "validate this," "test the MCP server locally," or wants to commit/open a PR for changes to SigRepo_Server or SigRepo's local_validation harness, even if they don't spell out the full stand-up-MySQL-then-boot-the-API-then-boot-MCP procedure themselves — that's exactly what this skill exists to do instead of re-deriving it in prose each time.
---

# Validate and ship

This encapsulates a real, previously-manual procedure: spin up MySQL,
bootstrap it through `/init_db`, boot the Plumber API and the MCP server
pointed at it, run `SigRepo/local_validation`'s harness, tear everything
down, and only then think about committing. The mechanical parts are a
script because they're mechanical — getting them right by hand cost real
time to a handful of non-obvious gotchas (see below). The git/PR part at
the end is deliberately *not* a script, because it needs judgment every
time.

## Prerequisites

`SigRepo` and `SigRepo_Server` must be checked out as sibling directories
(same parent folder) — the script resolves `SigRepo`'s location relative
to itself and fails with a clear message if it can't find it.

## Step 1: Run the validation script

```bash
.claude/skills/validate-and-ship/scripts/run_validation.sh
```

Run it from anywhere; it resolves its own paths. It will:

1. Start MySQL 8 in a throwaway Docker container.
2. Boot the Plumber API, bootstrap the database via `/init_db` (schema +
   reference tables + a curated MSigDB gene-set catalog — this step alone
   now takes **~3-4 minutes**, not the ~1-2 it used to, since gene-set
   population runs as part of it).
3. Boot the MCP server pointed at the same database.
4. Preload the `LLFS_Aging_Gene_2023` example signature (one of the
   harness's read-path checks expects to find it).
5. Run `SigRepo/local_validation/run_local_validation.sh` and print its
   `[pass]`/`[fail]`/`[skip]` summary.
6. Tear everything down — container, both background R processes — no
   matter how it exits, including on failure or interruption.

Takes several minutes end-to-end, mostly waiting on step 2's gene-set
fetch. That's expected, not a hang.

### Reading the result

A nonzero exit / `[fail]` lines don't automatically mean something's
broken. As of this writing, `signature_crud` and `collection_crud` both
fail with `"the condition has length > 1"` — a documented, external bug in
the `OmicSignature` package (see `SigRepo/local_validation/SETUP_GUIDE.md`'s
troubleshooting section), unrelated to anything in either repo. The script
prints a reminder about this. The bar isn't zero failures; it's *no new
ones* beyond what's already known and explained there. If that file's
troubleshooting section has moved on or a fix has landed upstream, this
list of "expected" failures may be stale — check there directly rather
than trusting this skill's memory of it.

If something *new* fails, that's worth stopping and investigating before
moving to step 2 — don't proceed to commit/PR territory on top of an
unexplained failure.

### Why the script looks the way it does

Three things bit real time during development and are baked in as
comments in the script itself, worth knowing if you ever need to debug it
by hand instead:

- R auto-loads a `.Renviron` file from the current working directory at
  startup, which silently overrides command-line env vars with
  `SigRepo_Server`'s real deployment values (container-internal hosts,
  production IPs). Every `Rscript` call needs `--no-environ`, or it'll
  quietly try to reach the wrong host.
- `api/run_sigrepo_api.R` always binds port 3838 — not configurable via an
  env var, unlike the MCP server's `MCP_PORT`. The script checks that port
  is free before starting rather than colliding with something already
  there.
- The MCP server's connection handler needs `API_LOCAL_HOST`/
  `API_LOCAL_PORT` set explicitly, or `SigRepo::runHypeR()`'s round-trip
  to the REST API for the `difexp` table silently targets production
  instead of the local stack.

## Step 2: Commit and open a PR — only once validation looks clean

Read `references/git-workflow.md` before touching git. The short version:
check for uncommitted work that isn't yours before staging anything,
figure out the actual right base branch (don't assume `master`/`main` —
what you're extending might itself be an open, unmerged PR), and never use
a broad `git add -A`/`git add .`. That file has the full reasoning and the
exact commands.
