# SigRepo web interface

The SigRepo front end: a React single-page app that talks to the Plumber API in
`api/`. It is the interface served at [sigrepo.org](https://sigrepo.org), built
to static assets and shipped as its own container.

## What each page does

| Page | Backed by |
| --- | --- |
| Dashboard | `/insights` — repository counts, recent signatures, top contributors |
| Signatures | `/signatures/search`, `/signatures/upload`, `/signatures/delete`, `/signatures/update` |
| Collections | `/collections/*` — create, delete, add and remove signatures |
| Annotate | `/annotate/*` — hypeR enrichment against cached MSigDB collections |
| Compare | `/signatures/compare`, `/signatures/compare/leading_edge` |
| Browse | `/features/search` — the reference feature catalog |

Sign-in is real: `/login` exchanges a username and password for an api_key,
which is held in `localStorage` and sent with every subsequent request. What a
person can see and do follows from their role and the per-signature access
grants the API enforces — `src/permissions.ts` mirrors those checks to decide
which controls to show, and is not itself the security boundary.

## Running it

Against a local API on port 3838:

```
cd web
npm install
npm run dev
```

Vite proxies `/api/*` to `http://localhost:3838`, so there is no CORS setup and
no API base URL to configure. Point it elsewhere with `VITE_API_TARGET`:

```
VITE_API_TARGET=http://localhost:8020 npm run dev
```

To run against the full local stack instead (MySQL, API, MCP and this app in
containers), use `docker-compose-local.yml` from the repository root.

## Building

```
npm run build
```

This runs `tsc -b` before `vite build`, and that type check is the one that
matters: `npx tsc --noEmit` resolves the root config and will report success on
code the build rejects. Treat `npm run build` as the check.

The production image (`web/Dockerfile`) is two-stage — Node builds the bundle,
nginx serves it — so the runtime image carries no Node and no source. nginx also
proxies `/api` to the `sigrepo-api` container, which keeps the API off the
public internet and avoids CORS entirely.

## Known gaps

- **Metabolomics is missing from Browse.** Its features are keyed by an
  identifier namespace (refmet/hmdb/smiles/inchikey) that has to be chosen
  explicitly, so it needs a control of its own rather than being folded into the
  assay-type filter and silently returning nothing.
