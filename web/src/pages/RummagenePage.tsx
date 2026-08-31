import { useCallback, useEffect, useState } from "react";
import { BookOpen, Download, Eye, ExternalLink, Search } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { SkeletonRows } from "../components/Skeleton";
import { tidyTerm } from "../lib/rummagene";
import { canUploadSignature } from "../permissions";
import {
  searchRummageneCatalog,
  getRummageneCatalogEntry,
  pullRummageneSignature,
  ApiError,
  type RummageneCatalogRow,
  type RummageneCatalogEntry,
  type RummageneCatalogSortKey,
} from "../api/client";

const PAGE_SIZE = 25;

// The columns the API will actually sort by (search_rummagene_catalog's own
// whitelist in api/lib/rummagene_catalog.R -- anything else falls back to
// year silently on the server). DataTable's serverSort mode makes EVERY
// column header a sort trigger with no per-column opt-out, but two of ours
// (the PMC link, the pull action) key on fields that aren't sortable, so
// isSortable() below turns a click on those into a deliberate no-op instead
// of silently re-sorting by whatever the server falls back to.
const RUMMAGENE_SORT_KEYS = ["term", "title", "year", "n_genes", "organism", "assay_type"] as const;
function isSortable(key: string): key is RummageneCatalogSortKey {
  return (RUMMAGENE_SORT_KEYS as readonly string[]).includes(key);
}

function formatValue(value: unknown): string {
  if (value === null || value === undefined || value === "") return "—";
  return String(value);
}

type PullState =
  | { status: "idle" }
  | { status: "pulling" }
  | { status: "done"; hashkey: string }
  | { status: "error"; message: string };

export default function RummagenePage() {
  const [rows, setRows] = useState<RummageneCatalogRow[]>([]);
  const [total, setTotal] = useState(0);
  const [page, setPage] = useState(0); // 0-based, matches DataTable's serverPagination
  const [loading, setLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);

  // Free text is held as a draft until Enter commits it: typing must not
  // fire a LIKE query per keystroke against a 135k-row table. The two
  // numeric filters commit immediately -- they're short, indexed (year,
  // n_genes) columns and cheap to requery.
  const [draftQuery, setDraftQuery] = useState("");
  const [query, setQuery] = useState("");
  const [yearMin, setYearMin] = useState("");
  const [genesMin, setGenesMin] = useState("");

  const [sortBy, setSortBy] = useState<RummageneCatalogSortKey>("year");
  const [sortDir, setSortDir] = useState<"asc" | "desc">("desc");

  // Row detail lives in a Drawer, the same pattern BrowsePage/CollectionsPage
  // use for "click a row to see more" -- not an inline expanding <tr>, which
  // DataTable has no support for.
  const [active, setActive] = useState<RummageneCatalogRow | null>(null);
  const [entry, setEntry] = useState<RummageneCatalogEntry | null>(null);
  const [entryLoading, setEntryLoading] = useState(false);
  const [entryError, setEntryError] = useState<string | null>(null);

  const [pulls, setPulls] = useState<Record<string, PullState>>({});

  // Mirrors build_signature_from_upload()'s own check (editor or admin) --
  // pulling a catalog entry takes exactly that upload path.
  const canPull = canUploadSignature();

  const load = useCallback(async () => {
    setLoading(true);
    setLoadError(null);
    try {
      const result = await searchRummageneCatalog({
        q: query || undefined,
        year_min: yearMin ? Number(yearMin) : undefined,
        n_genes_min: genesMin ? Number(genesMin) : undefined,
        limit: PAGE_SIZE,
        offset: page * PAGE_SIZE,
        sortBy,
        sortDir,
      });
      setRows(result.rows);
      setTotal(result.total);
    } catch (err) {
      setRows([]);
      setLoadError(err instanceof ApiError ? err.message : "Could not load the catalog.");
    } finally {
      setLoading(false);
    }
  }, [query, yearMin, genesMin, page, sortBy, sortDir]);

  useEffect(() => {
    void load();
  }, [load]);

  // The list endpoint omits gene_symbols/feature_names on purpose -- at
  // ~135k rows they would dominate every page response -- so opening a row
  // costs one extra request for its gene list. Everything else shown in the
  // drawer is already on the row from the list query.
  useEffect(() => {
    if (!active) {
      setEntry(null);
      setEntryError(null);
      return;
    }
    let cancelled = false;
    setEntry(null);
    setEntryError(null);
    setEntryLoading(true);
    getRummageneCatalogEntry(active.term)
      .then((e) => {
        if (!cancelled) setEntry(e);
      })
      .catch((err) => {
        if (!cancelled) {
          setEntryError(err instanceof ApiError ? err.message : "Could not load this entry's genes.");
        }
      })
      .finally(() => {
        if (!cancelled) setEntryLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [active?.term]);

  async function pull(term: string) {
    setPulls((p) => ({ ...p, [term]: { status: "pulling" } }));
    try {
      const res = await pullRummageneSignature(term);
      setPulls((p) => ({ ...p, [term]: { status: "done", hashkey: res.signature_hashkey } }));
    } catch (err) {
      // 409 is the ordinary "you already pulled this" case, not a failure
      // worth showing as a raw API error.
      const message =
        err instanceof ApiError && err.status === 409
          ? "You already have this signature."
          : err instanceof ApiError
            ? err.message
            : "Pull failed.";
      setPulls((p) => ({ ...p, [term]: { status: "error", message } }));
    }
  }

  // Shared by the table's own action column and the drawer footer, so both
  // always agree on one row's pull state.
  function pullControl(term: string) {
    if (!canPull) {
      return <span className="cell-sub">Editor access required</span>;
    }
    const state = pulls[term] ?? { status: "idle" };
    if (state.status === "done") {
      // A real anchor, not a router Link: the signature detail page is
      // meant to open in its own tab with no app chrome (see App.tsx),
      // the same convention SignaturesPage's own "View" action follows.
      return (
        <a
          className="btn btn-secondary btn-sm"
          href={`/signatures/${state.hashkey}`}
          target="_blank"
          rel="noopener"
          onClick={(e) => e.stopPropagation()}
        >
          <Eye size={13} /> View
        </a>
      );
    }
    return (
      <>
        <button
          className="btn btn-primary btn-sm"
          disabled={state.status === "pulling"}
          onClick={(e) => {
            e.stopPropagation();
            void pull(term);
          }}
        >
          <Download size={13} /> {state.status === "pulling" ? "Pulling…" : "Pull"}
        </button>
        {state.status === "error" && <span className="cell-sub">{state.message}</span>}
      </>
    );
  }

  const columns: Column<RummageneCatalogRow>[] = [
    { key: "term", label: "Gene set", render: (r) => <span className="cell-strong">{tidyTerm(r.term)}</span> },
    { key: "title", label: "Paper", render: (r) => formatValue(r.title) },
    { key: "year", label: "Year", render: (r) => formatValue(r.year) },
    { key: "n_genes", label: "Genes", align: "right" },
    {
      key: "pmcid",
      label: "Source",
      render: (r) => (
        <a
          href={`https://www.ncbi.nlm.nih.gov/pmc/articles/${r.pmcid}/`}
          target="_blank"
          rel="noreferrer"
          onClick={(e) => e.stopPropagation()}
        >
          {r.pmcid} <ExternalLink size={12} />
        </a>
      ),
    },
    { key: "rummagene_catalog_id", label: "Action", render: (r) => pullControl(r.term) },
  ];

  return (
    <div className="page">
      <PageHeader
        title="Rummagene"
        subtitle={loading ? "Loading the catalog…" : `${total.toLocaleString()} literature-mined gene sets ready to pull`}
      />

      <div className="rmg-caveat">
        <BookOpen size={15} />
        <p>
          Organism and assay type are attested by the source paper&rsquo;s PubMed MeSH indexing, not verified
          independently, and every entry here is Homo sapiens transcriptomics data whose genes already resolve
          in this repository&rsquo;s reference table. What MeSH indexing cannot tell us is whether a given table
          is actually a differential-expression contrast &mdash; read the source paper before pulling it in. A
          pulled entry becomes a private signature under your account; it is not added to the shared repository
          until you choose to share it.
        </p>
      </div>

      <Card padded={false}>
        <div className="toolbar">
          <div className="input-affix toolbar-search">
            <Search size={15} className="toolbar-search-icon" />
            <input
              className="input input-flush"
              placeholder="Search term, title or description…"
              value={draftQuery}
              onChange={(e) => setDraftQuery(e.target.value)}
              onKeyDown={(e) => {
                if (e.key === "Enter") {
                  setQuery(draftQuery.trim());
                  setPage(0);
                }
              }}
            />
          </div>
          <div className="rmg-numeric-filters">
            <label className="field field-inline">
              <span className="field-label">Year from</span>
              <input
                className="input"
                type="number"
                placeholder="Any"
                value={yearMin}
                onChange={(e) => {
                  setYearMin(e.target.value);
                  setPage(0);
                }}
                style={{ width: 84 }}
              />
            </label>
            <label className="field field-inline">
              <span className="field-label">Min genes</span>
              <input
                className="input"
                type="number"
                min={0}
                placeholder="Any"
                value={genesMin}
                onChange={(e) => {
                  setGenesMin(e.target.value);
                  setPage(0);
                }}
                style={{ width: 84 }}
              />
            </label>
          </div>
        </div>

        {loadError && (
          <p className="login-error" style={{ margin: "0 16px 12px" }}>
            {loadError}
          </p>
        )}

        {loading && rows.length === 0 ? (
          <SkeletonRows rows={10} cols={6} />
        ) : (
          <DataTable
            columns={columns}
            rows={rows}
            rowKey="term"
            selectedKey={active?.term ?? null}
            onSelectRow={setActive}
            emptyLabel="No catalog entries match your filters"
            scrollable
            maxHeight={560}
            searchable={false}
            serverPagination={{ page, pageSize: PAGE_SIZE, total, onPageChange: setPage }}
            serverSort={{
              sortBy,
              sortDir,
              onSortChange: (key, dir) => {
                if (!isSortable(key)) return;
                setSortBy(key);
                setSortDir(dir);
                // Re-sorting reorders the whole result set, so the old page
                // number no longer points at anything meaningful.
                setPage(0);
              },
            }}
          />
        )}
      </Card>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active ? (active.title ?? tidyTerm(active.term)) : ""}
        subtitle={active ? `${active.organism} · ${active.assay_type}${active.year ? ` · ${active.year}` : ""}` : undefined}
        footer={active && pullControl(active.term)}
      >
        {active && (
          <>
            <dl className="detail-list">
              <div>
                <dt>Source</dt>
                <dd>
                  <a href={`https://www.ncbi.nlm.nih.gov/pmc/articles/${active.pmcid}/`} target="_blank" rel="noreferrer">
                    {active.pmcid}
                  </a>
                </dd>
              </div>
              {active.pmid && (
                <div>
                  <dt>PMID</dt>
                  <dd>{active.pmid}</dd>
                </div>
              )}
              <div>
                <dt>Genes</dt>
                <dd>{active.n_genes}</dd>
              </div>
              {active.doi && (
                <div>
                  <dt>DOI</dt>
                  <dd>{active.doi}</dd>
                </div>
              )}
            </dl>

            <p className="detail-desc">
              <BookOpen size={13} /> Attested by MeSH: {active.mesh_evidence}
            </p>
            {active.description && <p className="detail-desc">{active.description}</p>}

            <div className="detail-section">
              <h4 className="detail-section-title">Gene symbols, as published</h4>
              {entryLoading ? (
                <SkeletonRows rows={3} />
              ) : entryError ? (
                <p className="login-error" style={{ margin: 12 }}>
                  {entryError}
                </p>
              ) : (
                <p className="rmg-genes">{entry?.gene_symbols.join(", ")}</p>
              )}
            </div>
          </>
        )}
      </Drawer>
    </div>
  );
}
