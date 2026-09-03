import { useEffect, useState } from "react";
import { Search } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { SkeletonRows } from "../components/Skeleton";
import {
  getVocabulary,
  searchFeatures,
  ApiError,
  type Vocabulary,
  type FeatureRow,
} from "../api/client";

const PAGE_SIZE = 25;

// Assay types the reference catalog can be browsed by, matching the server's
// FEATURE_SOURCES. Metabolomics is deliberately absent: its features are keyed
// by an identifier namespace (refmet/hmdb/smiles/inchikey) that has to be chosen
// explicitly, so it needs its own control rather than being folded in here and
// silently returning nothing.
const ASSAY_TYPES = ["transcriptomics", "proteomics", "snps"] as const;

const FALLBACK_ORGANISMS = ["Homo sapiens", "Mus musculus"];

// Human-readable headers for the columns the server may return. This doubles as
// the allow-list: a column absent from here is not rendered at all, so the table
// can never show a raw database name as a header.
const COLUMN_LABELS: Record<string, string> = {
  feature_name: "Feature",
  gene_symbol: "Symbol",
  chromosome: "Chr",
  position: "Position",
  annotation: "Annotation",
  organism: "Organism",
  version: "Version",
};

const MONO_COLUMNS = new Set(["feature_name", "position"]);
const NUMERIC_COLUMNS = new Set(["position"]);

function formatValue(value: unknown): string {
  if (value === null || value === undefined || value === "") return "—";
  return String(value);
}

export default function BrowsePage() {
  const [vocab, setVocab] = useState<Vocabulary | null>(null);
  useEffect(() => {
    let cancelled = false;
    getVocabulary()
      .then((v) => {
        if (!cancelled) setVocab(v);
      })
      .catch(() => {
        /* organism filter falls back below */
      });
    return () => {
      cancelled = true;
    };
  }, []);
  const organismOptions =
    vocab && vocab.organism.length > 0 ? vocab.organism : FALLBACK_ORGANISMS;

  const [assay, setAssay] = useState<string>("transcriptomics");
  const [organism, setOrganism] = useState("");
  // Free text is held as a draft until Enter commits it: the transcriptomics
  // table has ~100k rows and a LIKE query per keystroke would hammer it.
  const [draftQuery, setDraftQuery] = useState("");
  const [query, setQuery] = useState("");
  const [page, setPage] = useState(0);

  const [rows, setRows] = useState<FeatureRow[]>([]);
  const [columns, setColumns] = useState<string[]>([]);
  const [total, setTotal] = useState(0);
  const [loading, setLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);

  const [active, setActive] = useState<FeatureRow | null>(null);

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setLoadError(null);
    searchFeatures({
      assayType: assay,
      q: query || undefined,
      organism: organism || undefined,
      limit: PAGE_SIZE,
      offset: page * PAGE_SIZE,
    })
      .then((result) => {
        // Same cancellation guard the other paged pages use: without it a fast
        // filter change can let an earlier, slower response land after a later
        // one and overwrite the table with stale rows.
        if (!cancelled) {
          setRows(result.rows);
          setColumns(result.columns);
          setTotal(result.total);
        }
      })
      .catch((err) => {
        if (!cancelled) {
          setRows([]);
          setColumns([]);
          setTotal(0);
          setLoadError(
            err instanceof ApiError ? err.message : "Could not load the reference catalog."
          );
        }
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [assay, organism, query, page]);

  // Built from what the server returned, not from a fixed list, so each assay
  // type shows its own fields -- genetic variants have chromosome/position and
  // transcriptomics does not.
  //
  // Filtered to columns this page knows how to label. A column the server grows
  // later is skipped rather than rendered with a raw database name as its
  // header; add it to COLUMN_LABELS to surface it. The cast is safe precisely
  // because of that filter -- every surviving key is a field of FeatureRow.
  const tableColumns: Column<FeatureRow>[] = columns
    .filter((key) => key in COLUMN_LABELS)
    .map((key) => ({
      key: key as keyof FeatureRow,
      label: COLUMN_LABELS[key],
      align: NUMERIC_COLUMNS.has(key) ? ("right" as const) : undefined,
      render: (r: FeatureRow) => {
        const value = (r as unknown as Record<string, unknown>)[key];
        const text = formatValue(value);
        if (key === "feature_name") return <span className="cell-strong cell-mono">{text}</span>;
        if (MONO_COLUMNS.has(key)) return <span className="cell-mono">{text}</span>;
        return text;
      },
    }));

  return (
    <div className="page">
      <PageHeader
        title="Reference Browser"
        subtitle={
          loading
            ? "Loading the reference catalog…"
            : `${total.toLocaleString()} current ${assay} features`
        }
      />

      <div className="browse-layout">
        <Card title="Filters" className="browse-filters">
          <label className="field">
            <span className="field-label">Assay type</span>
            <select
              className="input"
              value={assay}
              onChange={(e) => {
                setAssay(e.target.value);
                setPage(0);
              }}
            >
              {ASSAY_TYPES.map((a) => (
                <option key={a} value={a}>
                  {a}
                </option>
              ))}
            </select>
          </label>

          <label className="field">
            <span className="field-label">Organism</span>
            <select
              className="input"
              value={organism}
              onChange={(e) => {
                setOrganism(e.target.value);
                setPage(0);
              }}
            >
              <option value="">Any</option>
              {organismOptions.map((o) => (
                <option key={o} value={o}>
                  {o}
                </option>
              ))}
            </select>
          </label>
        </Card>

        <Card padded={false} className="browse-results">
          <div className="toolbar">
            <div className="input-affix toolbar-search">
              <Search size={15} className="toolbar-search-icon" />
              <input
                className="input input-flush"
                placeholder="Search by identifier or symbol, then press Enter…"
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
          </div>

          {loadError && (
            <p className="login-error" style={{ margin: "0 16px 12px" }}>
              {loadError}
            </p>
          )}

          {loading && rows.length === 0 ? (
            <SkeletonRows rows={10} cols={4} />
          ) : (
            <DataTable
              columns={tableColumns}
              rows={rows}
              rowKey="feature_name"
              selectedKey={active?.feature_name ?? null}
              onSelectRow={setActive}
              emptyLabel="No features match your filters"
              scrollable
              maxHeight={560}
              searchable={false}
              serverPagination={{ page, pageSize: PAGE_SIZE, total, onPageChange: setPage }}
            />
          )}
        </Card>
      </div>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active?.feature_name ?? ""}
        subtitle={active ? `${assay}${active.organism ? ` · ${active.organism}` : ""}` : undefined}
      >
        {active && (
          <dl className="detail-list">
            {columns.filter((key) => key in COLUMN_LABELS).map((key) => {
              const value = (active as unknown as Record<string, unknown>)[key];
              if (value === null || value === undefined || value === "") return null;
              return (
                <div key={key}>
                  <dt>{COLUMN_LABELS[key]}</dt>
                  <dd>{String(value)}</dd>
                </div>
              );
            })}
          </dl>
        )}
      </Drawer>
    </div>
  );
}
