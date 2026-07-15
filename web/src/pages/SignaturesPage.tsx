import { useEffect, useMemo, useState } from "react";
import { Plus, Upload, Search, Download } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import {
  searchSignatures,
  getSignatureContext,
  type SignatureSummary,
  type SignatureContext,
} from "../api/client";

export default function SignaturesPage() {
  const [rows, setRows] = useState<SignatureSummary[]>([]);
  const [loading, setLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [query, setQuery] = useState("");

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    searchSignatures({ keyword: query || undefined, limit: 100 })
      .then((results) => {
        if (!cancelled) {
          setRows(results);
          setLoadError(null);
        }
      })
      .catch((err) => {
        if (!cancelled) setLoadError(err instanceof Error ? err.message : "Could not load signatures.");
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [query]);

  const [active, setActive] = useState<SignatureSummary | null>(null);
  const [context, setContext] = useState<SignatureContext | null>(null);
  const [contextLoading, setContextLoading] = useState(false);

  useEffect(() => {
    if (!active) {
      setContext(null);
      return;
    }
    let cancelled = false;
    setContextLoading(true);
    getSignatureContext(active.signature_hashkey)
      .then((ctx) => {
        if (!cancelled) setContext(ctx);
      })
      .catch(() => {
        if (!cancelled) setContext(null);
      })
      .finally(() => {
        if (!cancelled) setContextLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [active]);

  const columns: Column<SignatureSummary>[] = useMemo(
    () => [
      {
        key: "signature_name",
        label: "Signature",
        render: (r) => (
          <div>
            <span className="cell-strong">{r.signature_name}</span>
            {r.description && <span className="cell-sub">{r.description}</span>}
          </div>
        ),
      },
      { key: "organism", label: "Organism", render: (r) => <span className="cell-italic">{r.organism ?? "—"}</span> },
      { key: "assay_type", label: "Assay", render: (r) => <Badge tone="neutral">{r.assay_type}</Badge> },
      { key: "phenotype", label: "Phenotype", render: (r) => r.phenotype ?? "—" },
      { key: "feature_count", label: "Features", align: "right" },
      {
        key: "visibility",
        label: "Visibility",
        render: (r) => <Badge tone={r.visibility === 1 ? "success" : "neutral"}>{r.visibility === 1 ? "Public" : "Private"}</Badge>,
      },
    ],
    []
  );

  return (
    <div className="page">
      <PageHeader
        title="Signatures"
        subtitle={loading ? "Loading signatures…" : `${rows.length} signatures across the repository`}
        actions={
          <>
            <button className="btn btn-secondary">
              <Plus size={16} /> Create
            </button>
            <button className="btn btn-primary">
              <Upload size={16} /> Upload
            </button>
          </>
        }
      />

      <Card padded={false}>
        <div className="toolbar">
          <div className="input-affix toolbar-search">
            <Search size={15} className="toolbar-search-icon" />
            <input
              className="input input-flush"
              placeholder="Search signatures…"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
            />
          </div>
        </div>
        {loadError && <p className="login-error" style={{ margin: "0 16px 12px" }}>{loadError}</p>}
        <DataTable
          columns={columns}
          rows={rows}
          rowKey="signature_hashkey"
          selectedKey={active?.signature_hashkey ?? null}
          onSelectRow={setActive}
          emptyLabel={loading ? "Loading…" : "No signatures match your filters"}
        />
      </Card>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active?.signature_name ?? ""}
        subtitle={active ? `${active.organism ?? "—"} · ${active.assay_type}` : ""}
        footer={
          active && (
            <>
              <button className="btn btn-secondary">
                <Download size={15} /> Export
              </button>
              <button className="btn btn-primary">Run enrichment</button>
            </>
          )
        }
      >
        {active && (
          <>
            <dl className="detail-list">
              <div><dt>Phenotype</dt><dd>{active.phenotype ?? "—"}</dd></div>
              <div><dt>Owner</dt><dd>{active.user_name}</dd></div>
              <div><dt>Visibility</dt><dd><Badge tone={active.visibility === 1 ? "success" : "neutral"}>{active.visibility === 1 ? "Public" : "Private"}</Badge></dd></div>
              <div><dt>Created</dt><dd>{active.date_created}</dd></div>
              <div><dt>Hashkey</dt><dd className="cell-mono">{active.signature_hashkey}</dd></div>
            </dl>

            {active.description && <p className="detail-desc">{active.description}</p>}

            <h4 className="detail-section-title">Top features</h4>
            {contextLoading && <p className="cell-sub">Loading features…</p>}
            {!contextLoading && context && context.features.length > 0 && (
              <table className="dt-table dt-table-flush dt-table-compact">
                <thead>
                  <tr>
                    <th>Feature</th>
                    <th className="dt-right">Score</th>
                    <th className="dt-right">Direction</th>
                  </tr>
                </thead>
                <tbody>
                  {context.features.map((f, i) => {
                    const score = typeof f.score === "number" ? f.score : Number(f.score);
                    const label = f.probe_id ?? String(f.feature_id ?? i);
                    return (
                      <tr key={label}>
                        <td className="cell-strong">{label}</td>
                        <td className="dt-right cell-mono">{Number.isFinite(score) ? score.toFixed(2) : "—"}</td>
                        <td className="dt-right">
                          <Badge tone={score >= 0 ? "success" : "danger"}>{score >= 0 ? "Up" : "Down"}</Badge>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            )}
            {!contextLoading && context && context.features.length === 0 && (
              <p className="cell-sub">No features recorded for this signature.</p>
            )}
          </>
        )}
      </Drawer>
    </div>
  );
}
