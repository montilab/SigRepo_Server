import { useMemo, useState } from "react";
import { Plus, Upload, Search, Download } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { signatures, featurePreview, type Signature } from "../data/mock";

export default function SignaturesPage() {
  const [query, setQuery] = useState("");
  const [source, setSource] = useState<"all" | "local" | "remote">("all");
  const [active, setActive] = useState<Signature | null>(null);

  const rows = useMemo(
    () =>
      signatures.filter((s) => {
        if (source !== "all" && s.source_type !== source) return false;
        if (query && !`${s.signature_name} ${s.organism} ${s.phenotype}`.toLowerCase().includes(query.toLowerCase()))
          return false;
        return true;
      }),
    [query, source]
  );

  const columns: Column<Signature>[] = [
    {
      key: "signature_name",
      label: "Signature",
      render: (r) => (
        <div>
          <span className="cell-strong">{r.signature_name}</span>
          <span className="cell-sub">{r.description}</span>
        </div>
      ),
    },
    { key: "organism", label: "Organism", render: (r) => <span className="cell-italic">{r.organism}</span> },
    { key: "assay_type", label: "Assay", render: (r) => <Badge tone="neutral">{r.assay_type}</Badge> },
    { key: "phenotype", label: "Phenotype" },
    {
      key: "source_type",
      label: "Source",
      render: (r) => <Badge tone={r.source_type === "remote" ? "accent" : "neutral"}>{r.source_label}</Badge>,
    },
    {
      key: "visibility",
      label: "Visibility",
      render: (r) => <Badge tone={r.visibility === "Public" ? "success" : "neutral"}>{r.visibility}</Badge>,
    },
  ];

  return (
    <div className="page">
      <PageHeader
        title="Signatures"
        subtitle={`${signatures.length} signatures across the repository`}
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
          <div className="segmented">
            {(["all", "local", "remote"] as const).map((s) => (
              <button
                key={s}
                className={"segmented-btn" + (source === s ? " segmented-btn-active" : "")}
                onClick={() => setSource(s)}
              >
                {s === "all" ? "All" : s === "local" ? "Local" : "Connected"}
              </button>
            ))}
          </div>
        </div>
        <DataTable
          columns={columns}
          rows={rows}
          rowKey="signature_id"
          selectedKey={active?.signature_id ?? null}
          onSelectRow={setActive}
          emptyLabel="No signatures match your filters"
        />
      </Card>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active?.signature_name ?? ""}
        subtitle={active ? `${active.organism} · ${active.assay_type}` : ""}
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
              <div><dt>Phenotype</dt><dd>{active.phenotype}</dd></div>
              <div><dt>Sample type</dt><dd>{active.sample_type}</dd></div>
              <div><dt>Owner</dt><dd>{active.user_name}</dd></div>
              <div><dt>Visibility</dt><dd><Badge tone={active.visibility === "Public" ? "success" : "neutral"}>{active.visibility}</Badge></dd></div>
              <div><dt>Source</dt><dd><Badge tone={active.source_type === "remote" ? "accent" : "neutral"}>{active.source_label}</Badge></dd></div>
              <div><dt>Created</dt><dd>{active.date_created}</dd></div>
            </dl>

            <p className="detail-desc">{active.description}</p>

            <h4 className="detail-section-title">Top features</h4>
            <table className="dt-table dt-table-flush dt-table-compact">
              <thead>
                <tr>
                  <th>Feature</th>
                  <th className="dt-right">Score</th>
                  <th className="dt-right">Direction</th>
                </tr>
              </thead>
              <tbody>
                {featurePreview.map((f) => (
                  <tr key={f.feature_name}>
                    <td className="cell-strong">{f.feature_name}</td>
                    <td className="dt-right cell-mono">{f.score.toFixed(2)}</td>
                    <td className="dt-right">
                      <Badge tone={f.direction === "+" ? "success" : "danger"}>{f.direction === "+" ? "Up" : "Down"}</Badge>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </>
        )}
      </Drawer>
    </div>
  );
}
