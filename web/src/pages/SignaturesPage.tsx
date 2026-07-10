import { useState } from "react";
import { Plus, Upload } from "lucide-react";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import DataTable, { type Column } from "../components/DataTable";
import { signatures, featurePreview, type Signature } from "../data/mock";

export default function SignaturesPage() {
  const [sourceFilter, setSourceFilter] = useState<"all" | "local" | "remote">("all");
  const [selected, setSelected] = useState<Signature | null>(signatures[0]);

  const filtered = signatures.filter((s) => sourceFilter === "all" || s.source_type === sourceFilter);

  const columns: Column<Signature>[] = [
    { key: "signature_name", label: "Signature" },
    {
      key: "source_type",
      label: "Source",
      render: (row) => (
        <span className={"badge " + (row.source_type === "remote" ? "badge-remote" : "badge-local")}>
          {row.source_label}
        </span>
      ),
    },
    { key: "organism", label: "Organism" },
    { key: "assay_type", label: "Assay" },
    { key: "phenotype", label: "Phenotype" },
    { key: "user_name", label: "Owner" },
    { key: "visibility", label: "Visibility" },
    { key: "date_created", label: "Created" },
  ];

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #143a5a 0%, #245f86 100%)"
        title="Browse Signatures"
        description="Select a signature from the repository to review metadata, raw signature values, and differential expression in one place."
      />

      <Card>
        <div className="toolbar">
          <div className="toolbar-group">
            <button className="btn btn-default">
              <Plus size={16} /> Create Signature
            </button>
            <button className="btn btn-primary">
              <Upload size={16} /> Upload Signature
            </button>
            <span className="badge badge-status">Basket (0)</span>
          </div>
          <div className="toolbar-group">
            <select
              className="select-input"
              value={sourceFilter}
              onChange={(e) => setSourceFilter(e.target.value as typeof sourceFilter)}
            >
              <option value="all">All Sources</option>
              <option value="local">Local Only</option>
              <option value="remote">Connected Nodes</option>
            </select>
            <button className="btn btn-default">Manage Connections</button>
            <button className="btn btn-default">Refresh Sources</button>
          </div>
        </div>
        <p className="card-helper">
          Highlight one or more rows to add them to the basket. The most recently clicked row becomes the active
          selection, and View will load its full contents on demand.
        </p>
        <DataTable
          columns={columns}
          rows={filtered}
          rowKey="signature_id"
          selectedKey={selected?.signature_id ?? null}
          onSelectRow={setSelected}
        />
      </Card>

      <Card title="Selected Signature" helper="Selecting a row updates the active signature. Use View to load the full metadata and data tables below.">
        {!selected ? (
          <div className="empty-state">No signature selected.</div>
        ) : (
          <>
            <div className="selected-header">
              <span className="selected-label">Selection</span>
              <span className="selected-name">{selected.signature_name}</span>
            </div>
            <div className="stat-grid" style={{ marginTop: 14 }}>
              <div className="stat-card">
                <span className="stat-label">Organism</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.organism}</span>
              </div>
              <div className="stat-card">
                <span className="stat-label">Assay Type</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.assay_type}</span>
              </div>
              <div className="stat-card">
                <span className="stat-label">Sample Type</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.sample_type}</span>
              </div>
              <div className="stat-card">
                <span className="stat-label">Phenotype</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.phenotype}</span>
              </div>
            </div>
            <p className="card-helper" style={{ marginTop: 14 }}>{selected.description}</p>
            <h4 style={{ marginBottom: 8 }}>Top Features (preview)</h4>
            <table className="dt-table dt-table-compact">
              <thead>
                <tr>
                  <th>Feature</th>
                  <th>Symbol</th>
                  <th>Score</th>
                  <th>Direction</th>
                </tr>
              </thead>
              <tbody>
                {featurePreview.map((f) => (
                  <tr key={f.feature_name}>
                    <td>{f.feature_name}</td>
                    <td>{f.symbol}</td>
                    <td>{f.score.toFixed(2)}</td>
                    <td>
                      <span className={"badge " + (f.direction === "+" ? "badge-status" : "badge-remote")}>
                        {f.direction}
                      </span>
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
          </>
        )}
      </Card>
    </div>
  );
}
