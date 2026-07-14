import { useState } from "react";
import { Upload, FolderPlus } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { collections, signatures, type Collection } from "../data/mock";

export default function CollectionsPage() {
  const [active, setActive] = useState<Collection | null>(null);

  const columns: Column<Collection>[] = [
    {
      key: "collection_name",
      label: "Collection",
      render: (r) => (
        <div>
          <span className="cell-strong">{r.collection_name}</span>
          <span className="cell-sub">{r.description}</span>
        </div>
      ),
    },
    { key: "num_signatures", label: "Signatures", align: "right", render: (r) => <span className="cell-mono">{r.num_signatures}</span> },
    { key: "user_name", label: "Owner" },
    {
      key: "visibility",
      label: "Visibility",
      render: (r) => <Badge tone={r.visibility === "Public" ? "success" : "neutral"}>{r.visibility}</Badge>,
    },
    { key: "date_created", label: "Created", align: "right", render: (r) => <span className="cell-muted">{r.date_created}</span> },
  ];

  return (
    <div className="page">
      <PageHeader
        title="Collections"
        subtitle={`${collections.length} collections`}
        actions={
          <>
            <button className="btn btn-secondary">
              <FolderPlus size={16} /> New collection
            </button>
            <button className="btn btn-primary">
              <Upload size={16} /> Upload
            </button>
          </>
        }
      />

      <Card padded={false}>
        <DataTable columns={columns} rows={collections} rowKey="collection_id" selectedKey={active?.collection_id ?? null} onSelectRow={setActive} />
      </Card>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active?.collection_name ?? ""}
        subtitle={active ? `${active.num_signatures} signatures` : ""}
        footer={active && <button className="btn btn-primary btn-block">Open collection</button>}
      >
        {active && (
          <>
            <dl className="detail-list">
              <div><dt>Owner</dt><dd>{active.user_name}</dd></div>
              <div><dt>Visibility</dt><dd><Badge tone={active.visibility === "Public" ? "success" : "neutral"}>{active.visibility}</Badge></dd></div>
              <div><dt>Signatures</dt><dd>{active.num_signatures}</dd></div>
              <div><dt>Created</dt><dd>{active.date_created}</dd></div>
            </dl>
            <p className="detail-desc">{active.description}</p>

            <h4 className="detail-section-title">Member signatures (sample)</h4>
            <div className="member-list">
              {signatures.slice(0, Math.min(4, active.num_signatures)).map((s) => (
                <div className="member-item" key={s.signature_id}>
                  <span className="cell-strong">{s.signature_name}</span>
                  <Badge tone="neutral">{s.assay_type}</Badge>
                </div>
              ))}
            </div>
          </>
        )}
      </Drawer>
    </div>
  );
}
