import { useState } from "react";
import { Upload } from "lucide-react";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import DataTable, { type Column } from "../components/DataTable";
import { collections, type Collection } from "../data/mock";

export default function CollectionsPage() {
  const [selected, setSelected] = useState<Collection | null>(collections[0]);

  const columns: Column<Collection>[] = [
    { key: "collection_name", label: "Collection" },
    { key: "num_signatures", label: "# Signatures" },
    { key: "user_name", label: "Owner" },
    {
      key: "visibility",
      label: "Visibility",
      render: (row) => (
        <span className={"badge " + (row.visibility === "Public" ? "badge-status" : "badge-local")}>
          {row.visibility}
        </span>
      ),
    },
    { key: "date_created", label: "Created" },
  ];

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #184766 0%, #2d6f8f 100%)"
        title="Browse Collections"
        description="Select a collection from the repository to review metadata, member signatures, and collection-level details in one place."
      />

      <Card>
        <div className="toolbar">
          <div className="toolbar-group">
            <button className="btn btn-primary">
              <Upload size={16} /> Upload Collection
            </button>
          </div>
        </div>
        <p className="card-helper">
          Select a collection row to make it active. Use View to load the full collection details when you want to
          inspect it below.
        </p>
        <DataTable
          columns={columns}
          rows={collections}
          rowKey="collection_id"
          selectedKey={selected?.collection_id ?? null}
          onSelectRow={setSelected}
        />
      </Card>

      <Card
        title="Selected Collection"
        helper="The active collection summary appears immediately. Use View to fetch the collection details and member signatures on demand."
      >
        {!selected ? (
          <div className="empty-state">No collection selected.</div>
        ) : (
          <>
            <div className="selected-header">
              <span className="selected-label">Selection</span>
              <span className="selected-name">{selected.collection_name}</span>
            </div>
            <div className="stat-grid" style={{ marginTop: 14 }}>
              <div className="stat-card">
                <span className="stat-label">Signatures</span>
                <span className="stat-value">{selected.num_signatures}</span>
              </div>
              <div className="stat-card">
                <span className="stat-label">Owner</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.user_name}</span>
              </div>
              <div className="stat-card">
                <span className="stat-label">Visibility</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.visibility}</span>
              </div>
              <div className="stat-card">
                <span className="stat-label">Created</span>
                <span className="stat-value" style={{ fontSize: 16 }}>{selected.date_created}</span>
              </div>
            </div>
            <p className="card-helper" style={{ marginTop: 14 }}>{selected.description}</p>
          </>
        )}
      </Card>
    </div>
  );
}
