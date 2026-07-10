import { useState } from "react";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import DataTable, { type Column } from "../components/DataTable";
import { referenceFeatures } from "../data/mock";

type RefFeature = (typeof referenceFeatures)[number];

export default function BrowsingPage() {
  const [organism, setOrganism] = useState("Homo sapiens");
  const [assay, setAssay] = useState("transcriptomics");
  const [featureName, setFeatureName] = useState("");
  const [selected, setSelected] = useState<RefFeature | null>(null);

  const results = referenceFeatures.filter(
    (f) => f.assay_type === assay && (!featureName || f.feature_name.toLowerCase().includes(featureName.toLowerCase()))
  );

  const columns: Column<RefFeature>[] = [
    { key: "feature_name", label: "Feature" },
    { key: "symbol", label: "Symbol" },
    { key: "gene_id", label: "Gene ID" },
    { key: "chromosome", label: "Chr" },
  ];

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #17485f 0%, #2d758d 100%)"
        title="Reference Browser"
        description="Search transcriptomic or proteomic reference features, review the result set, and inspect individual feature records inline."
      />

      <div className="grid-1-2">
        <Card title="Search Filters" helper="Choose an assay and organism, then optionally narrow the results to a specific feature name.">
          <div className="form-grid-2">
            <label className="field">
              <span>Organism</span>
              <select className="select-input" value={organism} onChange={(e) => setOrganism(e.target.value)}>
                <option>Homo sapiens</option>
                <option>Mus musculus</option>
              </select>
            </label>
            <label className="field">
              <span>Assay</span>
              <select className="select-input" value={assay} onChange={(e) => setAssay(e.target.value)}>
                <option value="transcriptomics">Transcriptomics</option>
                <option value="proteomics">Proteomics</option>
              </select>
            </label>
          </div>
          <label className="field" style={{ marginTop: 12 }}>
            <span>Feature Name</span>
            <input
              className="select-input"
              placeholder="Optional: filter to a specific feature"
              value={featureName}
              onChange={(e) => setFeatureName(e.target.value)}
            />
          </label>
          <button className="btn btn-primary" style={{ marginTop: 14 }}>
            Search Features
          </button>
        </Card>

        <div>
          <Card title="Search Results">
            <p className="card-helper">{results.length} feature(s) found for {assay} in {organism}.</p>
            <DataTable
              columns={columns}
              rows={results}
              rowKey="gene_id"
              selectedKey={selected?.gene_id ?? null}
              onSelectRow={setSelected}
            />
          </Card>

          <Card
            title="Selected Feature"
            helper="Selecting a row loads the feature details below so you can inspect the full record without scanning every column in the table."
          >
            {!selected ? (
              <div className="empty-state">No feature selected.</div>
            ) : (
              <div className="stat-grid">
                <div className="stat-card">
                  <span className="stat-label">Feature</span>
                  <span className="stat-value" style={{ fontSize: 16 }}>{selected.feature_name}</span>
                </div>
                <div className="stat-card">
                  <span className="stat-label">Symbol</span>
                  <span className="stat-value" style={{ fontSize: 16 }}>{selected.symbol}</span>
                </div>
                <div className="stat-card">
                  <span className="stat-label">Gene ID</span>
                  <span className="stat-value" style={{ fontSize: 16 }}>{selected.gene_id}</span>
                </div>
                <div className="stat-card">
                  <span className="stat-label">Chromosome</span>
                  <span className="stat-value" style={{ fontSize: 16 }}>{selected.chromosome}</span>
                </div>
              </div>
            )}
          </Card>
        </div>
      </div>
    </div>
  );
}
