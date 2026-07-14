import { useState } from "react";
import { Search } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { referenceFeatures } from "../data/mock";

type RefFeature = (typeof referenceFeatures)[number];

export default function BrowsePage() {
  const [organism, setOrganism] = useState("Homo sapiens");
  const [assay, setAssay] = useState("transcriptomics");
  const [query, setQuery] = useState("");
  const [active, setActive] = useState<RefFeature | null>(null);

  const rows = referenceFeatures.filter(
    (f) => f.assay_type === assay && (!query || f.feature_name.toLowerCase().includes(query.toLowerCase()))
  );

  const columns: Column<RefFeature>[] = [
    { key: "feature_name", label: "Feature", render: (r) => <span className="cell-strong">{r.feature_name}</span> },
    { key: "symbol", label: "Symbol" },
    { key: "gene_id", label: "Gene ID", render: (r) => <span className="cell-mono">{r.gene_id}</span> },
    { key: "chromosome", label: "Chr", align: "right" },
  ];

  return (
    <div className="page">
      <PageHeader title="Browse" subtitle="Search the transcriptomic and proteomic reference feature catalog." />

      <div className="browse-layout">
        <Card title="Filters" className="browse-filters">
          <label className="field">
            <span className="field-label">Organism</span>
            <select className="input" value={organism} onChange={(e) => setOrganism(e.target.value)}>
              <option>Homo sapiens</option>
              <option>Mus musculus</option>
            </select>
          </label>
          <label className="field">
            <span className="field-label">Assay</span>
            <select className="input" value={assay} onChange={(e) => setAssay(e.target.value)}>
              <option value="transcriptomics">Transcriptomics</option>
              <option value="proteomics">Proteomics</option>
            </select>
          </label>
          <label className="field">
            <span className="field-label">Feature name</span>
            <div className="input-affix">
              <Search size={15} className="toolbar-search-icon" />
              <input className="input input-flush" placeholder="Optional filter…" value={query} onChange={(e) => setQuery(e.target.value)} />
            </div>
          </label>
          <button className="btn btn-primary btn-block">Search features</button>
        </Card>

        <Card title="Results" subtitle={`${rows.length} features · ${assay} · ${organism}`} padded={false} className="browse-results">
          <DataTable columns={columns} rows={rows} rowKey="gene_id" selectedKey={active?.gene_id ?? null} onSelectRow={setActive} emptyLabel="No features found" />
        </Card>
      </div>

      <Drawer open={active !== null} onClose={() => setActive(null)} title={active?.feature_name ?? ""} subtitle={active?.symbol}>
        {active && (
          <dl className="detail-list">
            <div><dt>Feature</dt><dd>{active.feature_name}</dd></div>
            <div><dt>Symbol</dt><dd>{active.symbol}</dd></div>
            <div><dt>Gene ID</dt><dd className="cell-mono">{active.gene_id}</dd></div>
            <div><dt>Chromosome</dt><dd>{active.chromosome}</dd></div>
            <div><dt>Assay</dt><dd><Badge tone="neutral">{active.assay_type}</Badge></dd></div>
          </dl>
        )}
      </Drawer>
    </div>
  );
}
