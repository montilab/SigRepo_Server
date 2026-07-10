import { useState } from "react";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import { signatures } from "../data/mock";

const enrichmentResults = [
  { geneset: "REACTOME_CELLULAR_SENESCENCE", pval: 0.00012, fdr: 0.0021, overlap: "14/86" },
  { geneset: "KEGG_COMPLEMENT_AND_COAGULATION_CASCADES", pval: 0.00045, fdr: 0.0038, overlap: "9/68" },
  { geneset: "WP_OXIDATIVE_STRESS_RESPONSE", pval: 0.0012, fdr: 0.0091, overlap: "7/41" },
  { geneset: "REACTOME_INNATE_IMMUNE_SYSTEM", pval: 0.0034, fdr: 0.018, overlap: "11/122" },
  { geneset: "KEGG_PPAR_SIGNALING_PATHWAY", pval: 0.0067, fdr: 0.031, overlap: "5/29" },
];

export default function AnnotatePage() {
  const [selectedSig, setSelectedSig] = useState(signatures[0].signature_id);
  const [collection, setCollection] = useState("CP:REACTOME");
  const [hasRun, setHasRun] = useState(false);

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #1f4a37 0%, #2f7a58 100%)"
        title="Annotate"
        description="Run gene set enrichment analysis against repository signatures using MSigDB collections, powered by hypeR."
      />

      <Card title="Enrichment Setup" helper="Choose a signature and MSigDB gene set collection, then run the enrichment.">
        <div className="form-grid-2">
          <label className="field">
            <span>Signature</span>
            <select className="select-input" value={selectedSig} onChange={(e) => setSelectedSig(e.target.value)}>
              {signatures.map((s) => (
                <option key={s.signature_id} value={s.signature_id}>
                  {s.signature_name}
                </option>
              ))}
            </select>
          </label>
          <label className="field">
            <span>MSigDB Collection</span>
            <select className="select-input" value={collection} onChange={(e) => setCollection(e.target.value)}>
              <option value="CP:REACTOME">CP:REACTOME</option>
              <option value="CP:KEGG_LEGACY">CP:KEGG_LEGACY</option>
              <option value="CP:WIKIPATHWAYS">CP:WIKIPATHWAYS</option>
              <option value="CP:BIOCARTA">CP:BIOCARTA</option>
              <option value="H">H (Hallmark)</option>
            </select>
          </label>
        </div>
        <button className="btn btn-primary" style={{ marginTop: 14 }} onClick={() => setHasRun(true)}>
          Run Enrichment
        </button>
      </Card>

      <Card
        title="Enrichment Results"
        helper={hasRun ? `Top gene sets enriched in ${signatures.find((s) => s.signature_id === selectedSig)?.signature_name} (${collection}).` : "Run an enrichment to see results here."}
      >
        {!hasRun ? (
          <div className="empty-state">No enrichment run yet.</div>
        ) : (
          <table className="dt-table">
            <thead>
              <tr>
                <th>Gene Set</th>
                <th>P-value</th>
                <th>FDR</th>
                <th>Overlap</th>
              </tr>
            </thead>
            <tbody>
              {enrichmentResults.map((r) => (
                <tr key={r.geneset}>
                  <td>{r.geneset}</td>
                  <td>{r.pval.toExponential(1)}</td>
                  <td>{r.fdr.toFixed(4)}</td>
                  <td>{r.overlap}</td>
                </tr>
              ))}
            </tbody>
          </table>
        )}
      </Card>
    </div>
  );
}
