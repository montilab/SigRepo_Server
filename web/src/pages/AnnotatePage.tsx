import { useState } from "react";
import {
  ScatterChart,
  Scatter,
  XAxis,
  YAxis,
  ZAxis,
  CartesianGrid,
  Tooltip,
  ResponsiveContainer,
  Cell,
} from "recharts";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import Stepper from "../components/Stepper";
import { signatures, enrichmentResults } from "../data/mock";

const STEPS = ["Signature", "Method & Library", "Results"];

const SIGNIFICANT_FDR = 0.01;

export default function AnnotatePage() {
  const [step, setStep] = useState(0);
  const [selectedSig, setSelectedSig] = useState(signatures[0].signature_id);
  const [method, setMethod] = useState<"hypeR" | "hyperGEM">("hypeR");
  const [collection, setCollection] = useState("CP:REACTOME");
  const [fdrCutoff, setFdrCutoff] = useState(0.05);

  const sig = signatures.find((s) => s.signature_id === selectedSig)!;

  const plotData = enrichmentResults
    .filter((r) => r.fdr <= fdrCutoff)
    .map((r) => ({
      ...r,
      geneRatio: r.overlapCount / r.genesetSize,
      shortName: r.geneset.length > 28 ? r.geneset.slice(0, 26) + "…" : r.geneset,
    }));

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #1f4a37 0%, #2f7a58 100%)"
        title="Annotate"
        description="Run gene set enrichment analysis against repository signatures using MSigDB collections, powered by hypeR."
      />

      <Card>
        <Stepper steps={STEPS} current={step} />
      </Card>

      {step === 0 && (
        <Card title="1. Choose a Signature" helper="Pick the signature to run enrichment analysis against.">
          <div className="annotate-sig-list">
            {signatures.map((s) => (
              <label
                className={"annotate-sig-option" + (selectedSig === s.signature_id ? " annotate-sig-option-selected" : "")}
                key={s.signature_id}
              >
                <input
                  type="radio"
                  name="signature"
                  checked={selectedSig === s.signature_id}
                  onChange={() => setSelectedSig(s.signature_id)}
                />
                <span className="annotate-sig-option-text">
                  <strong>{s.signature_name}</strong>
                  <span>
                    {s.organism} · {s.assay_type} · {s.phenotype}
                  </span>
                </span>
              </label>
            ))}
          </div>
          <div className="wizard-actions">
            <button className="btn btn-primary" onClick={() => setStep(1)}>
              Continue
            </button>
          </div>
        </Card>
      )}

      {step === 1 && (
        <Card title="2. Method & Gene Set Library" helper={`Configure the enrichment run for ${sig.signature_name}.`}>
          <div className="form-grid-2">
            <label className="field">
              <span>Method</span>
              <select className="select-input" value={method} onChange={(e) => setMethod(e.target.value as typeof method)}>
                <option value="hypeR">runHypeR (hypergeometric)</option>
                <option value="hyperGEM">runHyperGEM (GSEA-style)</option>
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
          <label className="field" style={{ marginTop: 14 }}>
            <span>FDR Cutoff ({fdrCutoff.toFixed(2)})</span>
            <input
              type="range"
              min={0.01}
              max={0.1}
              step={0.01}
              value={fdrCutoff}
              onChange={(e) => setFdrCutoff(Number(e.target.value))}
            />
          </label>
          <div className="wizard-actions">
            <button className="btn btn-default" onClick={() => setStep(0)}>
              Back
            </button>
            <button className="btn btn-primary" onClick={() => setStep(2)}>
              Run Enrichment
            </button>
          </div>
        </Card>
      )}

      {step === 2 && (
        <>
          <Card
            title="Enrichment Results"
            helper={`Top gene sets enriched in ${sig.signature_name} (${collection}, ${method}, FDR ≤ ${fdrCutoff.toFixed(2)}).`}
          >
            {plotData.length === 0 ? (
              <div className="empty-state">No gene sets pass the current FDR cutoff. Try loosening it in the previous step.</div>
            ) : (
              <ResponsiveContainer width="100%" height={280}>
                <ScatterChart margin={{ top: 10, right: 20, bottom: 10, left: 10 }}>
                  <CartesianGrid strokeDasharray="3 3" stroke="#e6edf3" />
                  <XAxis
                    type="number"
                    dataKey="geneRatio"
                    name="Gene Ratio"
                    tickFormatter={(v) => v.toFixed(2)}
                    tick={{ fontSize: 11, fill: "#3f556b" }}
                    label={{ value: "Gene Ratio", position: "insideBottom", offset: -5, fontSize: 12, fill: "#597189" }}
                  />
                  <YAxis type="category" dataKey="shortName" name="Gene Set" width={220} tick={{ fontSize: 11, fill: "#3f556b" }} />
                  <ZAxis dataKey="overlapCount" range={[80, 420]} name="Overlap" />
                  <Tooltip
                    cursor={{ strokeDasharray: "3 3" }}
                    formatter={(value, name) => (name === "Gene Ratio" && typeof value === "number" ? value.toFixed(3) : value)}
                    labelFormatter={() => ""}
                  />
                  <Scatter data={plotData}>
                    {plotData.map((entry) => (
                      <Cell key={entry.geneset} fill={entry.fdr <= SIGNIFICANT_FDR ? "#1c5d87" : "#8fb3c9"} />
                    ))}
                  </Scatter>
                </ScatterChart>
              </ResponsiveContainer>
            )}
          </Card>

          <Card>
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
                {plotData.map((r) => (
                  <tr key={r.geneset}>
                    <td>{r.geneset}</td>
                    <td>{r.pval.toExponential(1)}</td>
                    <td>{r.fdr.toFixed(4)}</td>
                    <td>
                      {r.overlapCount}/{r.genesetSize}
                    </td>
                  </tr>
                ))}
              </tbody>
            </table>
            <div className="wizard-actions">
              <button className="btn btn-default" onClick={() => setStep(1)}>
                Edit Parameters
              </button>
              <button className="btn btn-default" onClick={() => setStep(0)}>
                Start Over
              </button>
            </div>
          </Card>
        </>
      )}
    </div>
  );
}
