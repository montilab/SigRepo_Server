import { useState } from "react";
import { ScatterChart, Scatter, XAxis, YAxis, ZAxis, CartesianGrid, Tooltip, ResponsiveContainer, Cell } from "recharts";
import { ArrowLeft, ArrowRight, Play, RotateCcw } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Stepper from "../components/Stepper";
import { signatures, enrichmentResults } from "../data/mock";

const STEPS = ["Signature", "Method", "Results"];
const SIGNIFICANT_FDR = 0.01;
const tooltipStyle = { border: "1px solid var(--border)", borderRadius: 8, boxShadow: "var(--shadow-md)", fontSize: 12 } as const;

export default function AnnotatePage() {
  const [step, setStep] = useState(0);
  const [sigId, setSigId] = useState(signatures[0].signature_id);
  const [method, setMethod] = useState<"hypeR" | "hyperGEM">("hypeR");
  const [collection, setCollection] = useState("CP:REACTOME");
  const [fdr, setFdr] = useState(0.05);

  const sig = signatures.find((s) => s.signature_id === sigId)!;
  const plot = enrichmentResults
    .filter((r) => r.fdr <= fdr)
    .map((r) => ({ ...r, geneRatio: r.overlapCount / r.genesetSize, shortName: r.geneset.length > 30 ? r.geneset.slice(0, 28) + "…" : r.geneset }));

  return (
    <div className="page">
      <PageHeader title="Annotate" subtitle="Gene set enrichment analysis against MSigDB, powered by hypeR." />

      <Card>
        <Stepper steps={STEPS} current={step} />
      </Card>

      {step === 0 && (
        <Card title="Choose a signature" subtitle="Select the signature to run enrichment against">
          <div className="radio-list">
            {signatures.map((s) => (
              <label key={s.signature_id} className={"radio-row" + (sigId === s.signature_id ? " radio-row-active" : "")}>
                <input type="radio" name="sig" checked={sigId === s.signature_id} onChange={() => setSigId(s.signature_id)} />
                <span className="radio-row-text">
                  <strong>{s.signature_name}</strong>
                  <small>{s.organism} · {s.assay_type} · {s.phenotype}</small>
                </span>
                <Badge tone="neutral">{s.assay_type}</Badge>
              </label>
            ))}
          </div>
          <div className="wizard-nav">
            <button className="btn btn-primary" onClick={() => setStep(1)}>
              Continue <ArrowRight size={16} />
            </button>
          </div>
        </Card>
      )}

      {step === 1 && (
        <Card title="Method & gene set library" subtitle={`Configure the run for ${sig.signature_name}`}>
          <div className="form-grid">
            <label className="field">
              <span className="field-label">Method</span>
              <select className="input" value={method} onChange={(e) => setMethod(e.target.value as typeof method)}>
                <option value="hypeR">runHypeR — hypergeometric</option>
                <option value="hyperGEM">runHyperGEM — GSEA-style</option>
              </select>
            </label>
            <label className="field">
              <span className="field-label">MSigDB collection</span>
              <select className="input" value={collection} onChange={(e) => setCollection(e.target.value)}>
                <option value="CP:REACTOME">CP:REACTOME</option>
                <option value="CP:KEGG_LEGACY">CP:KEGG_LEGACY</option>
                <option value="CP:WIKIPATHWAYS">CP:WIKIPATHWAYS</option>
                <option value="CP:BIOCARTA">CP:BIOCARTA</option>
                <option value="H">H — Hallmark</option>
              </select>
            </label>
          </div>
          <label className="field field-slider">
            <span className="field-label">FDR cutoff <span className="field-value">{fdr.toFixed(2)}</span></span>
            <input type="range" min={0.01} max={0.1} step={0.01} value={fdr} onChange={(e) => setFdr(Number(e.target.value))} />
          </label>
          <div className="wizard-nav">
            <button className="btn btn-ghost" onClick={() => setStep(0)}>
              <ArrowLeft size={16} /> Back
            </button>
            <button className="btn btn-primary" onClick={() => setStep(2)}>
              <Play size={15} /> Run enrichment
            </button>
          </div>
        </Card>
      )}

      {step === 2 && (
        <>
          <Card
            title="Enrichment results"
            subtitle={`${sig.signature_name} · ${collection} · ${method} · FDR ≤ ${fdr.toFixed(2)}`}
            actions={
              <button className="btn btn-ghost btn-sm" onClick={() => setStep(1)}>
                <RotateCcw size={14} /> Edit
              </button>
            }
          >
            {plot.length === 0 ? (
              <p className="muted-note">No gene sets pass the current FDR cutoff.</p>
            ) : (
              <ResponsiveContainer width="100%" height={280}>
                <ScatterChart margin={{ top: 8, right: 24, bottom: 20, left: 8 }}>
                  <CartesianGrid stroke="var(--viz-grid)" />
                  <XAxis type="number" dataKey="geneRatio" name="Gene ratio" tickFormatter={(v) => v.toFixed(2)} tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} label={{ value: "Gene ratio", position: "insideBottom", offset: -8, fontSize: 12, fill: "var(--text-secondary)" }} />
                  <YAxis type="category" dataKey="shortName" width={230} tick={{ fontSize: 11, fill: "var(--text-secondary)" }} axisLine={false} tickLine={false} />
                  <ZAxis dataKey="overlapCount" range={[90, 460]} name="Overlap" />
                  <Tooltip cursor={{ strokeDasharray: "3 3" }} contentStyle={tooltipStyle} formatter={(v, n) => (n === "Gene ratio" && typeof v === "number" ? v.toFixed(3) : v)} labelFormatter={() => ""} />
                  <Scatter data={plot}>
                    {plot.map((e) => (
                      <Cell key={e.geneset} fill={e.fdr <= SIGNIFICANT_FDR ? "var(--accent)" : "var(--viz-2)"} fillOpacity={0.85} />
                    ))}
                  </Scatter>
                </ScatterChart>
              </ResponsiveContainer>
            )}
          </Card>

          <Card padded={false}>
            <table className="dt-table dt-table-flush">
              <thead>
                <tr>
                  <th>Gene set</th>
                  <th className="dt-right">P-value</th>
                  <th className="dt-right">FDR</th>
                  <th className="dt-right">Overlap</th>
                </tr>
              </thead>
              <tbody>
                {plot.map((r) => (
                  <tr key={r.geneset}>
                    <td className="cell-strong">{r.geneset}</td>
                    <td className="dt-right cell-mono">{r.pval.toExponential(1)}</td>
                    <td className="dt-right cell-mono">{r.fdr.toFixed(4)}</td>
                    <td className="dt-right cell-mono">{r.overlapCount}/{r.genesetSize}</td>
                  </tr>
                ))}
              </tbody>
            </table>
            <div className="wizard-nav wizard-nav-padded">
              <button className="btn btn-ghost" onClick={() => setStep(0)}>
                <RotateCcw size={15} /> Start over
              </button>
            </div>
          </Card>
        </>
      )}
    </div>
  );
}
