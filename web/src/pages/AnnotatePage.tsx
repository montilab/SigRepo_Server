import { useEffect, useMemo, useState } from "react";
import { ScatterChart, Scatter, XAxis, YAxis, ZAxis, CartesianGrid, Tooltip, ResponsiveContainer, Cell } from "recharts";
import { ArrowLeft, ArrowRight, Play, RotateCcw, Loader2 } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Stepper from "../components/Stepper";
import {
  searchSignatures,
  getMsigdbCollections,
  runAnnotation,
  type SignatureSummary,
  type MsigdbCollectionOption,
  type EnrichmentRun,
} from "../api/client";

const STEPS = ["Signature", "Method", "Results"];
const SIGNIFICANT_FDR = 0.01;
const tooltipStyle = { border: "1px solid var(--border)", borderRadius: 8, boxShadow: "var(--shadow-md)", fontSize: 12 } as const;

export default function AnnotatePage() {
  const [step, setStep] = useState(0);

  const [signatures, setSignatures] = useState<SignatureSummary[]>([]);
  const [signaturesLoading, setSignaturesLoading] = useState(true);
  const [signaturesError, setSignaturesError] = useState<string | null>(null);
  const [sigHashkey, setSigHashkey] = useState("");

  useEffect(() => {
    searchSignatures({ limit: 100 })
      .then((results) => {
        setSignatures(results);
        if (results.length > 0) setSigHashkey(results[0].signature_hashkey);
      })
      .catch((err) => setSignaturesError(err instanceof Error ? err.message : "Could not load signatures."))
      .finally(() => setSignaturesLoading(false));
  }, []);

  const sig = signatures.find((s) => s.signature_hashkey === sigHashkey) ?? null;

  const [collections, setCollections] = useState<MsigdbCollectionOption[]>([]);
  const [collectionsLoading, setCollectionsLoading] = useState(true);
  const [collectionsError, setCollectionsError] = useState<string | null>(null);

  useEffect(() => {
    getMsigdbCollections()
      .then(setCollections)
      .catch((err) => setCollectionsError(err instanceof Error ? err.message : "Could not load MSigDB collections."))
      .finally(() => setCollectionsLoading(false));
  }, []);

  const collectionOptions = useMemo(() => Array.from(new Set(collections.map((c) => c.Collection))).sort(), [collections]);
  const [collection, setCollection] = useState("H");
  const subcollectionOptions = useMemo(
    () => collections.filter((c) => c.Collection === collection).map((c) => c.Subcollection).filter(Boolean).sort(),
    [collections, collection]
  );
  const [subcollection, setSubcollection] = useState("");

  useEffect(() => {
    setSubcollection("");
  }, [collection]);

  const [test, setTest] = useState<"hypergeometric" | "kstest">("hypergeometric");
  const [fdr, setFdr] = useState(0.05);

  // Rank-based enrichment needs per-feature scores from difexp, which not
  // every signature has stored.
  const kstestAvailable = sig?.has_difexp === 1;
  useEffect(() => {
    if (!kstestAvailable && test === "kstest") setTest("hypergeometric");
  }, [kstestAvailable, test]);

  const [running, setRunning] = useState(false);
  const [runError, setRunError] = useState<string | null>(null);
  const [result, setResult] = useState<EnrichmentRun | null>(null);

  async function handleRun() {
    if (!sig) return;
    setRunning(true);
    setRunError(null);
    try {
      const run = await runAnnotation({
        signatureHashkey: sig.signature_hashkey,
        test,
        collection,
        subcollection: subcollection || undefined,
        fdr,
      });
      setResult(run);
      setStep(2);
    } catch (err) {
      setRunError(err instanceof Error ? err.message : "Enrichment failed.");
    } finally {
      setRunning(false);
    }
  }

  const plot = (result?.results ?? []).map((r) => ({
    ...r,
    geneRatio: r.overlap / r.geneset,
    shortName: r.label.length > 30 ? r.label.slice(0, 28) + "…" : r.label,
  }));

  return (
    <div className="page">
      <PageHeader title="Annotate" subtitle="Gene set enrichment analysis against MSigDB, powered by hypeR." />

      <Card>
        <Stepper steps={STEPS} current={step} />
      </Card>

      {step === 0 && (
        <Card title="Choose a signature" subtitle="Select the signature to run enrichment against">
          {signaturesError && <p className="login-error">{signaturesError}</p>}
          {signaturesLoading && <p className="cell-sub">Loading signatures…</p>}
          {!signaturesLoading && signatures.length === 0 && !signaturesError && (
            <p className="muted-note">No signatures available.</p>
          )}
          <div className="radio-list">
            {signatures.map((s) => (
              <label key={s.signature_hashkey} className={"radio-row" + (sigHashkey === s.signature_hashkey ? " radio-row-active" : "")}>
                <input type="radio" name="sig" checked={sigHashkey === s.signature_hashkey} onChange={() => setSigHashkey(s.signature_hashkey)} />
                <span className="radio-row-text">
                  <strong>{s.signature_name}</strong>
                  <small>{s.organism ?? "—"} · {s.assay_type} · {s.phenotype ?? "—"}</small>
                </span>
                <Badge tone="neutral">{s.assay_type}</Badge>
              </label>
            ))}
          </div>
          <div className="wizard-nav">
            <button className="btn btn-primary" disabled={!sig} onClick={() => setStep(1)}>
              Continue <ArrowRight size={16} />
            </button>
          </div>
        </Card>
      )}

      {step === 1 && sig && (
        <Card title="Method & gene set library" subtitle={`Configure the run for ${sig.signature_name}`}>
          <div className="form-grid">
            <label className="field">
              <span className="field-label">Method</span>
              <select className="input" value={test} onChange={(e) => setTest(e.target.value as typeof test)}>
                <option value="hypergeometric">Hypergeometric — feature overlap</option>
                <option value="kstest" disabled={!kstestAvailable}>
                  Rank-based (KS test) {!kstestAvailable ? "— requires stored difexp" : ""}
                </option>
              </select>
            </label>
            <label className="field">
              <span className="field-label">MSigDB collection</span>
              <select className="input" value={collection} onChange={(e) => setCollection(e.target.value)} disabled={collectionsLoading}>
                {collectionOptions.map((c) => (
                  <option key={c} value={c}>{c}</option>
                ))}
              </select>
            </label>
            {subcollectionOptions.length > 0 && (
              <label className="field">
                <span className="field-label">Subcollection</span>
                <select className="input" value={subcollection} onChange={(e) => setSubcollection(e.target.value)}>
                  <option value="">All</option>
                  {subcollectionOptions.map((s) => (
                    <option key={s} value={s}>{s}</option>
                  ))}
                </select>
              </label>
            )}
          </div>
          {collectionsError && <p className="login-error">{collectionsError}</p>}
          <label className="field field-slider">
            <span className="field-label">FDR cutoff <span className="field-value">{fdr.toFixed(2)}</span></span>
            <input type="range" min={0.01} max={0.1} step={0.01} value={fdr} onChange={(e) => setFdr(Number(e.target.value))} />
          </label>
          {runError && <p className="login-error">{runError}</p>}
          <div className="wizard-nav">
            <button className="btn btn-ghost" onClick={() => setStep(0)} disabled={running}>
              <ArrowLeft size={16} /> Back
            </button>
            <button className="btn btn-primary" onClick={handleRun} disabled={running || collectionsLoading}>
              {running ? (
                <>
                  <Loader2 size={15} className="spin" /> Running… (first run per collection can take ~10s)
                </>
              ) : (
                <>
                  <Play size={15} /> Run enrichment
                </>
              )}
            </button>
          </div>
        </Card>
      )}

      {step === 2 && sig && result && (
        <>
          <Card
            title="Enrichment results"
            subtitle={`${result.signature_name} · ${result.collection}${result.subcollection ? ":" + result.subcollection : ""} · ${result.test} · FDR ≤ ${result.fdr.toFixed(2)}`}
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
                  <ZAxis dataKey="overlap" range={[90, 460]} name="Overlap" />
                  <Tooltip cursor={{ strokeDasharray: "3 3" }} contentStyle={tooltipStyle} formatter={(v, n) => (n === "Gene ratio" && typeof v === "number" ? v.toFixed(3) : v)} labelFormatter={() => ""} />
                  <Scatter data={plot}>
                    {plot.map((e) => (
                      <Cell key={e.label} fill={e.fdr <= SIGNIFICANT_FDR ? "var(--accent)" : "var(--viz-2)"} fillOpacity={0.85} />
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
                  <tr key={r.label}>
                    <td className="cell-strong">{r.label}</td>
                    <td className="dt-right cell-mono">{r.pval.toExponential(1)}</td>
                    <td className="dt-right cell-mono">{r.fdr.toFixed(4)}</td>
                    <td className="dt-right cell-mono">{r.overlap}/{r.geneset}</td>
                  </tr>
                ))}
              </tbody>
            </table>
            <div className="wizard-nav wizard-nav-padded">
              <button className="btn btn-ghost" onClick={() => { setStep(0); setResult(null); }}>
                <RotateCcw size={15} /> Start over
              </button>
            </div>
          </Card>
        </>
      )}
    </div>
  );
}
