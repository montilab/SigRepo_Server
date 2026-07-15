import { useEffect, useMemo, useState } from "react";
import { ArrowLeft, ArrowRight, Play, RotateCcw, Loader2, CheckCircle2, CircleDashed } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Stepper from "../components/Stepper";
import {
  searchSignatures,
  getMsigdbSpecies,
  getMsigdbCollections,
  fetchGenesets,
  runAnnotation,
  type SignatureSummary,
  type MsigdbCollectionOption,
  type GenesetsReadiness,
  type EnrichmentRun,
} from "../api/client";

const STEPS = ["Signature", "Method", "Results"];

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

  // ---------- Geneset picking (species -> collection -> subcollection -> Fetch Genesets), mirroring the Shiny app ----------

  const [speciesOptions, setSpeciesOptions] = useState<string[]>([]);
  useEffect(() => {
    getMsigdbSpecies().then(setSpeciesOptions).catch(() => setSpeciesOptions(["Homo sapiens", "Mus musculus"]));
  }, []);
  const [species, setSpecies] = useState("Homo sapiens");

  const [collections, setCollections] = useState<MsigdbCollectionOption[]>([]);
  const [collectionsLoading, setCollectionsLoading] = useState(true);
  const [collectionsError, setCollectionsError] = useState<string | null>(null);

  useEffect(() => {
    getMsigdbCollections()
      .then(setCollections)
      .catch((err) => setCollectionsError(err instanceof Error ? err.message : "Could not load MSigDB collections."))
      .finally(() => setCollectionsLoading(false));
  }, []);

  const collectionOptions = useMemo(() => {
    const seen = new Map<string, string>();
    for (const c of collections) if (!seen.has(c.collection)) seen.set(c.collection, c.collection_label);
    return Array.from(seen.entries());
  }, [collections]);
  const [collection, setCollection] = useState("H");
  const subcollectionOptions = useMemo(
    () => collections.filter((c) => c.collection === collection).map((c) => c.subcollection).filter(Boolean).sort(),
    [collections, collection]
  );
  const [subcollection, setSubcollection] = useState("");

  const [genesetStatus, setGenesetStatus] = useState<GenesetsReadiness | null>(null);
  const [genesetFetching, setGenesetFetching] = useState(false);
  const [genesetError, setGenesetError] = useState<string | null>(null);

  // Any change to the picker invalidates the previous "Fetch Genesets" result.
  useEffect(() => {
    setSubcollection("");
  }, [collection]);
  useEffect(() => {
    setGenesetStatus(null);
    setGenesetError(null);
  }, [species, collection, subcollection]);

  async function handleFetchGenesets() {
    setGenesetFetching(true);
    setGenesetError(null);
    try {
      const readiness = await fetchGenesets({ species, collection, subcollection: subcollection || undefined });
      setGenesetStatus(readiness);
    } catch (err) {
      setGenesetError(err instanceof Error ? err.message : "Could not fetch gene sets.");
    } finally {
      setGenesetFetching(false);
    }
  }

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
    if (!sig || !genesetStatus) return;
    setRunning(true);
    setRunError(null);
    try {
      const run = await runAnnotation({
        signatureHashkey: sig.signature_hashkey,
        test,
        species,
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
        <>
          <Card title="Method" subtitle={`Configure the run for ${sig.signature_name}`}>
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
              <label className="field field-slider">
                <span className="field-label">FDR cutoff <span className="field-value">{fdr.toFixed(2)}</span></span>
                <input type="range" min={0.01} max={0.1} step={0.01} value={fdr} onChange={(e) => setFdr(Number(e.target.value))} />
              </label>
            </div>
          </Card>

          <Card title="Geneset selection" subtitle="Choose a species, collection, and subcollection, then fetch gene sets.">
            <div className="form-grid">
              <label className="field">
                <span className="field-label">Species</span>
                <select className="input" value={species} onChange={(e) => setSpecies(e.target.value)}>
                  {(speciesOptions.length > 0 ? speciesOptions : ["Homo sapiens"]).map((s) => (
                    <option key={s} value={s}>{s}</option>
                  ))}
                </select>
              </label>
              <label className="field">
                <span className="field-label">Collection</span>
                <select className="input" value={collection} onChange={(e) => setCollection(e.target.value)} disabled={collectionsLoading}>
                  {collectionOptions.map(([value, label]) => (
                    <option key={value} value={value}>{label}</option>
                  ))}
                </select>
              </label>
              <label className="field">
                <span className="field-label">Subcollection</span>
                <select className="input" value={subcollection} onChange={(e) => setSubcollection(e.target.value)} disabled={subcollectionOptions.length === 0}>
                  <option value="">{subcollectionOptions.length === 0 ? "No subcollection available" : "All"}</option>
                  {subcollectionOptions.map((s) => (
                    <option key={s} value={s}>{s}</option>
                  ))}
                </select>
              </label>
            </div>
            {collectionsError && <p className="login-error">{collectionsError}</p>}
            <div className="wizard-nav" style={{ justifyContent: "flex-start", gap: 16 }}>
              <button className="btn btn-secondary" onClick={handleFetchGenesets} disabled={genesetFetching}>
                {genesetFetching ? (
                  <>
                    <Loader2 size={15} className="spin" /> Fetching…
                  </>
                ) : (
                  "Fetch Genesets"
                )}
              </button>
              {genesetStatus ? (
                <span className="badge badge-success" style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
                  <CheckCircle2 size={14} /> {genesetStatus.n_genesets} genesets ready
                  {genesetStatus.source === "live" ? " (fetched live)" : ""}
                </span>
              ) : (
                <span className="badge badge-neutral" style={{ display: "inline-flex", alignItems: "center", gap: 6 }}>
                  <CircleDashed size={14} /> No genesets fetched
                </span>
              )}
            </div>
            {genesetError && <p className="login-error">{genesetError}</p>}
          </Card>

          {runError && <p className="login-error">{runError}</p>}
          <div className="wizard-nav">
            <button className="btn btn-ghost" onClick={() => setStep(0)} disabled={running}>
              <ArrowLeft size={16} /> Back
            </button>
            <button className="btn btn-primary" onClick={handleRun} disabled={running || !genesetStatus}>
              {running ? (
                <>
                  <Loader2 size={15} className="spin" /> Running…
                </>
              ) : (
                <>
                  <Play size={15} /> Run enrichment
                </>
              )}
            </button>
          </div>
        </>
      )}

      {step === 2 && sig && result && (
        <>
          <Card
            title="Enrichment results"
            subtitle={`${result.signature_name} · ${result.collection}${result.subcollection ? ":" + result.subcollection : ""} · ${result.test} · FDR ≤ ${result.fdr.toFixed(2)} · genesets: ${result.geneset_source}`}
            actions={
              <button className="btn btn-ghost btn-sm" onClick={() => setStep(1)}>
                <RotateCcw size={14} /> Edit
              </button>
            }
          >
            {result.results.length === 0 ? (
              <p className="muted-note">No gene sets pass the current FDR cutoff.</p>
            ) : result.dotplot_png ? (
              <img src={result.dotplot_png} alt="hypeR enrichment dot plot" style={{ maxWidth: "100%", display: "block", margin: "0 auto" }} />
            ) : (
              <p className="muted-note">Dot plot could not be rendered for this run.</p>
            )}
          </Card>

          <Card padded={false}>
            <div className="dt-scroll">
              <table className="dt-table dt-table-flush">
                <thead>
                  <tr>
                    <th>Gene set</th>
                    <th className="dt-right">P-value</th>
                    <th className="dt-right">FDR</th>
                    <th className="dt-right">Overlap</th>
                    <th>Hits</th>
                  </tr>
                </thead>
                <tbody>
                  {result.results.map((r) => (
                    <tr key={r.label}>
                      <td className="cell-strong">{r.label}</td>
                      <td className="dt-right cell-mono">{r.pval.toExponential(1)}</td>
                      <td className="dt-right cell-mono">{r.fdr.toFixed(4)}</td>
                      <td className="dt-right cell-mono">{r.overlap}/{r.geneset}</td>
                      <td className="cell-sub" style={{ maxWidth: 320 }}>{r.hits}</td>
                    </tr>
                  ))}
                </tbody>
              </table>
            </div>
            <div className="wizard-nav wizard-nav-padded">
              <button className="btn btn-ghost" onClick={() => { setStep(0); setResult(null); setGenesetStatus(null); }}>
                <RotateCcw size={15} /> Start over
              </button>
            </div>
          </Card>
        </>
      )}
    </div>
  );
}
