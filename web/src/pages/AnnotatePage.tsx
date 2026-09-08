import { useEffect, useMemo, useState } from "react";
import { Play, RotateCcw, Loader2, CheckCircle2, CircleDashed, ShoppingBasket, AlertTriangle, Download } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Stepper from "../components/Stepper";
import DataTable, { type Column } from "../components/DataTable";
import EnrichmentDotPlot from "../components/EnrichmentDotPlot";
import SignatureResultRow from "../components/SignatureResultRow";
import {
  searchSignatures,
  getMsigdbSpecies,
  getMsigdbCollections,
  fetchGenesets,
  runAnnotation,
  downloadDotplot,
  type SignatureSummary,
  type MsigdbCollectionOption,
  type GenesetsReadiness,
  type EnrichmentRun,
  type EnrichmentTest,
} from "../api/client";
import { useBasket } from "../basket";

const STEPS = ["Setup", "Results"];

export default function AnnotatePage() {
  const [step, setStep] = useState(0);

  const [signatures, setSignatures] = useState<SignatureSummary[]>([]);
  const [signaturesLoading, setSignaturesLoading] = useState(true);
  const [signaturesError, setSignaturesError] = useState<string | null>(null);
  const [selectedHashkeys, setSelectedHashkeys] = useState<Set<string>>(new Set());

  useEffect(() => {
    searchSignatures({ limit: 500 })
      .then((results) => {
        setSignatures(results);
        if (results.length > 0) setSelectedHashkeys(new Set([results[0].signature_hashkey]));
      })
      .catch((err) => setSignaturesError(err instanceof Error ? err.message : "Could not load signatures."))
      .finally(() => setSignaturesLoading(false));
  }, []);

  const selectedSignatures = useMemo(
    () => signatures.filter((s) => selectedHashkeys.has(s.signature_hashkey)),
    [signatures, selectedHashkeys]
  );

  function toggleRow(row: SignatureSummary) {
    setSelectedHashkeys((prev) => {
      const next = new Set(prev);
      if (next.has(row.signature_hashkey)) next.delete(row.signature_hashkey);
      else next.add(row.signature_hashkey);
      return next;
    });
  }

  function toggleAll(rows: SignatureSummary[], checked: boolean) {
    setSelectedHashkeys((prev) => {
      const next = new Set(prev);
      for (const row of rows) {
        if (checked) next.add(row.signature_hashkey);
        else next.delete(row.signature_hashkey);
      }
      return next;
    });
  }

  const basket = useBasket();
  function handleAddFromBasket() {
    setSelectedHashkeys((prev) => {
      const next = new Set(prev);
      for (const item of basket) next.add(item.signature_hashkey);
      return next;
    });
  }

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

  const [test, setTest] = useState<EnrichmentTest>("hypergeometric");
  const [fdr, setFdr] = useState(0.05);
  // GEM only: directional metabolic models tell a reaction's substrates from
  // its products, so a metabolite maps to different enzymes depending on
  // which side it sits on.
  const [gemDirectional, setGemDirectional] = useState(true);

  // Rank-based enrichment needs per-feature scores from difexp, which not
  // every signature has stored. The option stays enabled as long as *any*
  // selected signature qualifies -- the backend skips the rest and reports
  // them back as `skipped`, so a basket mixing eligible and ineligible
  // signatures still runs on the ones that qualify instead of hard-blocking.
  const kstestEligibleCount = selectedSignatures.filter((s) => s.has_difexp === 1).length;
  const kstestAvailable = kstestEligibleCount > 0;

  // hypeR-GEM maps metabolites onto the genes whose enzymes act on them, so it
  // only means anything for a metabolomics signature -- a transcriptomics one
  // is already genes and there is nothing to map. It also runs one signature
  // at a time, unlike the hypeR tests.
  const isGem = test === "gem_hypergeo" || test === "gem_weighted";
  const metabolomicsCount = selectedSignatures.filter((s) => s.assay_type === "metabolomics").length;
  const gemAvailable = metabolomicsCount > 0;

  useEffect(() => {
    if (!kstestAvailable && (test === "kstest" || test === "gsea")) setTest("hypergeometric");
    if (!gemAvailable && (test === "gem_hypergeo" || test === "gem_weighted")) setTest("hypergeometric");
  }, [kstestAvailable, gemAvailable, test]);

  const [running, setRunning] = useState(false);
  const [openSignatureLabel, setOpenSignatureLabel] = useState<string | null>(null);
  const [runError, setRunError] = useState<string | null>(null);
  const [result, setResult] = useState<EnrichmentRun | null>(null);

  const [downloadingPlot, setDownloadingPlot] = useState(false);
  const [downloadError, setDownloadError] = useState<string | null>(null);

  async function handleRun() {
    if (selectedHashkeys.size === 0 || !genesetStatus) return;
    setRunning(true);
    setRunError(null);
    try {
      // GEM runs a single signature, so send the first metabolomics one
      // rather than the whole basket -- the server would ignore the rest
      // silently otherwise.
      const hashkeys = isGem
        ? [(selectedSignatures.find((s) => s.assay_type === "metabolomics") ?? selectedSignatures[0]).signature_hashkey]
        : Array.from(selectedHashkeys);
      const run = await runAnnotation({
        signatureHashkeys: hashkeys,
        test,
        species,
        collection,
        subcollection: subcollection || undefined,
        fdr,
        gemDirectional,
      });
      setResult(run);
      setOpenSignatureLabel(run.signatures[0]?.label ?? null);
      setStep(1);
    } catch (err) {
      setRunError(err instanceof Error ? err.message : "Enrichment failed.");
    } finally {
      setRunning(false);
    }
  }

  // Re-runs the enrichment server-side to render the figure, so it can take
  // a couple of seconds -- fetched via downloadDotplot (blob + synthetic
  // click) rather than a plain <a href>, which would put the API credential
  // in the DOM.
  async function handleDownloadPlot() {
    if (!result) return;
    setDownloadingPlot(true);
    setDownloadError(null);
    try {
      await downloadDotplot({
        signatureHashkeys: result.signatures.map((s) => s.signature_hashkey),
        test: result.test,
        species,
        collection: result.collection,
        subcollection: result.subcollection,
        fdr: result.fdr,
      });
    } catch (err) {
      setDownloadError(err instanceof Error ? err.message : "Could not download plot.");
    } finally {
      setDownloadingPlot(false);
    }
  }

  const columns: Column<SignatureSummary>[] = useMemo(
    () => [
      { key: "signature_name", label: "Signature", render: (r) => <span className="cell-strong">{r.signature_name}</span> },
      { key: "organism", label: "Organism", filterable: true, render: (r) => <span className="cell-italic">{r.organism ?? "—"}</span> },
      { key: "assay_type", label: "Assay", filterable: true, render: (r) => <Badge tone="neutral">{r.assay_type}</Badge> },
      { key: "phenotype", label: "Phenotype", filterable: true, render: (r) => r.phenotype ?? "—" },
      { key: "feature_count", label: "Features", align: "right" },
      {
        key: "has_difexp",
        label: "Has Difexp",
        render: (r) => <Badge tone={r.has_difexp === 1 ? "success" : "neutral"}>{r.has_difexp === 1 ? "Yes" : "No"}</Badge>,
      },
    ],
    []
  );

  const totalEnriched = (result?.signatures ?? []).reduce((n, s) => n + (s.n_enriched ?? 0), 0);
  // Reflects the run that produced the table on screen, not the method
  // currently chosen in the form -- those diverge as soon as the user changes
  // the selector without re-running.
  const isGemResult = result?.test === "gem_hypergeo" || result?.test === "gem_weighted";

  return (
    <div className="page">
      <PageHeader title="Annotate" subtitle="Gene set enrichment analysis against MSigDB, powered by hypeR." />

      <Card>
        <Stepper steps={STEPS} current={step} />
      </Card>

      {step === 0 && (
        <>
          <div className="annotate-setup">
            <Card
              title="Signatures"
              subtitle="Choose one or more signatures to run enrichment against — hypeR runs them together in a single call."
              className="annotate-signatures"
              actions={
                basket.length > 0 && (
                  <button className="btn btn-secondary btn-sm" onClick={handleAddFromBasket}>
                    <ShoppingBasket size={14} /> Add from Basket ({basket.length})
                  </button>
                )
              }
            >
              {signaturesError && <p className="login-error">{signaturesError}</p>}
              <p className="cell-sub" style={{ marginBottom: 10 }}>
                {selectedHashkeys.size} of {signatures.length} selected
              </p>
              <DataTable
                columns={columns}
                rows={signatures}
                rowKey="signature_hashkey"
                selectable
                selectedKeys={selectedHashkeys}
                onToggleRow={toggleRow}
                onToggleAll={toggleAll}
                scrollable
                maxHeight={560}
                emptyLabel={signaturesLoading ? "Loading signatures…" : "No signatures available."}
              />
            </Card>

            <div className="annotate-config">
              <Card
                title="Method"
                subtitle={
                  selectedSignatures.length === 0
                    ? "Select at least one signature first"
                    : `Configure the run for ${selectedSignatures.length} signature${selectedSignatures.length === 1 ? "" : "s"}`
                }
              >
                <div className="form-grid">
                  <label className="field">
                    <span className="field-label">Method</span>
                    <select className="input" value={test} onChange={(e) => setTest(e.target.value as typeof test)}>
                      <option value="hypergeometric">Hypergeometric — feature overlap</option>
                      <option value="kstest" disabled={!kstestAvailable}>
                        Rank-based (KS test) {!kstestAvailable ? "— requires stored difexp" : ""}
                      </option>
                      {/* GSEA is the KS statistic with hits weighted by score;
                          it has the same stored-difexp requirement. */}
                      <option value="gsea" disabled={!kstestAvailable}>
                        GSEA — weighted, with leading edge {!kstestAvailable ? "— requires stored difexp" : ""}
                      </option>
                      {/* hypeR-GEM. Labels match the legacy Shiny app's
                          "GEM Hypergeometric" / "GEM Weighted". */}
                      <option value="gem_hypergeo" disabled={!gemAvailable}>
                        GEM Hypergeometric — via metabolic model {!gemAvailable ? "— requires a metabolomics signature" : ""}
                      </option>
                      <option value="gem_weighted" disabled={!gemAvailable}>
                        GEM Weighted — via metabolic model {!gemAvailable ? "— requires a metabolomics signature" : ""}
                      </option>
                    </select>
                  </label>
                  {isGem && (
                    <label className="field">
                      <span className="field-label">Metabolic model</span>
                      <select
                        className="input"
                        value={gemDirectional ? "directional" : "undirected"}
                        onChange={(e) => setGemDirectional(e.target.value === "directional")}
                      >
                        <option value="directional">Directional — separate substrates from products</option>
                        <option value="undirected">Undirected — any reaction the metabolite takes part in</option>
                      </select>
                    </label>
                  )}
                  <label className="field field-slider">
                    <span className="field-label">FDR cutoff <span className="field-value">{fdr.toFixed(2)}</span></span>
                    <input type="range" min={0.01} max={0.1} step={0.01} value={fdr} onChange={(e) => setFdr(Number(e.target.value))} />
                  </label>
                </div>
                {isGem && metabolomicsCount > 1 && (
                  <p className="cell-sub" style={{ marginTop: 10, display: "flex", alignItems: "center", gap: 6 }}>
                    <AlertTriangle size={13} /> GEM runs one signature at a time; the first metabolomics signature in your
                    selection will be used.
                  </p>
                )}
                {test === "kstest" && selectedSignatures.length > 0 && kstestEligibleCount < selectedSignatures.length && (
                  <p className="cell-sub" style={{ marginTop: 10, display: "flex", alignItems: "center", gap: 6 }}>
                    <AlertTriangle size={13} /> Only {kstestEligibleCount} of {selectedSignatures.length} selected signatures have stored
                    difexp; the rest will be skipped for this run.
                  </p>
                )}
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
            </div>
          </div>

          {runError && <p className="login-error">{runError}</p>}
          <div className="wizard-nav">
            <button className="btn btn-primary" onClick={handleRun} disabled={running || selectedHashkeys.size === 0 || !genesetStatus}>
              {running ? (
                <>
                  <Loader2 size={15} className="spin" /> Running…
                </>
              ) : (
                <>
                  <Play size={15} /> Run enrichment{!isGem && selectedHashkeys.size > 1 ? ` (${selectedHashkeys.size} signatures)` : ""}
                </>
              )}
            </button>
          </div>
        </>
      )}

      {step === 1 && result && (
        <>
          <Card
            title="Enrichment results"
            subtitle={
              `${result.signatures.length} signature${result.signatures.length === 1 ? "" : "s"} · ${result.collection}${result.subcollection ? ":" + result.subcollection : ""} · ${result.test} · FDR ≤ ${result.fdr.toFixed(2)} · genesets: ${result.geneset_source}` +
              (isGemResult ? ` · ${result.n_metabolites ?? 0} metabolites → ${result.n_genes ?? 0} genes via ${result.reference_key ?? "refmet_name"}` : "")
            }
            actions={
              <button className="btn btn-ghost btn-sm" onClick={() => setStep(0)}>
                <RotateCcw size={14} /> Edit
              </button>
            }
          >
            <div style={{ display: "flex", flexWrap: "wrap", gap: 8, marginBottom: result.skipped.length > 0 ? 14 : 0 }}>
              {result.signatures.map((s) => (
                <span key={s.signature_hashkey} className="badge badge-neutral" title={`${s.n_query} query genes`}>
                  {s.label} · {s.n_query}
                </span>
              ))}
            </div>
            {result.skipped.length > 0 && (
              <p className="cell-sub" style={{ display: "flex", alignItems: "flex-start", gap: 6 }}>
                <AlertTriangle size={13} style={{ marginTop: 2, flexShrink: 0 }} />
                <span>
                  Skipped {result.skipped.length} signature{result.skipped.length === 1 ? "" : "s"}:{" "}
                  {result.skipped.map((s) => s.signature_name ?? s.signature_hashkey).join(", ")}
                </span>
              </p>
            )}
          </Card>

          <Card
            title="Enrichment"
            subtitle={`${totalEnriched} gene set${totalEnriched === 1 ? "" : "s"} across ${result.signatures.length} signature${result.signatures.length === 1 ? "" : "s"}`}
            actions={
              !isGemResult && (
                <button className="btn btn-secondary btn-sm" onClick={handleDownloadPlot} disabled={downloadingPlot}>
                  <Download size={14} /> {downloadingPlot ? "Downloading…" : "Download plot"}
                </button>
              )
            }
          >
            {downloadError && <p className="login-error">{downloadError}</p>}
            {totalEnriched > 0 && <EnrichmentDotPlot signatures={result.signatures} />}
            {totalEnriched === 0 && <p className="muted-note">No gene sets pass the current FDR cutoff.</p>}
          </Card>

          <Card title="Signatures" subtitle="Select a signature to see its enriched gene sets.">
            <div>
              {result.signatures.map((sig) => (
                <SignatureResultRow
                  key={sig.label}
                  signature={sig}
                  expanded={openSignatureLabel === sig.label}
                  onToggle={() => setOpenSignatureLabel(openSignatureLabel === sig.label ? null : sig.label)}
                  isGsea={result.test === "gsea"}
                  species={species}
                  collection={result.collection}
                  subcollection={result.subcollection}
                />
              ))}
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
