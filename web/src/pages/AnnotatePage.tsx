import { useEffect, useMemo, useState } from "react";
import { Play, RotateCcw, Loader2, CheckCircle2, CircleDashed, ShoppingBasket, AlertTriangle } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Stepper from "../components/Stepper";
import DataTable, { type Column } from "../components/DataTable";
import LeadingEdgePlot from "../components/LeadingEdgePlot";
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
  // Which gene set's enrichment curve is open. Only meaningful after a gsea
  // run; cleared whenever new results replace the old ones.
  const [openGeneset, setOpenGeneset] = useState<string | null>(null);
  const [runError, setRunError] = useState<string | null>(null);
  const [result, setResult] = useState<EnrichmentRun | null>(null);

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
      setStep(1);
    } catch (err) {
      setRunError(err instanceof Error ? err.message : "Enrichment failed.");
    } finally {
      setRunning(false);
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

  // DataTable needs a stable per-row key; hypeR's output has none, so derive
  // one. label alone is not unique when several signatures are run together.
  const resultRows = useMemo(
    () =>
      (result?.results ?? []).map((r, i) => ({
        ...r,
        rowId: `${r.signature_label}::${r.label}::${i}`,
      })),
    [result]
  );

  const multiSignature = (result?.signatures.length ?? 0) > 1;
  // Reflects the run that produced the table on screen, not the method
  // currently chosen in the form -- those diverge as soon as the user changes
  // the selector without re-running.
  const isGemResult = result?.test === "gem_hypergeo" || result?.test === "gem_weighted";

  const resultColumns: Column<(typeof resultRows)[number]>[] = useMemo(() => {
    const cols: Column<(typeof resultRows)[number]>[] = [
      {
        key: "label",
        label: "Gene set",
        render: (r) => <span className="cell-strong">{r.label}</span>,
      },
    ];
    if (multiSignature) {
      cols.push({ key: "signature_label", label: "Signature", filterable: true });
    }
    cols.push(
      // FDR first among the numbers: it is what the cutoff is set on and what
      // people actually rank by.
      {
        key: "fdr",
        label: "FDR",
        align: "right",
        render: (r) => <span className="cell-mono enrich-num">{r.fdr < 0.0001 ? r.fdr.toExponential(1) : r.fdr.toFixed(4)}</span>,
      },
      {
        key: "pval",
        label: "P-value",
        align: "right",
        render: (r) => <span className="cell-mono enrich-num">{r.pval.toExponential(1)}</span>,
      },
      // overlap / geneset / signature / background were four separate columns
      // of raw counts. The ratio is the thing being judged, so show that with
      // the counts alongside rather than making the reader divide.
      {
        key: "overlap",
        // GEM's weighted method reports weighted_overlap instead of a plain
        // count, and both GEM methods add the metabolite -> gene step that the
        // hypeR tests do not have.
        label: isGemResult ? (result?.gem_method === "weighted" ? "Weighted overlap" : "Gene overlap") : "Overlap",
        align: "right",
        render: (r) => (
          <span className="enrich-overlap" title={`${r.weighted_overlap ?? r.overlap} of ${r.geneset} genes; query ${r.signature}, background ${r.background}`}>
            <span className="cell-mono">{r.weighted_overlap ?? r.overlap}/{r.geneset}</span>
            <span className="enrich-bar" aria-hidden="true">
              <span className="enrich-bar-fill" style={{ width: `${Math.min(100, (r.overlap / Math.max(1, r.geneset)) * 100)}%` }} />
            </span>
          </span>
        ),
      },
      {
        key: "hits",
        label: isGemResult ? "Gene hits" : "Hits",
        render: (r) => {
          const hits = r.gene_hits ?? r.hits;
          return <span className="cell-sub enrich-hits" title={hits}>{hits}</span>;
        },
      }
    );
    // The metabolites behind the gene hits -- GEM's whole point, and the one
    // thing a reader cannot reconstruct from the gene list.
    if (isGemResult) {
      cols.push({
        key: "metabolite_hits",
        label: "Metabolite hits",
        render: (r) => (
          <span className="cell-sub enrich-hits" title={r.metabolite_hits ?? ""}>
            {r.num_met_hits != null ? `${r.num_met_hits} · ` : ""}{r.metabolite_hits ?? "—"}
          </span>
        ),
      });
    }
    return cols;
  }, [multiSignature, isGemResult, result?.gem_method]);

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

          {/* The dot plot is hypeR's own hyp_dots() output, rendered server-side
              from the hypeR object. hypeR.GEM returns plain tables and no such
              object, so a GEM run has nothing to draw -- drop the card entirely
              rather than reporting an absence-by-design as a failure. */}
          {!isGemResult && (
            <Card>
              {result.results.length === 0 ? (
                <p className="muted-note">No gene sets pass the current FDR cutoff.</p>
              ) : result.dotplot_png ? (
                <img src={result.dotplot_png} alt="hypeR enrichment dot plot" style={{ maxWidth: "100%", display: "block", margin: "0 auto" }} />
              ) : (
                <p className="muted-note">Dot plot could not be rendered for this run.</p>
              )}
            </Card>
          )}
          {isGemResult && result.results.length === 0 && (
            <Card>
              <p className="muted-note">No gene sets pass the current FDR cutoff.</p>
            </Card>
          )}

          <Card
            title="Enriched gene sets"
            subtitle={`${result.results.length} gene set${result.results.length === 1 ? "" : "s"} below the FDR cutoff. Sort by any column, or search by name.`}
          >
            {/* Was a hand-rolled 9-column table with no sort, search or paging,
                and four raw hypeR counts spread across separate columns. Using
                the shared DataTable gives the same affordances as every other
                table in the app, and overlap reads as a proportion of the gene
                set rather than four numbers to reconcile by eye. */}
            <DataTable
              columns={resultColumns}
              rows={resultRows}
              rowKey="rowId"
              pageSize={25}
              emptyLabel="No gene sets pass the current FDR cutoff."
              selectedKey={openGeneset}
              onSelectRow={result.test === "gsea" ? (r) => setOpenGeneset(openGeneset === r.rowId ? null : r.rowId) : undefined}
            />

            {/* The curve only exists for a ranked run -- there is no ranking to
                walk after a hypergeometric one, so it is not offered there. */}
            {result.test === "gsea" &&
              (openGeneset ? (
                <div className="le-wrap">
                  <div className="le-head">
                    <h4 className="detail-section-title" style={{ margin: 0 }}>
                      {resultRows.find((r) => r.rowId === openGeneset)?.label}
                    </h4>
                    <button className="btn btn-ghost btn-sm" onClick={() => setOpenGeneset(null)}>
                      Close
                    </button>
                  </div>
                  <LeadingEdgePlot
                    signatureHashkey={result.signatures[0]?.signature_hashkey ?? ""}
                    genesetLabel={resultRows.find((r) => r.rowId === openGeneset)?.label ?? ""}
                    species={species}
                    collection={collection}
                    subcollection={subcollection}
                  />
                </div>
              ) : (
                <p className="cell-sub le-hint">
                  Select a gene set above to see its running enrichment curve and leading edge.
                </p>
              ))}
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
