import { useState } from "react";
import { BookOpen, ExternalLink, Search } from "lucide-react";
import Card from "./Card";
import { SkeletonRows } from "./Skeleton";
import { rummageneEnrich, ApiError, type RummageneResult } from "../api/client";

// Format a p-value compactly (e.g. 4.0e-24, 0.013).
function fmtP(p: number | null): string {
  if (p == null || Number.isNaN(p)) return "—";
  if (p === 0) return "0";
  return p < 1e-3 ? p.toExponential(1) : p.toFixed(3);
}

// Rummagene terms look like "PMC6819084-elife-47013-supp2.xlsx-IPA_mono_upstream-...".
// Trim the PMCid + file prefix into something readable when there's no title.
function tidyTerm(term: string): string {
  const parts = term.split("-");
  return parts.length > 2 ? parts.slice(2).join(" ").replace(/_/g, " ") : term.replace(/_/g, " ");
}

export default function RummagenePanel({ signatureHashkey }: { signatureHashkey: string }) {
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState<RummageneResult | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [started, setStarted] = useState(false);

  async function run() {
    setStarted(true);
    setLoading(true);
    setError(null);
    try {
      setResult(await rummageneEnrich({ signatureHashkey, limit: 20 }));
    } catch (err) {
      setResult(null);
      setError(err instanceof ApiError ? err.message : "Rummagene lookup failed.");
    } finally {
      setLoading(false);
    }
  }

  return (
    <Card className="rmg-card">
      <div className="rmg-head">
        <div className="rmg-title">
          <BookOpen size={16} />
          <div>
            <h4 className="detail-section-title" style={{ margin: 0 }}>
              Related published gene sets
            </h4>
            <span className="cell-sub">
              Gene sets mined from PMC papers that overlap this signature ·{" "}
              <a href="https://rummagene.com" target="_blank" rel="noreferrer">
                Rummagene
              </a>
            </span>
          </div>
        </div>
        {started && !loading && (
          <button className="btn btn-ghost btn-sm" onClick={run}>
            <Search size={14} /> Refresh
          </button>
        )}
      </div>

      {!started ? (
        <div className="rmg-cta">
          <button className="btn btn-primary" onClick={run}>
            <Search size={15} /> Find related papers
          </button>
          <span className="cell-sub">Enriches this signature's genes against ~1M literature gene sets.</span>
        </div>
      ) : loading ? (
        <SkeletonRows rows={5} />
      ) : error ? (
        <div className="empty-state">{error}</div>
      ) : result && result.hits.length > 0 ? (
        <>
          <div className="rmg-summary cell-sub">
            Top {result.hits.length} of {result.total_count.toLocaleString()} matches for {result.query_size} genes
          </div>
          <ul className="rmg-list">
            {result.hits.map((h, i) => (
              <li className="rmg-item" key={i}>
                <div className="rmg-item-main">
                  {h.pmc_url ? (
                    <a className="rmg-item-title" href={h.pmc_url} target="_blank" rel="noreferrer">
                      {h.title || tidyTerm(h.term)} <ExternalLink size={12} />
                    </a>
                  ) : (
                    <span className="rmg-item-title">{h.title || tidyTerm(h.term)}</span>
                  )}
                  <div className="rmg-item-meta cell-sub">
                    {h.pmcid && <span>{h.pmcid}</span>}
                    {h.year != null && <span>{h.year}</span>}
                    {h.n_sets != null && h.n_sets > 1 && <span>{h.n_sets} papers</span>}
                  </div>
                </div>
                <div className="rmg-item-stats">
                  <span className="rmg-stat" title="Overlapping genes / gene set size">
                    {h.n_overlap ?? "—"}
                    {h.n_geneset != null ? `/${h.n_geneset}` : ""}
                  </span>
                  <span className="rmg-pval" title="Fisher exact p-value">
                    p={fmtP(h.pvalue)}
                  </span>
                </div>
              </li>
            ))}
          </ul>
        </>
      ) : (
        <div className="empty-state">No overlapping published gene sets found.</div>
      )}
    </Card>
  );
}
