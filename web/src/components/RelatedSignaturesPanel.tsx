import { useState } from "react";
import { Network, ArrowRight } from "lucide-react";
import { useNavigate } from "react-router-dom";
import Card from "./Card";
import { SkeletonRows } from "./Skeleton";
import { searchSignaturesByGenes, ApiError, type GeneSearchResult } from "../api/client";

// Signatures that share genes with this one. Lazy on purpose, like the
// Rummagene panel: resolving this signature's symbols and scanning the feature
// set is real work, and most visits to a detail page do not want it.
export default function RelatedSignaturesPanel({ signatureHashkey }: { signatureHashkey: string }) {
  const navigate = useNavigate();
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState<GeneSearchResult | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [started, setStarted] = useState(false);

  async function run() {
    setStarted(true);
    setLoading(true);
    setError(null);
    try {
      setResult(await searchSignaturesByGenes({ signatureHashkey, limit: 10 }));
    } catch (err) {
      setError(err instanceof ApiError ? err.message : "Could not reach the server.");
    } finally {
      setLoading(false);
    }
  }

  return (
    <Card className="rel-card">
      <div className="rel-head">
        <div className="rel-title">
          <Network size={16} />
          <div>
            <h4 className="detail-section-title" style={{ margin: 0 }}>Related signatures</h4>
            <span className="cell-sub">Other signatures that share genes with this one.</span>
          </div>
        </div>
        {started && !loading && (
          <button className="btn btn-ghost btn-sm" onClick={run}>Refresh</button>
        )}
      </div>

      {!started && (
        <div className="rel-cta">
          <button className="btn btn-primary" onClick={run}>Find related</button>
          <span className="cell-sub">Matches on gene overlap, ranked by Jaccard similarity.</span>
        </div>
      )}

      {loading && <SkeletonRows rows={4} />}
      {error && <div className="empty-state">{error}</div>}

      {!loading && !error && result && (
        <>
          <div className="rel-summary cell-sub">
            {result.total === 0
              ? `No other signature shares any of these ${result.query_size} genes.`
              : `${result.total} signature${result.total === 1 ? "" : "s"} share genes with these ${result.query_size}.`}
          </div>

          {result.total > 0 && (
            <ul className="rel-list">
              {result.hits.map((h) => (
                <li className="rel-item" key={h.signature_hashkey}>
                  <button
                    className="rel-item-main"
                    onClick={() => navigate(`/signatures/${h.signature_hashkey}`)}
                  >
                    <span className="rel-name">{h.signature_name}</span>
                    <span className="cell-sub">
                      {[h.organism, h.phenotype, h.assay_type].filter(Boolean).join(" · ")}
                    </span>
                    {h.matched_genes && (
                      <span className="rel-genes" title={h.matched_genes}>{h.matched_genes}</span>
                    )}
                  </button>
                  <div className="rel-metrics">
                    {/* Overlap is the count people reason about; Jaccard is what
                        the ordering actually uses, so both are shown. */}
                    <span className="rel-metric">
                      <b>{h.n_overlap}</b>
                      <span className="cell-sub">shared</span>
                    </span>
                    <span className="rel-metric">
                      <b>{h.jaccard.toFixed(3)}</b>
                      <span className="cell-sub">jaccard</span>
                    </span>
                    <ArrowRight size={14} className="rel-chevron" />
                  </div>
                </li>
              ))}
            </ul>
          )}
        </>
      )}
    </Card>
  );
}
