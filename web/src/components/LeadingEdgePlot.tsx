import { useEffect, useState } from "react";
import { Loader2 } from "lucide-react";
import { fetchLeadingEdge, ApiError, type LeadingEdge } from "../api/client";

// The classic GSEA figure: the running enrichment score across the ranked
// signature, with a tick for every gene in the set and the peak marked.
//
// Drawn as inline SVG rather than fetched as a rendered image so it stays
// crisp, themes with the page, and can be read by the tooltip -- and so the
// server does not have to render a PNG per gene set.
export default function LeadingEdgePlot({
  signatureHashkey,
  genesetLabel,
  species,
  collection,
  subcollection,
}: {
  signatureHashkey: string;
  genesetLabel: string;
  species: string;
  collection: string;
  subcollection?: string;
}) {
  const [data, setData] = useState<LeadingEdge | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [loading, setLoading] = useState(true);

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);
    setData(null);
    fetchLeadingEdge({ signatureHashkey, genesetLabel, species, collection, subcollection })
      .then((d) => { if (!cancelled) setData(d); })
      .catch((err) => { if (!cancelled) setError(err instanceof ApiError ? err.message : "Could not load the enrichment curve."); })
      .finally(() => { if (!cancelled) setLoading(false); });
    return () => { cancelled = true; };
  }, [signatureHashkey, genesetLabel, species, collection, subcollection]);

  if (loading) return <div className="le-loading"><Loader2 size={15} className="spin" /> Building enrichment curve…</div>;
  if (error) return <div className="empty-state">{error}</div>;
  if (!data) return null;

  const W = 720, H = 200, PAD_L = 46, PAD_R = 12, PAD_T = 12;
  const CURVE_H = 130, TICK_TOP = CURVE_H + PAD_T + 10, TICK_H = 26;

  const scores = data.curve.running_score;
  const maxAbs = Math.max(...scores.map(Math.abs), 0.0001);
  const plotW = W - PAD_L - PAD_R;

  const x = (pos: number) => PAD_L + (pos / data.n_total) * plotW;
  // Zero sits mid-height so a negative enrichment reads as clearly as a
  // positive one -- both are meaningful, they just mean opposite ends.
  const y = (v: number) => PAD_T + CURVE_H / 2 - (v / maxAbs) * (CURVE_H / 2 - 6);

  const path = data.curve.position
    .map((p, i) => `${i === 0 ? "M" : "L"}${x(p).toFixed(1)},${y(scores[i]).toFixed(1)}`)
    .join(" ");

  // Hundreds of ticks at one pixel each is a solid block; thin them to keep the
  // density readable while preserving where the hits actually cluster.
  const ticks = data.hit_positions.length > 400
    ? data.hit_positions.filter((_, i) => i % Math.ceil(data.hit_positions.length / 400) === 0)
    : data.hit_positions;

  return (
    <div className="le-plot">
      <div className="le-stats">
        <span><b>{data.es_score.toFixed(3)}</b><span className="cell-sub">enrichment score</span></span>
        <span><b>{data.n_leading}</b><span className="cell-sub">leading edge genes</span></span>
        <span><b>{data.es_index.toLocaleString()}</b><span className="cell-sub">peak rank of {data.n_total.toLocaleString()}</span></span>
        <span className={data.es_direction === "positive" ? "le-dir-pos" : "le-dir-neg"}>
          {data.es_direction === "positive" ? "enriched at the top" : "enriched at the bottom"}
        </span>
      </div>

      <svg viewBox={`0 0 ${W} ${H}`} className="le-svg" role="img"
           aria-label={`Running enrichment curve for ${data.geneset_label}: score ${data.es_score.toFixed(3)}, ${data.n_leading} leading edge genes`}>
        <line x1={PAD_L} y1={y(0)} x2={W - PAD_R} y2={y(0)} className="le-axis" />
        <text x={PAD_L - 8} y={y(maxAbs) + 4} className="le-tick-label" textAnchor="end">{maxAbs.toFixed(2)}</text>
        <text x={PAD_L - 8} y={y(0) + 4} className="le-tick-label" textAnchor="end">0</text>
        <text x={PAD_L - 8} y={y(-maxAbs) + 4} className="le-tick-label" textAnchor="end">{(-maxAbs).toFixed(2)}</text>

        {/* the peak: where the leading edge ends */}
        <line x1={x(data.es_index)} y1={PAD_T} x2={x(data.es_index)} y2={PAD_T + CURVE_H} className="le-peak" />
        <path d={path} className="le-curve" fill="none" />

        {ticks.map((p, i) => (
          <line key={i} x1={x(p)} y1={TICK_TOP} x2={x(p)} y2={TICK_TOP + TICK_H}
                className={p <= data.es_index === (data.es_direction === "positive") ? "le-hit le-hit-leading" : "le-hit"} />
        ))}
        <text x={PAD_L} y={H - 6} className="le-tick-label">rank 1</text>
        <text x={W - PAD_R} y={H - 6} className="le-tick-label" textAnchor="end">{data.n_total.toLocaleString()}</text>
      </svg>

      <details className="le-genes">
        <summary>Leading edge genes ({data.n_leading})</summary>
        <p className="cell-mono le-gene-list">{data.leading_edge_genes.join(", ")}</p>
      </details>
    </div>
  );
}
