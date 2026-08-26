import { useMemo, useState } from "react";
import type { EnrichmentRunSignature } from "../api/client";

// hypeR's dot plot, drawn in the browser rather than fetched as a rendered
// PNG. Three reasons: the server figure was 1114x1611px on a three-signature
// run and dominated the page; an SVG scales to its container so it cannot; and
// hovering a dot can show the numbers that a static image can only encode.
//
// Encoding differs by run size, on purpose:
//
//   one signature   x = -log10(FDR), size = overlap, no colour
//   many signatures x = signature,   size = overlap, colour = -log10(FDR)
//
// With one signature the x position already encodes significance, so colouring
// by it too is the redundancy behind hypeR's two competing legends. With
// several, x carries the signature instead, so colour has to carry
// significance.

const MARGIN = { top: 12, right: 16, bottom: 40, left: 260 };
const ROW_H = 22;
const MAX_BODY_H = 420;

// -log10, floored so an FDR of 0 does not produce Infinity.
function negLog10(v: number): number {
  if (!Number.isFinite(v) || v <= 0) return 320;
  return -Math.log10(v);
}

function radius(overlap: number, maxOverlap: number): number {
  if (!Number.isFinite(overlap) || overlap <= 0) return 2.5;
  const t = Math.sqrt(overlap) / Math.sqrt(Math.max(maxOverlap, 1));
  return 3 + t * 7;
}

// Single-hue ramp, dark (least significant) to red (most). Matches the
// direction hypeR uses so the two figures read the same way.
function heat(t: number): string {
  const c = Math.max(0, Math.min(1, t));
  const r = Math.round(17 + c * (229 - 17));
  const g = Math.round(67 + c * (57 - 67));
  const b = Math.round(87 + c * (53 - 87));
  return `rgb(${r},${g},${b})`;
}

interface Dot {
  key: string;
  label: string;
  signature: string;
  x: number;
  y: number;
  r: number;
  fill: string;
  fdr: number;
  pval: number;
  overlap: number;
  geneset: number;
  hits: string;
}

export default function EnrichmentDotPlot({
  signatures,
  topN = 25,
}: {
  signatures: EnrichmentRunSignature[];
  topN?: number;
}) {
  const [hover, setHover] = useState<Dot | null>(null);
  const multi = signatures.length > 1;

  const model = useMemo(() => {
    // Rank gene sets by their best FDR across signatures, then keep the top N
    // so a 300-set run cannot grow the figure without bound.
    const best = new Map<string, number>();
    for (const sig of signatures) {
      for (const row of sig.results ?? []) {
        const prev = best.get(row.label);
        if (prev === undefined || row.fdr < prev) best.set(row.label, row.fdr);
      }
    }
    const labels = [...best.entries()].sort((a, b) => a[1] - b[1]).slice(0, topN).map(([l]) => l);
    if (labels.length === 0) return null;

    const rowIndex = new Map(labels.map((l, i) => [l, i]));
    const bodyH = Math.min(MAX_BODY_H, Math.max(labels.length * ROW_H, ROW_H));
    const rowH = bodyH / labels.length;

    const shown = signatures.map((s) => ({
      sig: s,
      rows: (s.results ?? []).filter((r) => rowIndex.has(r.label)),
    }));
    const allRows = shown.flatMap((s) => s.rows);
    const maxOverlap = Math.max(1, ...allRows.map((r) => r.overlap ?? 0));
    const maxNeg = Math.max(1, ...allRows.map((r) => negLog10(r.fdr)));

    // Plot width is unitless: the viewBox scales to the container.
    const bodyW = 520;
    const colW = multi ? bodyW / signatures.length : bodyW;

    const dots: Dot[] = [];
    shown.forEach((entry, sigIdx) => {
      for (const row of entry.rows) {
        const yi = rowIndex.get(row.label)!;
        const neg = negLog10(row.fdr);
        dots.push({
          key: `${entry.sig.label}::${row.label}`,
          label: row.label,
          signature: entry.sig.signature_name,
          x: multi ? colW * (sigIdx + 0.5) : (neg / maxNeg) * bodyW,
          y: rowH * (yi + 0.5),
          r: radius(row.overlap ?? 0, maxOverlap),
          fill: multi ? heat(neg / maxNeg) : "#E53935",
          fdr: row.fdr,
          pval: row.pval,
          overlap: row.overlap,
          geneset: row.geneset,
          hits: row.hits ?? "",
        });
      }
    });

    // Four ticks across the significance axis, in -log10 units -- plain
    // numbers rather than hypeR's 1e-34 style breaks.
    const ticks = multi
      ? signatures.map((s, i) => ({ x: colW * (i + 0.5), text: s.signature_name }))
      : [0, 0.25, 0.5, 0.75, 1].map((f) => ({ x: f * bodyW, text: (f * maxNeg).toFixed(0) }));

    return { labels, rowH, bodyW, bodyH, dots, ticks, maxOverlap };
  }, [signatures, topN, multi]);

  if (!model) {
    return <p className="muted-note">No gene sets pass the current FDR cutoff.</p>;
  }

  const w = MARGIN.left + model.bodyW + MARGIN.right;
  const h = MARGIN.top + model.bodyH + MARGIN.bottom;

  return (
    <div className="edp">
      <svg className="edp-svg" viewBox={`0 0 ${w} ${h}`} role="img"
           aria-label="Enrichment dot plot: gene sets by significance">
        <g transform={`translate(${MARGIN.left},${MARGIN.top})`}>
          {model.labels.map((label, i) => (
            <g key={label}>
              <rect x={0} y={model.rowH * i} width={model.bodyW} height={model.rowH}
                    className={i % 2 === 0 ? "edp-band" : "edp-band edp-band-alt"} />
              <text x={-10} y={model.rowH * (i + 0.5)} className="edp-ylab"
                    dominantBaseline="middle" textAnchor="end">{label}</text>
            </g>
          ))}
          {model.ticks.map((t, i) => (
            <text key={i} x={t.x} y={model.bodyH + 16} className="edp-xlab"
                  textAnchor={multi ? "end" : "middle"}
                  transform={multi ? `rotate(-35 ${t.x} ${model.bodyH + 16})` : undefined}>
              {t.text}
            </text>
          ))}
          {!multi && (
            <text x={model.bodyW / 2} y={model.bodyH + 34} className="edp-axis-title"
                  textAnchor="middle">−log10(FDR)</text>
          )}
          {model.dots.map((d) => (
            <circle key={d.key} cx={d.x} cy={d.y} r={d.r} fill={d.fill} className="edp-dot"
                    onMouseEnter={() => setHover(d)} onMouseLeave={() => setHover(null)} />
          ))}
        </g>
      </svg>
      <div className="edp-legend">
        <span className="edp-legend-item">Dot size = overlap (max {model.maxOverlap})</span>
        {multi && <span className="edp-legend-item">Colour = −log10(FDR)</span>}
      </div>
      {hover && (
        <div className="edp-tip">
          <b>{hover.label}</b>
          <span>{hover.signature}</span>
          <span>FDR {hover.fdr.toExponential(2)} · p {hover.pval.toExponential(2)}</span>
          <span>overlap {hover.overlap}/{hover.geneset}</span>
          <span className="edp-tip-hits">{hover.hits}</span>
        </div>
      )}
    </div>
  );
}
