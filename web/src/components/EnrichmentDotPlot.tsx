import { useMemo, useState } from "react";
import type { EnrichmentRunSignature } from "../api/client";

// hypeR's dot plot, drawn in the browser rather than fetched as a rendered
// PNG. Three reasons: the server figure was 1114x1611px on a three-signature
// run and dominated the page; an SVG scales to its container so it cannot; and
// hovering a dot can show the numbers that a static image can only encode.
//
// Encoding follows hypeR::hyp_dots() rather than reinventing one, so this
// figure and the downloadable PNG read the same way:
//
//   one signature   x = FDR on a log axis, colour = FDR, size = geneset size
//   many signatures x = signature,         colour = FDR, size = geneset size
//
// hyp_dots' own spec, for reference:
//   aes(x = label, y = significance, color = significance, size = size)
//   scale_y_continuous(trans = log10_trans())        # y is FDR, then coord_flip
//   scale_color_continuous(high = "#114357", low = "#E53935", trans = log10)
//   size_by = "genesets"                             # dot area = geneset size
//   geom_hline(yintercept = 0.05, linetype = "dotted")
//
// Note the axis is FDR ITSELF on a log scale, not -log10(FDR): small values
// are more significant, so the most significant gene sets sit on the LEFT and
// the 0.05 cutoff line falls near the right.

const MARGIN = { top: 12, right: 16, bottom: 40, left: 260 };
const ROW_H = 22;
const MAX_BODY_H = 420;
// hypeR draws geom_hline(yintercept = 0.05, linetype = "dotted") on its FDR
// axis. Same value, same meaning: the conventional significance threshold.
const FDR_CUTOFF_LINE = 0.05;

// Axis label glyphs render at 10px. Row labels (.edp-ylab) use the monospace
// stack, where every glyph really is ~0.6em wide -- measuring the actual
// stack (Menlo) at 10pt confirms ~6.0px/glyph, matching this almost exactly.
const YLAB_CHAR_PX = 6;

// Row labels: truncating to what MARGIN.left can actually hold -- minus the
// tick offset and a little breathing room -- keeps a long pathway name from
// running past the SVG's left edge; the full name is still one hover away
// via a <title>.
const YLAB_MAX_CHARS = Math.max(4, Math.floor((MARGIN.left - 18) / YLAB_CHAR_PX));

// Multi-signature mode's x-axis ticks are signature names -- often 30-40+
// characters, e.g. "Aging_4mosc1_arecoline_vs_PBS_MontiLab2024" -- rendered
// rotated -35deg with text-anchor "end", pivoting at
// (tickX, bodyH + XLAB_Y_OFFSET): the classic D3 rotated-label idiom, which
// anchors the label's *last* character and lets the rest trail away from it.
// For a rotation of theta degrees, an end-anchored label whose rendered
// width is L px has its first character land L*sin(theta) further down the
// page and L*cos(theta) to the left of that anchor. The leftward part is a
// non-issue here (MARGIN.left is 260px), but the downward part has to fit
// inside MARGIN.bottom -- which budgeted only 40 - 16 = 24px below the
// anchor, room for about four rotated characters at any reasonable glyph
// width. That is exactly why every long label rendered as a clipped tail
// ("...Lab2024"): the rest of the string was drawn below the viewBox.
//
// Fix: give multi mode a taller bottom margin, sized in the same units, and
// derive how many characters actually fit in it -- mirroring YLAB_MAX_CHARS
// above, but solved in the opposite direction (there, space was fixed and
// chars were solved for; here, a margin is chosen and chars follow from it).
// Single-signature mode's ticks are short, unrotated numbers that were never
// part of this problem, so MARGIN.bottom (40) is untouched for it.
//
// .edp-xlab has no font-family override, so it inherits the proportional
// --font stack (-apple-system/Segoe UI/Helvetica/Arial), not .edp-ylab's
// monospace one -- glyph width there varies per character instead of being
// fixed. Measuring real signature names (incl. an all-caps rendering, wider
// than the mixed-case names actually stored) against the system UI font at
// 10pt puts every case at 5.6-6.7px/glyph; XLAB_CHAR_PX below is set above
// that whole measured range on purpose, so the character budget it produces
// still leaves slack even for an unusually wide name.
const XLAB_Y_OFFSET = 16;
const XLAB_TILT_DEG = 35;
const XLAB_DESCENDER_PAD = 8; // headroom for glyph descenders past the baseline
const MARGIN_BOTTOM_MULTI = 110;
const XLAB_CHAR_PX = 7.5;
const XLAB_MAX_LABEL_PX =
  (MARGIN_BOTTOM_MULTI - XLAB_Y_OFFSET - XLAB_DESCENDER_PAD) / Math.sin((XLAB_TILT_DEG * Math.PI) / 180);
const XLAB_MAX_CHARS = Math.max(6, Math.floor(XLAB_MAX_LABEL_PX / XLAB_CHAR_PX));

function truncateLabel(label: string, maxChars: number): string {
  if (label.length <= maxChars) return label;
  return label.slice(0, maxChars - 1) + "…";
}

// log10(FDR), the axis hypeR plots on. Only ever called on values that already
// passed isPlottableFdr below -- an FDR of exactly 0 (a common BH underflow)
// or any other non-finite value must never reach this, or it defines the whole
// axis and crushes every genuinely significant dot against one edge.
function log10Fdr(v: number): number {
  return Math.log10(v);
}

function isPlottableFdr(v: number): boolean {
  return Number.isFinite(v) && v > 0;
}

// hypeR sizes dots by GENESET SIZE (size_by = "genesets"), not by overlap.
function radius(genesetSize: number, maxGeneset: number, rowH: number): number {
  // Dot size must not exceed the row itself: above ~19 rows, MAX_BODY_H
  // shrinks rowH below ROW_H (the default topN=25 already crosses that
  // line), so the whole 2.5-10 size range scales down by the same fraction
  // rowH has shrunk. At rowH == ROW_H, scale == 1 and sizing is exactly what
  // it was before this fix.
  const scale = Math.min(1, rowH / ROW_H);
  if (!Number.isFinite(genesetSize) || genesetSize <= 0) return 2.5 * scale;
  const t = Math.sqrt(genesetSize) / Math.sqrt(Math.max(maxGeneset, 1));
  return (3 + t * 7) * scale;
}

// hypeR's exact ramp: low (most significant) #E53935 red -> high (least)
// #114357 dark teal. `t` is 0 at the most significant end, 1 at the least.
const FDR_LOW = { r: 229, g: 57, b: 53 };   // #E53935
const FDR_HIGH = { r: 17, g: 67, b: 87 };   // #114357
function heat(t: number): string {
  const c = Math.max(0, Math.min(1, t));
  const r = Math.round(FDR_LOW.r + c * (FDR_HIGH.r - FDR_LOW.r));
  const g = Math.round(FDR_LOW.g + c * (FDR_HIGH.g - FDR_LOW.g));
  const b = Math.round(FDR_LOW.b + c * (FDR_HIGH.b - FDR_LOW.b));
  return `rgb(${r},${g},${b})`;
}

interface Dot {
  key: string;
  label: string;
  signature: string;
  x: number;
  y: number;
  r: number;
  // undefined in single-signature mode -- colour comes from the
  // Always set now: colour carries FDR in both layouts, as hypeR does.
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
  // Only the hovered dot's key is kept in state. The dot itself is looked up
  // from the current model on every render (below) rather than cached here,
  // so a `signatures` change that drops or replaces the hovered gene set --
  // browsers do not reliably fire mouseleave when the hovered node is
  // replaced by a re-render -- cannot leave a stale tooltip on screen: a key
  // that no longer resolves simply renders nothing.
  const [hoverKey, setHoverKey] = useState<string | null>(null);
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
    const maxGeneset = Math.max(1, ...allRows.map((r) => r.geneset ?? 0));

    // The axis scale comes from genuine signal only. A BH FDR that
    // underflows to exactly 0 (or is otherwise non-finite) must not get a
    // say in it, or that one row silently crushes every real dot toward the
    // origin. Rows that fail isPlottableFdr are instead pinned to whatever
    // maximum the real data established, once per-dot below. The floor of 1
    // keeps the axis -- and an all-zero-FDR run, which has no plottable rows
    // at all -- well-defined rather than dividing by zero.
    // Plot width is unitless: the viewBox scales to the container.
    const bodyW = 520;
    const colW = multi ? bodyW / signatures.length : bodyW;

    const finiteLog = allRows.map((r) => r.fdr).filter(isPlottableFdr).map(log10Fdr);
    // The axis spans the real data's log10(FDR) range. loMin is the most
    // significant end (most negative), hiMax the least. A degenerate range
    // (one distinct value, or none) is widened so dots do not all land on a
    // single x.
    const rawLo = finiteLog.length > 0 ? Math.min(...finiteLog) : -2;
    const rawHi = finiteLog.length > 0 ? Math.max(...finiteLog) : 0;
    // Always include the 0.05 cutoff line so it is visible on the axis.
    const cutoffLog = Math.log10(FDR_CUTOFF_LINE);
    const loMin = Math.min(rawLo, cutoffLog) - 0.25;
    const hiMax = Math.max(rawHi, cutoffLog) + 0.25;
    const span = Math.max(hiMax - loMin, 0.5);
    // FDR ascends left to right, so the most significant sets sit on the left
    // -- hypeR's orientation.
    const xForFdr = (fdr: number) => ((log10Fdr(fdr) - loMin) / span) * bodyW;


    const dots: Dot[] = [];
    shown.forEach((entry, sigIdx) => {
      // Indexed rather than a plain for..of: a GEM run's group/direction
      // splits (see SignatureResultRow's "Split" column) all share one
      // entry.sig.label, so more than one row can carry the same gene-set
      // label within a single entry (e.g. the same set hit on both the
      // "up" and "dn" split). Without the row index, those rows produced
      // identical keys -- duplicate React keys, dots silently sharing a y
      // band, and the tooltip's `model.dots.find(d => d.key === hoverKey)`
      // resolving to whichever of them React kept.
      entry.rows.forEach((row, rowIdx) => {
        const yi = rowIndex.get(row.label)!;
        // A non-plottable FDR (exactly 0, or non-finite) is pinned to the most
        // significant end rather than being allowed to define the axis.
        const t = isPlottableFdr(row.fdr) ? (log10Fdr(row.fdr) - loMin) / span : 0;
        dots.push({
          key: `${entry.sig.label}::${row.label}::${rowIdx}`,
          label: row.label,
          signature: entry.sig.signature_name,
          x: multi ? colW * (sigIdx + 0.5) : (isPlottableFdr(row.fdr) ? xForFdr(row.fdr) : 0),
          y: rowH * (yi + 0.5),
          r: radius(row.geneset ?? 0, maxGeneset, rowH),
          // Colour carries FDR in both layouts now, as hypeR does -- it is the
          // only channel available when x holds the signature, and hypeR uses
          // it in the single-signature case too.
          fill: heat(t),
          fdr: row.fdr,
          pval: row.pval,
          overlap: row.overlap,
          geneset: row.geneset,
          hits: row.hits ?? "",
        });
      });
    });

    // Top-N is a global cut across every signature's results, so one
    // signature with many strong hits can fill every slot and leave
    // another's column with zero dots even though that signature's own
    // results are non-empty -- it just didn't rank against the rest. Flag
    // those columns explicitly so an empty column reads as "outranked", not
    // "no results".
    const crowdedOut = multi
      ? shown
          .map((entry, i) => ({ entry, i, total: (entry.sig.results ?? []).length }))
          .filter(({ entry, total }) => entry.rows.length === 0 && total > 0)
          .map(({ entry, i, total }) => ({
            x: colW * (i + 0.5),
            signatureName: entry.sig.signature_name,
            total,
          }))
      : [];

    // Ticks at whole powers of ten across the range, labelled as FDR values
    // (1e-34, 1e-21, ...) the way hypeR's axis is.
    const decadeTicks: { x: number; text: string }[] = [];
    const firstDecade = Math.ceil(loMin);
    const lastDecade = Math.floor(hiMax);
    const decadeCount = lastDecade - firstDecade + 1;
    // Thin the ticks so long labels cannot overprint on a 520-unit axis.
    const stride = Math.max(1, Math.ceil(decadeCount / 6));
    for (let d = firstDecade; d <= lastDecade; d += stride) {
      decadeTicks.push({
        x: ((d - loMin) / span) * bodyW,
        text: d === 0 ? "1" : `1e${d}`,
      });
    }

    const ticks = multi
      ? signatures.map((s, i) => ({ x: colW * (i + 0.5), text: s.signature_name }))
      : decadeTicks;

    // hypeR draws a dotted line at FDR = 0.05. Only meaningful on the FDR
    // axis, so single-signature layout only.
    const cutoffX = multi ? null : ((cutoffLog - loMin) / span) * bodyW;

    return { labels, rowH, bodyW, bodyH, dots, ticks, maxGeneset, crowdedOut, cutoffX, loMin, span };
  }, [signatures, topN, multi]);

  if (!model) {
    return <p className="muted-note">No gene sets pass the current FDR cutoff.</p>;
  }

  const hover = hoverKey ? model.dots.find((d) => d.key === hoverKey) ?? null : null;

  const w = MARGIN.left + model.bodyW + MARGIN.right;
  const h = MARGIN.top + model.bodyH + (multi ? MARGIN_BOTTOM_MULTI : MARGIN.bottom);

  return (
    <div className="edp">
      <svg className="edp-svg" viewBox={`0 0 ${w} ${h}`} role="img"
           aria-label="Enrichment dot plot: gene sets by significance">
        <g transform={`translate(${MARGIN.left},${MARGIN.top})`}>
          {model.labels.map((label, i) => {
            const truncated = truncateLabel(label, YLAB_MAX_CHARS);
            return (
              <g key={label}>
                <rect x={0} y={model.rowH * i} width={model.bodyW} height={model.rowH}
                      className={i % 2 === 0 ? "edp-band" : "edp-band edp-band-alt"} />
                <text x={-10} y={model.rowH * (i + 0.5)} className="edp-ylab"
                      dominantBaseline="middle" textAnchor="end">
                  {truncated !== label && <title>{label}</title>}
                  {truncated}
                </text>
              </g>
            );
          })}
          {model.ticks.map((t, i) => {
            const text = multi ? truncateLabel(t.text, XLAB_MAX_CHARS) : t.text;
            const y = model.bodyH + XLAB_Y_OFFSET;
            return (
              <text key={i} x={t.x} y={y} className="edp-xlab"
                    textAnchor={multi ? "end" : "middle"}
                    transform={multi ? `rotate(-${XLAB_TILT_DEG} ${t.x} ${y})` : undefined}>
                {multi && text !== t.text && <title>{t.text}</title>}
                {text}
              </text>
            );
          })}
          {!multi && (
            <text x={model.bodyW / 2} y={model.bodyH + 34} className="edp-axis-title"
                  textAnchor="middle">FDR</text>
          )}
          {model.crowdedOut.map((c) => (
            <text key={`empty-${c.x}`} x={c.x} y={model.bodyH / 2} className="edp-empty-note"
                  textAnchor="middle" dominantBaseline="middle">
              <title>{`${c.signatureName}: ${c.total} result${c.total === 1 ? "" : "s"} below the FDR cutoff, none in the shared top ${topN} gene sets shown here`}</title>
              outranked
            </text>
          ))}
          {/* hypeR's dotted significance line at FDR = 0.05. */}
          {model.cutoffX != null && (
            <line x1={model.cutoffX} x2={model.cutoffX} y1={0} y2={model.bodyH} className="edp-cutoff" />
          )}
          {model.dots.map((d) => (
            <circle key={d.key} cx={d.x} cy={d.y} r={d.r} fill={d.fill}
                    className="edp-dot"
                    onMouseEnter={() => setHoverKey(d.key)} onMouseLeave={() => setHoverKey(null)} />
          ))}
        </g>
      </svg>
      <div className="edp-legend">
        <span className="edp-legend-item">
          <span className="edp-legend-swatch" aria-hidden="true" /> Colour = FDR (red = more significant)
        </span>
        <span className="edp-legend-item">Dot size = geneset size (max {model.maxGeneset})</span>
        {!multi && <span className="edp-legend-item edp-legend-cutoff">Dotted line = FDR 0.05</span>}
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
