import { useEffect, useMemo, useState } from "react";
import { Search, GitCompare, SlidersHorizontal } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import { SkeletonRows } from "../components/Skeleton";
import {
  searchSignatures,
  compareSignatures,
  compareLeadingEdge,
  ApiError,
  type SignatureSummary,
  type CompareResult,
  type CompareMatrix,
  type CompareMeasure,
  type CompareSignatureMeta,
  type LeadingEdgeResult,
} from "../api/client";

const METHODS = [
  { value: "overlap", label: "Overlap — Jaccard / Fisher" },
  { value: "ks_rank", label: "KS — rank position" },
  { value: "ks_score", label: "KS — score distribution" },
  { value: "gsea", label: "GSEA — fgsea (NES)" },
];

const MEASURE_LABEL: Record<CompareMeasure, string> = {
  jaccard: "Jaccard index",
  pvalue: "−log10(p-value)",
  counts: "Overlap size",
  score: "Enrichment score",
};

// How many signatures a single comparison will accept per list. Comfortably
// above any realistic hand-picked comparison; keeps the heatmap legible.
const MAX_SELECTED = 25;

function fmt(v: number | null | undefined): string {
  if (v === null || v === undefined || Number.isNaN(v)) return "—";
  if (Math.abs(v) !== 0 && (Math.abs(v) < 0.001 || Math.abs(v) >= 1e4)) return v.toExponential(1);
  return Number.isInteger(v) ? String(v) : v.toFixed(2);
}

// p-values are plotted as -log10 (vignette: "larger values indicate stronger
// overlap enrichment"), so both the color ramp and the label use the transform.
function displayValue(v: number | null, measure: CompareMeasure): number | null {
  if (v === null || v === undefined || Number.isNaN(v)) return null;
  if (measure === "pvalue") return -Math.log10(Math.max(v, 1e-300));
  return v;
}

// Cell fill, scaled within the matrix. Jaccard is a fixed [0,1] single-hue ramp;
// counts/-log10(p) are sequential scaled to the matrix max; the signed KS/GSEA
// score is a diverging red/teal ramp around zero.
function cellStyle(shown: number | null, measure: CompareMeasure, maxAbs: number): React.CSSProperties {
  if (shown === null) {
    return { background: "var(--surface-sunken)", color: "var(--text-muted)" };
  }
  if (measure === "jaccard" || measure === "counts" || measure === "pvalue") {
    const t =
      measure === "jaccard"
        ? Math.max(0, Math.min(1, shown))
        : maxAbs > 0
          ? Math.max(0, Math.min(1, shown / maxAbs))
          : 0;
    return { background: `rgba(37, 99, 235, ${0.08 + 0.85 * t})`, color: t > 0.5 ? "#fff" : "var(--text)" };
  }
  const t = maxAbs > 0 ? shown / maxAbs : 0;
  const a = 0.12 + 0.82 * Math.min(1, Math.abs(t));
  const strong = Math.abs(t) > 0.55;
  return shown >= 0
    ? { background: `rgba(20, 184, 166, ${a})`, color: strong ? "#fff" : "var(--text)" }
    : { background: `rgba(239, 68, 68, ${a})`, color: strong ? "#fff" : "var(--text)" };
}

function matrixMaxAbs(matrix: CompareMatrix | undefined, measure: CompareMeasure): number {
  if (!matrix) return 0;
  let m = 0;
  for (const row of matrix.values) {
    for (const v of row) {
      const d = displayValue(v, measure);
      if (d !== null && Number.isFinite(d)) m = Math.max(m, Math.abs(d));
    }
  }
  return m;
}

const short = (name: string) => (name.length > 22 ? name.slice(0, 21) + "…" : name);

function Heatmap({
  matrix,
  secondary,
  pvalue,
  measure,
  triangle,
  levelNames,
  onCellClick,
  activeCell,
}: {
  matrix: CompareMatrix;
  // When set, the cell is split diagonally: bottom-left = `matrix` (level 1),
  // top-right = `secondary` (level 2). Mirrors signature_similarity_heatmap()'s
  // mode="combined".
  secondary?: CompareMatrix;
  pvalue?: CompareMatrix;
  measure: CompareMeasure;
  // "upper" hides the redundant lower half of a symmetric self-comparison.
  triangle?: "full" | "upper";
  levelNames?: [string, string];
  onCellClick?: (rowName: string, colName: string) => void;
  activeCell?: { row: string; col: string } | null;
}) {
  const maxAbs = useMemo(() => {
    const a = matrixMaxAbs(matrix, measure);
    const b = secondary ? matrixMaxAbs(secondary, measure) : 0;
    return Math.max(a, b);
  }, [matrix, secondary, measure]);

  return (
    <div className="heatmap-scroll">
      <table className="heatmap">
        <thead>
          <tr>
            <th className="heatmap-corner" />
            {matrix.cols.map((c) => (
              <th key={c} className="heatmap-colhead" title={c}>
                <span>{short(c)}</span>
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {matrix.rows.map((r, i) => (
            <tr key={r}>
              <th className="heatmap-rowhead" title={r}>{short(r)}</th>
              {matrix.cols.map((c, j) => {
                // Upper triangle only applies to a square self-comparison.
                if (triangle === "upper" && !secondary && j < i) {
                  return <td key={c} className="heatmap-cell heatmap-cell-blank" />;
                }
                const raw = matrix.values[i]?.[j] ?? null;
                const v = displayValue(raw, measure);
                const p = pvalue?.values[i]?.[j] ?? null;
                const clickable = !!onCellClick && raw !== null && !(matrix.rows[i] === matrix.cols[j]);
                const isActive = activeCell?.row === r && activeCell?.col === c;

                if (secondary) {
                  const raw2 = secondary.values[i]?.[j] ?? null;
                  const v2 = displayValue(raw2, measure);
                  const l1 = levelNames?.[0] ?? "level 1";
                  const l2 = levelNames?.[1] ?? "level 2";
                  return (
                    <td
                      key={c}
                      className={"heatmap-cell heatmap-cell-split" + (clickable ? " heatmap-cell-clickable" : "") + (isActive ? " heatmap-cell-active" : "")}
                      onClick={clickable ? () => onCellClick!(r, c) : undefined}
                      title={`${r}\n× ${c}\n${l1}: ${fmt(v)}\n${l2}: ${fmt(v2)}`}
                    >
                      <span className="heatmap-tri heatmap-tri-bl" style={cellStyle(v, measure, maxAbs)} />
                      <span className="heatmap-tri heatmap-tri-tr" style={cellStyle(v2, measure, maxAbs)} />
                      <span className="heatmap-split-labels">
                        <span className="heatmap-split-bl">{fmt(v)}</span>
                        <span className="heatmap-split-tr">{fmt(v2)}</span>
                      </span>
                    </td>
                  );
                }

                return (
                  <td
                    key={c}
                    className={"heatmap-cell" + (clickable ? " heatmap-cell-clickable" : "") + (isActive ? " heatmap-cell-active" : "")}
                    style={cellStyle(v, measure, maxAbs)}
                    onClick={clickable ? () => onCellClick!(r, c) : undefined}
                    title={
                      `${r}\n× ${c}\n${MEASURE_LABEL[measure]}: ${fmt(v)}` +
                      (p != null && measure !== "pvalue" ? `\np = ${fmt(p)}` : "") +
                      (clickable ? "\n(click for leading edge)" : "")
                    }
                  >
                    {fmt(v)}
                  </td>
                );
              })}
            </tr>
          ))}
        </tbody>
      </table>
    </div>
  );
}

// "level1_vs_level2" -> { g: 1, r: 2 } (geneset level, ranking level).
function pairingLevels(p: string): { g: number; r: number } {
  const m = p.match(/level(\d+)_vs_level(\d+)/);
  return m ? { g: Number(m[1]), r: Number(m[2]) } : { g: 1, r: 1 };
}

// GSEA enrichment (leading-edge) plot: running enrichment score along the
// ranked list, with geneset hit ticks and the leading-edge region shaded.
// Hand-drawn SVG so the curve, ticks, and shading share one exact rank->x scale.
function EnrichmentPlot({ data }: { data: LeadingEdgeResult }) {
  const W = 680;
  const H = 196;
  const padL = 40;
  const padR = 14;
  const top = 10;
  const curveH = 126;
  const gap = 6;
  const tickH = 20;
  const n = Math.max(1, data.n_ranked);
  const xs = (rank: number) => padL + (rank / n) * (W - padL - padR);

  const esVals = data.curve.map((p) => p.ES);
  const esMin = Math.min(0, ...esVals);
  const esMax = Math.max(0, ...esVals);
  const span = esMax - esMin || 1;
  const ys = (es: number) => top + ((esMax - es) / span) * curveH;
  const zeroY = ys(0);

  let peak = data.curve[0] ?? { rank: 0, ES: 0 };
  for (const p of data.curve) if (Math.abs(p.ES) > Math.abs(peak.ES)) peak = p;
  const posES = (data.ES ?? peak.ES) >= 0;

  const first = data.curve[0]?.rank ?? 0;
  const last = data.curve[data.curve.length - 1]?.rank ?? n;
  const linePts = data.curve.map((p) => `${xs(p.rank)},${ys(p.ES)}`).join(" ");
  const areaPath =
    `M ${xs(first)},${zeroY} ` +
    data.curve.map((p) => `L ${xs(p.rank)},${ys(p.ES)}`).join(" ") +
    ` L ${xs(last)},${zeroY} Z`;

  const leX0 = posES ? padL : xs(peak.rank);
  const leX1 = posES ? xs(peak.rank) : W - padR;
  const tickTop = top + curveH + gap;
  const stroke = posES ? "var(--viz-3)" : "var(--danger)";

  return (
    <div className="enrich-plot">
      <svg viewBox={`0 0 ${W} ${H}`} width="100%" preserveAspectRatio="xMidYMid meet">
        <rect x={leX0} y={top} width={Math.max(0, leX1 - leX0)} height={curveH} fill="var(--accent)" opacity={0.07} />
        <line x1={padL} y1={zeroY} x2={W - padR} y2={zeroY} stroke="var(--border-strong)" strokeDasharray="3 3" />
        <path d={areaPath} fill={posES ? "rgba(20,184,166,0.14)" : "rgba(239,68,68,0.14)"} />
        <polyline points={linePts} fill="none" stroke={stroke} strokeWidth={2} />
        <line x1={xs(peak.rank)} y1={top} x2={xs(peak.rank)} y2={top + curveH} stroke="var(--text-muted)" strokeDasharray="2 3" opacity={0.6} />
        {data.ticks.map((t, i) => (
          <line key={i} x1={xs(t)} y1={tickTop} x2={xs(t)} y2={tickTop + tickH} stroke="var(--text-secondary)" strokeWidth={1} opacity={0.5} />
        ))}
        <text x={padL} y={tickTop + tickH + 13} className="enrich-axis" textAnchor="start">{data.ranking_label} (top)</text>
        <text x={W - padR} y={tickTop + tickH + 13} className="enrich-axis" textAnchor="end">{data.ranking_contrast} (bottom)</text>
        <text x={2} y={top + 8} className="enrich-axis" textAnchor="start">ES</text>
      </svg>
    </div>
  );
}

type Role = "query" | "reference";

export default function ComparePage() {
  const [signatures, setSignatures] = useState<SignatureSummary[]>([]);
  const [listLoading, setListLoading] = useState(true);
  const [listError, setListError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    searchSignatures({ limit: 100 })
      .then((results) => {
        if (!cancelled) setSignatures(results);
      })
      .catch((err) => {
        if (!cancelled) setListError(err instanceof Error ? err.message : "Could not load signatures.");
      })
      .finally(() => {
        if (!cancelled) setListLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, []);

  // "self" compares one set against itself; "two" compares a query set against
  // a reference set (compare_omic_signatures' sig_list2).
  const [compareMode, setCompareMode] = useState<"self" | "two">("self");
  const [selected, setSelected] = useState<Set<string>>(new Set());
  const [reference, setReference] = useState<Set<string>>(new Set());
  const [search, setSearch] = useState("");
  const [method, setMethod] = useState("overlap");
  const [scoreCutoff, setScoreCutoff] = useState(0);
  const [adjPCutoff, setAdjPCutoff] = useState(0.05);
  const [minFeatures, setMinFeatures] = useState(5);
  const [maxFeature, setMaxFeature] = useState(500);
  const [gseaScore, setGseaScore] = useState<"NES" | "ES">("NES");
  const [adjust, setAdjust] = useState(false);
  const [showAdvanced, setShowAdvanced] = useState(false);

  // Explicit level matching, keyed by hashkey: { hk: "treated,control" }.
  const [pairingText, setPairingText] = useState<Record<string, string>>({});

  const [result, setResult] = useState<CompareResult | null>(null);
  const [pairing, setPairing] = useState("");
  const [measure, setMeasure] = useState<CompareMeasure>("jaccard");
  const [heatMode, setHeatMode] = useState<"separate" | "combined">("separate");
  const [triangle, setTriangle] = useState<"full" | "upper">("full");
  const [comparing, setComparing] = useState(false);
  const [error, setError] = useState<string | null>(null);

  // GSEA leading-edge drill-down (populated when a heatmap cell is clicked).
  const [activeCell, setActiveCell] = useState<{ row: string; col: string } | null>(null);
  const [le, setLe] = useState<LeadingEdgeResult | null>(null);
  const [leLoading, setLeLoading] = useState(false);
  const [leError, setLeError] = useState<string | null>(null);

  // Row names come from sig_list1; column names from sig_list2 in a two-list run.
  const metaByName = useMemo(() => {
    const m = new Map<string, CompareSignatureMeta>();
    for (const s of result?.signatures ?? []) m.set(s.name, s);
    for (const s of result?.reference_signatures ?? []) m.set(s.name, s);
    return m;
  }, [result]);

  function invalidate() {
    setResult(null);
    setActiveCell(null);
    setLe(null);
    setLeError(null);
  }

  function clearLeadingEdge() {
    setActiveCell(null);
    setLe(null);
    setLeError(null);
  }

  async function handleCellClick(rowName: string, colName: string) {
    const geneset = metaByName.get(rowName);
    const ranking = metaByName.get(colName);
    if (!geneset || !ranking) return;
    setActiveCell({ row: rowName, col: colName });
    setLe(null);
    setLeError(null);
    setLeLoading(true);
    const { g, r } = pairingLevels(pairing);
    try {
      setLe(
        await compareLeadingEdge({
          geneset_hashkey: geneset.hashkey,
          ranking_hashkey: ranking.hashkey,
          geneset_level: g,
          ranking_level: r,
          score_cutoff: scoreCutoff,
          adj_p_cutoff: adjPCutoff,
          min_features: minFeatures,
        })
      );
    } catch (err) {
      setLeError(err instanceof ApiError ? err.message : "Could not compute the leading edge.");
    } finally {
      setLeLoading(false);
    }
  }

  const filtered = useMemo(() => {
    const q = search.trim().toLowerCase();
    return q ? signatures.filter((s) => s.signature_name.toLowerCase().includes(q)) : signatures;
  }, [signatures, search]);

  function toggleRole(hk: string, role: Role) {
    invalidate();
    const [set, setter] = role === "query" ? [selected, setSelected] : [reference, setReference];
    const other = role === "query" ? reference : selected;
    const otherSetter = role === "query" ? setReference : setSelected;
    const next = new Set(set);
    if (next.has(hk)) next.delete(hk);
    else if (next.size < MAX_SELECTED) {
      next.add(hk);
      // A signature can't be in both lists at once.
      if (other.has(hk)) {
        const o = new Set(other);
        o.delete(hk);
        otherSetter(o);
      }
    }
    setter(next);
  }

  const twoList = compareMode === "two";
  const canCompare = twoList ? selected.size >= 1 && reference.size >= 1 : selected.size >= 2;

  // Signatures whose levels the user can explicitly pair (both lists).
  const pairableHashkeys = useMemo(
    () => [...selected, ...(twoList ? [...reference] : [])],
    [selected, reference, twoList]
  );
  const nameByHashkey = useMemo(() => {
    const m = new Map<string, string>();
    for (const s of signatures) m.set(s.signature_hashkey, s.signature_name);
    return m;
  }, [signatures]);

  function buildPairing(keys: string[]): Record<string, string[]> | undefined {
    const out: Record<string, string[]> = {};
    for (const hk of keys) {
      const levels = (pairingText[hk] ?? "")
        .split(",")
        .map((s) => s.trim())
        .filter(Boolean);
      if (levels.length >= 2) out[hk] = levels;
    }
    return Object.keys(out).length > 0 ? out : undefined;
  }

  async function handleCompare() {
    if (!canCompare) return;
    setComparing(true);
    setError(null);
    try {
      const r = await compareSignatures({
        signature_hashkeys: [...selected],
        reference_hashkeys: twoList ? [...reference] : undefined,
        method,
        score_cutoff: scoreCutoff,
        adj_p_cutoff: adjPCutoff,
        min_features: minFeatures,
        max_feature: maxFeature,
        label_pairing: buildPairing([...selected]),
        label_pairing2: twoList ? buildPairing([...reference]) : undefined,
        adjust,
        gsea_score: gseaScore,
      });
      setResult(r);
      setPairing(r.pairings[0] ?? "");
      setMeasure(r.primary_measure);
      setHeatMode("separate");
      setTriangle("full");
      clearLeadingEdge();
    } catch (err) {
      setError(err instanceof ApiError ? err.message : "Could not compare signatures.");
      setResult(null);
    } finally {
      setComparing(false);
    }
  }

  const activePairing = result && pairing ? result.comparisons[pairing] : null;
  const matrix = activePairing ? activePairing[measure] : undefined;
  const combinable = !!result && result.pairings.length >= 2;
  const secondaryMatrix =
    heatMode === "combined" && result && combinable
      ? result.comparisons[result.pairings[1]]?.[measure]
      : undefined;
  // Symmetric only for a self-comparison shown as a single (non-split) matrix.
  const canTriangle = !!result && !result.two_list && !secondaryMatrix;

  // level names for the combined view, from label_order when available.
  const levelNames = useMemo<[string, string]>(() => {
    const first = result?.label_order?.[0];
    if (first && first.levels.length >= 2) return [first.levels[0], first.levels[1]];
    return ["level 1", "level 2"];
  }, [result]);

  const sizes = activePairing?.sizes ?? [];

  function renderPicker(role: Role) {
    const set = role === "query" ? selected : reference;
    return (
      <div className="compare-picker-list">
        {filtered.length === 0 && <p className="cell-sub" style={{ padding: "12px 14px" }}>No signatures match.</p>}
        {filtered.map((s) => {
          const checked = set.has(s.signature_hashkey);
          return (
            <label key={s.signature_hashkey} className={"compare-picker-item" + (checked ? " compare-picker-item-on" : "")}>
              <input
                type="checkbox"
                checked={checked}
                onChange={() => toggleRole(s.signature_hashkey, role)}
                disabled={!checked && set.size >= MAX_SELECTED}
              />
              <span className="compare-picker-name">
                <span className="cell-strong">{s.signature_name}</span>
                <span className="cell-sub">{s.organism ?? "—"} · {s.assay_type}</span>
              </span>
            </label>
          );
        })}
      </div>
    );
  }

  return (
    <div className="page">
      <PageHeader
        title="Compare"
        subtitle="Compare signatures by feature overlap or rank-based enrichment (KS / GSEA), powered by OmicSignature."
      />

      <div className="compare-layout">
        <div className="compare-side">
          <div className="segmented" style={{ marginBottom: 10 }}>
            <button
              className={"segmented-btn" + (compareMode === "self" ? " segmented-btn-active" : "")}
              onClick={() => { setCompareMode("self"); invalidate(); }}
            >
              One set
            </button>
            <button
              className={"segmented-btn" + (compareMode === "two" ? " segmented-btn-active" : "")}
              onClick={() => { setCompareMode("two"); invalidate(); }}
            >
              Query vs reference
            </button>
          </div>

          <Card
            title={twoList ? "Query signatures" : "Select signatures"}
            subtitle={`${selected.size} selected${selected.size >= MAX_SELECTED ? " (max)" : ""}`}
            padded={false}
          >
            <div className="compare-picker-search">
              <Search size={15} className="toolbar-search-icon" />
              <input
                className="input input-flush"
                placeholder="Filter signatures…"
                value={search}
                onChange={(e) => setSearch(e.target.value)}
              />
            </div>
            {listError && <p className="login-error" style={{ margin: "0 14px 12px" }}>{listError}</p>}
            {listLoading ? <SkeletonRows rows={8} cols={1} /> : renderPicker("query")}
          </Card>

          {twoList && (
            <Card
              title="Reference signatures"
              subtitle={`${reference.size} selected${reference.size >= MAX_SELECTED ? " (max)" : ""}`}
              padded={false}
              className="compare-refcard"
            >
              {listLoading ? <SkeletonRows rows={5} cols={1} /> : renderPicker("reference")}
            </Card>
          )}
        </div>

        <div className="compare-main">
          <Card title="Comparison">
            <div className="compare-controls">
              <label className="field">
                <span className="field-label">Method</span>
                <select className="input" value={method} onChange={(e) => { setMethod(e.target.value); invalidate(); }}>
                  {METHODS.map((m) => (
                    <option key={m.value} value={m.value}>{m.label}</option>
                  ))}
                </select>
              </label>
              <label className="field">
                <span className="field-label">Score cutoff</span>
                <input className="input" type="number" step="0.1" min={0} value={scoreCutoff}
                  onChange={(e) => setScoreCutoff(Number(e.target.value))} />
              </label>
              <label className="field">
                <span className="field-label">Adj. p cutoff</span>
                <input className="input" type="number" step="0.01" min={0} max={1} value={adjPCutoff}
                  onChange={(e) => setAdjPCutoff(Number(e.target.value))} />
              </label>
              <label className="field">
                <span className="field-label">Min features</span>
                <input className="input" type="number" step="1" min={3} value={minFeatures}
                  onChange={(e) => setMinFeatures(Number(e.target.value))} />
              </label>
              <label className="field">
                <span className="field-label">Max features</span>
                <input className="input" type="number" step="50" min={10} value={maxFeature}
                  onChange={(e) => setMaxFeature(Number(e.target.value))} />
              </label>
            </div>

            <button className="btn btn-ghost btn-sm" style={{ marginTop: 10 }} onClick={() => setShowAdvanced((v) => !v)}>
              <SlidersHorizontal size={14} /> {showAdvanced ? "Hide" : "Show"} label pairing & options
            </button>

            {showAdvanced && (
              <div className="compare-advanced">
                <p className="cell-sub" style={{ marginBottom: 8 }}>
                  By default, levels pair by each signature's <code>group_label</code> factor order. If signatures use
                  different label names (e.g. treated/control vs up/down), set them explicitly as
                  <strong> level1, level2</strong>.
                </p>
                {pairableHashkeys.length === 0 && <p className="cell-sub">Select signatures first.</p>}
                {pairableHashkeys.map((hk) => (
                  <label className="field compare-pair-row" key={hk}>
                    <span className="field-label" title={nameByHashkey.get(hk) ?? hk}>
                      {short(nameByHashkey.get(hk) ?? hk)}
                    </span>
                    <input
                      className="input"
                      placeholder="level1, level2"
                      value={pairingText[hk] ?? ""}
                      onChange={(e) => { setPairingText((p) => ({ ...p, [hk]: e.target.value })); invalidate(); }}
                    />
                  </label>
                ))}
                <div className="compare-opt-row">
                  <label className="checkline">
                    <input type="checkbox" checked={adjust} onChange={(e) => { setAdjust(e.target.checked); invalidate(); }} />
                    <span>Adjust p-values (BH)</span>
                  </label>
                  {method === "gsea" && (
                    <label className="field" style={{ minWidth: 140 }}>
                      <span className="field-label">GSEA statistic</span>
                      <select className="input" value={gseaScore} onChange={(e) => { setGseaScore(e.target.value as "NES" | "ES"); invalidate(); }}>
                        <option value="NES">NES</option>
                        <option value="ES">ES</option>
                      </select>
                    </label>
                  )}
                </div>
              </div>
            )}

            <div style={{ display: "flex", alignItems: "center", gap: 12, marginTop: 14 }}>
              <button className="btn btn-primary" disabled={!canCompare || comparing} onClick={handleCompare}>
                <GitCompare size={16} /> {comparing ? "Comparing…" : "Compare"}
              </button>
              {!canCompare && (
                <span className="cell-sub">
                  {twoList ? "Select at least one query and one reference signature." : "Select at least two signatures."}
                </span>
              )}
            </div>
            {method !== "overlap" && (
              <p className="muted-note" style={{ marginTop: 10 }}>
                Rank-based methods (KS, GSEA) rank against each signature's difexp table; signatures without difexp appear as blank (—) columns.
              </p>
            )}
            {error && <p className="login-error" style={{ marginTop: 12 }}>{error}</p>}
          </Card>

          {result && matrix && (
            <Card
              title="Similarity heatmap"
              subtitle={`${MEASURE_LABEL[measure]} · ${matrix.rows.length} × ${matrix.cols.length}${result.two_list ? " · query × reference" : ""}`}
            >
              <div className="compare-view-bar">
                {result.measures.length > 1 && (
                  <label className="field field-inline">
                    <span className="field-label">Measure</span>
                    <select className="input" value={measure} onChange={(e) => setMeasure(e.target.value as CompareMeasure)}>
                      {result.measures.map((m) => (
                        <option key={m} value={m}>{MEASURE_LABEL[m]}</option>
                      ))}
                    </select>
                  </label>
                )}
                {combinable && (
                  <div className="segmented segmented-sm">
                    <button
                      className={"segmented-btn" + (heatMode === "separate" ? " segmented-btn-active" : "")}
                      onClick={() => setHeatMode("separate")}
                    >
                      Separate
                    </button>
                    <button
                      className={"segmented-btn" + (heatMode === "combined" ? " segmented-btn-active" : "")}
                      onClick={() => { setHeatMode("combined"); clearLeadingEdge(); }}
                    >
                      Combined
                    </button>
                  </div>
                )}
                {canTriangle && (
                  <label className="checkline">
                    <input
                      type="checkbox"
                      checked={triangle === "upper"}
                      onChange={(e) => setTriangle(e.target.checked ? "upper" : "full")}
                    />
                    <span>Upper triangle only</span>
                  </label>
                )}
              </div>

              {result.pairings.length > 1 && heatMode === "separate" && (
                <div className="segmented" style={{ marginBottom: 14 }}>
                  {result.pairings.map((p) => (
                    <button
                      key={p}
                      className={"segmented-btn" + (p === pairing ? " segmented-btn-active" : "")}
                      onClick={() => { setPairing(p); clearLeadingEdge(); }}
                    >
                      {p.replace(/_/g, " ")}
                    </button>
                  ))}
                </div>
              )}

              {result.skipped.length > 0 && (
                <p className="muted-note" style={{ marginBottom: 12 }}>
                  {result.skipped.length} selected signature{result.skipped.length === 1 ? "" : "s"} skipped (not visible, missing, or failed to load).
                </p>
              )}

              <Heatmap
                matrix={matrix}
                secondary={secondaryMatrix}
                pvalue={activePairing?.pvalue}
                measure={measure}
                triangle={triangle}
                levelNames={levelNames}
                onCellClick={result.method === "gsea" && !secondaryMatrix ? handleCellClick : undefined}
                activeCell={activeCell}
              />

              <div className="heatmap-legend">
                <Badge tone="neutral">{result.method}</Badge>
                <span className="cell-sub">
                  {secondaryMatrix
                    ? `Each cell is split: bottom-left = ${levelNames[0]}, top-right = ${levelNames[1]}.`
                    : result.method === "gsea"
                      ? "Click a cell (row = geneset, column = ranking) for its leading-edge plot."
                      : "Hover a cell for the exact value and p-value."}
                </span>
              </div>

              {sizes.length > 0 && (
                <div className="compare-sizes">
                  <span className="field-label">Retained feature-set size</span>
                  <div className="pill-list" style={{ justifyContent: "flex-start", marginTop: 6 }}>
                    {sizes.map((s) => (
                      <span className="pill" key={s.name} title={s.name}>{short(s.name)}: {s.size ?? "—"}</span>
                    ))}
                  </div>
                </div>
              )}

              {result.label_order && result.label_order.length > 0 && (
                <details className="compare-levels">
                  <summary className="cell-sub">Level assignment per signature</summary>
                  <table className="dt-table dt-table-compact" style={{ marginTop: 8 }}>
                    <thead>
                      <tr><th>Signature</th><th>Level 1</th><th>Level 2</th>{result.two_list && <th>List</th>}</tr>
                    </thead>
                    <tbody>
                      {result.label_order.map((lo, i) => (
                        <tr key={i}>
                          <td className="cell-strong">{lo.signature}</td>
                          <td>{lo.levels[0] ?? "—"}</td>
                          <td>{lo.levels[1] ?? "—"}</td>
                          {result.two_list && <td className="cell-sub">{lo.list === "sig_list2" ? "reference" : "query"}</td>}
                        </tr>
                      ))}
                    </tbody>
                  </table>
                </details>
              )}
            </Card>
          )}

          {result?.method === "gsea" && (leLoading || le || leError) && (
            <Card
              title="Leading-edge enrichment"
              subtitle={activeCell ? `${activeCell.row}  ▸  ${activeCell.col}` : undefined}
            >
              {leLoading && <SkeletonRows rows={4} cols={1} />}
              {leError && <p className="login-error">{leError}</p>}
              {le && !leLoading && (
                <>
                  <div className="enrich-stats">
                    <div className="enrich-stat">
                      <span className="enrich-stat-label">NES</span>
                      <span className="enrich-stat-value">{fmt(le.NES)}</span>
                    </div>
                    <div className="enrich-stat">
                      <span className="enrich-stat-label">p-value</span>
                      <span className="enrich-stat-value">{fmt(le.pvalue)}</span>
                    </div>
                    <div className="enrich-stat">
                      <span className="enrich-stat-label">Geneset in ranking</span>
                      <span className="enrich-stat-value">{le.n_geneset}</span>
                    </div>
                    <div className="enrich-stat">
                      <span className="enrich-stat-label">Leading edge</span>
                      <span className="enrich-stat-value">{le.leading_edge.length}</span>
                    </div>
                  </div>
                  <p className="cell-sub" style={{ margin: "2px 0 6px" }}>
                    Geneset: <strong>{le.geneset_name}</strong> ({le.geneset_label}) · Ranked by <strong>{le.ranking_name}</strong> ({le.ranking_label} vs {le.ranking_contrast})
                  </p>
                  <EnrichmentPlot data={le} />
                  {le.leading_edge.length > 0 && (
                    <div className="enrich-le">
                      <span className="enrich-stat-label">Leading-edge features</span>
                      <div className="pill-list" style={{ justifyContent: "flex-start", marginTop: 6 }}>
                        {le.leading_edge.map((g) => (
                          <span className="pill" key={g}>{g}</span>
                        ))}
                      </div>
                    </div>
                  )}
                </>
              )}
            </Card>
          )}
        </div>
      </div>
    </div>
  );
}
