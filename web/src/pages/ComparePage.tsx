import { useState } from "react";
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from "recharts";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import StatCard from "../components/StatCard";
import { signatures, signatureFeatures } from "../data/mock";

const tooltipStyle = { border: "1px solid var(--border)", borderRadius: 8, boxShadow: "var(--shadow-md)", fontSize: 12 } as const;

function pearson(xs: number[], ys: number[]): number | null {
  const n = xs.length;
  if (n < 2) return null;
  const mx = xs.reduce((a, b) => a + b, 0) / n;
  const my = ys.reduce((a, b) => a + b, 0) / n;
  let num = 0, dx2 = 0, dy2 = 0;
  for (let i = 0; i < n; i++) {
    const dx = xs[i] - mx, dy = ys[i] - my;
    num += dx * dy; dx2 += dx * dx; dy2 += dy * dy;
  }
  if (dx2 === 0 || dy2 === 0) return null;
  return num / Math.sqrt(dx2 * dy2);
}

function ksTest(a: number[], b: number[]): { d: number; pValue: number } {
  const all = [...a, ...b].sort((x, y) => x - y);
  let d = 0;
  for (const v of all) {
    d = Math.max(d, Math.abs(a.filter((x) => x <= v).length / a.length - b.filter((x) => x <= v).length / b.length));
  }
  const nEff = (a.length * b.length) / (a.length + b.length);
  return { d, pValue: Math.min(1, 2 * Math.exp(-2 * nEff * d * d)) };
}

export default function ComparePage() {
  const [aId, setAId] = useState(signatures[0].signature_id);
  const [bId, setBId] = useState(signatures[4].signature_id);
  const [ran, setRan] = useState(false);

  const sigA = signatures.find((s) => s.signature_id === aId)!;
  const sigB = signatures.find((s) => s.signature_id === bId)!;
  const fa = signatureFeatures[aId] ?? [];
  const fb = signatureFeatures[bId] ?? [];
  const na = new Set(fa.map((f) => f.feature_name));
  const nb = new Set(fb.map((f) => f.feature_name));
  const shared = fa.filter((f) => nb.has(f.feature_name));
  const onlyA = fa.filter((f) => !nb.has(f.feature_name));
  const onlyB = fb.filter((f) => !na.has(f.feature_name));
  const union = na.size + nb.size - shared.length;
  const jaccard = union === 0 ? 0 : shared.length / union;

  const scatter = shared.map((f) => ({ feature: f.feature_name, scoreA: f.score, scoreB: fb.find((g) => g.feature_name === f.feature_name)!.score }));
  const corr = pearson(scatter.map((d) => d.scoreA), scatter.map((d) => d.scoreB));
  const ks = ksTest(fa.map((f) => f.score), fb.map((f) => f.score));

  const rows = [
    ...shared.map((f) => ({ feature: f.feature_name, status: "Shared" as const, a: f.score as number | null, b: fb.find((g) => g.feature_name === f.feature_name)!.score as number | null })),
    ...onlyA.map((f) => ({ feature: f.feature_name, status: "Only A" as const, a: f.score as number | null, b: null })),
    ...onlyB.map((f) => ({ feature: f.feature_name, status: "Only B" as const, a: null, b: f.score as number | null })),
  ];

  return (
    <div className="page">
      <PageHeader title="Compare" subtitle="Pairwise signature comparison — overlap, correlation, and distribution." />

      <Card title="Select signatures">
        <div className="compare-picker">
          <label className="field">
            <span className="field-label">Signature A</span>
            <select className="input" value={aId} onChange={(e) => { setAId(e.target.value); setRan(false); }}>
              {signatures.map((s) => <option key={s.signature_id} value={s.signature_id}>{s.signature_name}</option>)}
            </select>
          </label>
          <div className="compare-vs">vs</div>
          <label className="field">
            <span className="field-label">Signature B</span>
            <select className="input" value={bId} onChange={(e) => { setBId(e.target.value); setRan(false); }}>
              {signatures.map((s) => <option key={s.signature_id} value={s.signature_id}>{s.signature_name}</option>)}
            </select>
          </label>
          <button className="btn btn-primary" disabled={aId === bId} onClick={() => setRan(true)}>
            Compare
          </button>
        </div>
        {aId === bId && <p className="muted-note">Choose two different signatures.</p>}
      </Card>

      {ran && aId !== bId && (
        <>
          <div className="stat-row">
            <StatCard label="Shared features" value={shared.length} />
            <StatCard label={`Only in A`} value={onlyA.length} />
            <StatCard label={`Only in B`} value={onlyB.length} />
            <StatCard label="Jaccard index" value={jaccard.toFixed(2)} />
          </div>

          <div className="compare-grid">
            <Card title="Feature overlap">
              <svg viewBox="0 0 320 170" width="100%" height="170" className="venn">
                <circle cx="124" cy="85" r="70" fill="var(--viz-1)" fillOpacity="0.28" />
                <circle cx="196" cy="85" r="70" fill="var(--viz-5)" fillOpacity="0.28" />
                <text x="78" y="88" textAnchor="middle" className="venn-num">{onlyA.length}</text>
                <text x="160" y="88" textAnchor="middle" className="venn-num">{shared.length}</text>
                <text x="242" y="88" textAnchor="middle" className="venn-num">{onlyB.length}</text>
                <text x="78" y="150" textAnchor="middle" className="venn-label">A only</text>
                <text x="160" y="150" textAnchor="middle" className="venn-label">Shared</text>
                <text x="242" y="150" textAnchor="middle" className="venn-label">B only</text>
              </svg>
            </Card>

            <Card title="Score correlation" subtitle={`Pearson r = ${corr === null ? "N/A" : corr.toFixed(2)}`}>
              {scatter.length === 0 ? (
                <p className="muted-note">No shared features to correlate.</p>
              ) : (
                <ResponsiveContainer width="100%" height={220}>
                  <ScatterChart margin={{ top: 8, right: 16, bottom: 8, left: 0 }}>
                    <CartesianGrid stroke="var(--viz-grid)" />
                    <XAxis type="number" dataKey="scoreA" name="A" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} />
                    <YAxis type="number" dataKey="scoreB" name="B" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} />
                    <Tooltip cursor={{ strokeDasharray: "3 3" }} contentStyle={tooltipStyle} />
                    <Scatter data={scatter} fill="var(--accent)" fillOpacity={0.85} />
                  </ScatterChart>
                </ResponsiveContainer>
              )}
            </Card>
          </div>

          <Card title="Score distribution" subtitle="Two-sample Kolmogorov–Smirnov test">
            <div className="ks-row">
              <div className="ks-stat"><span className="ks-label">D statistic</span><span className="ks-value">{ks.d.toFixed(3)}</span></div>
              <div className="ks-stat"><span className="ks-label">P-value (approx.)</span><span className="ks-value">{ks.pValue < 0.001 ? ks.pValue.toExponential(1) : ks.pValue.toFixed(3)}</span></div>
              <div className="ks-stat"><span className="ks-label">{sigA.signature_name}</span><span className="ks-value ks-value-sm">{fa.length} features</span></div>
              <div className="ks-stat"><span className="ks-label">{sigB.signature_name}</span><span className="ks-value ks-value-sm">{fb.length} features</span></div>
            </div>
          </Card>

          <Card title="Feature breakdown" padded={false}>
            <table className="dt-table dt-table-flush">
              <thead>
                <tr>
                  <th>Feature</th>
                  <th>Status</th>
                  <th className="dt-right">Score A</th>
                  <th className="dt-right">Score B</th>
                </tr>
              </thead>
              <tbody>
                {rows.map((r) => (
                  <tr key={r.feature + r.status}>
                    <td className="cell-strong">{r.feature}</td>
                    <td><Badge tone={r.status === "Shared" ? "accent" : "neutral"}>{r.status}</Badge></td>
                    <td className="dt-right cell-mono">{r.a === null ? "—" : r.a.toFixed(2)}</td>
                    <td className="dt-right cell-mono">{r.b === null ? "—" : r.b.toFixed(2)}</td>
                  </tr>
                ))}
              </tbody>
            </table>
          </Card>
        </>
      )}
    </div>
  );
}
