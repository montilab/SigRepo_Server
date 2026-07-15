import { useEffect, useMemo, useState } from "react";
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from "recharts";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import StatCard from "../components/StatCard";
import { searchSignatures, getSignatureContext, type SignatureSummary } from "../api/client";

const tooltipStyle = { border: "1px solid var(--border)", borderRadius: 8, boxShadow: "var(--shadow-md)", fontSize: 12 } as const;

// Signatures can carry a large number of features; this is well above what
// any real signature in the repo has today, so it effectively means "all of
// them" without leaving the endpoint's max_features uncapped.
const MAX_COMPARE_FEATURES = 2000;

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

function ksTest(a: number[], b: number[]): { d: number; pValue: number } | null {
  if (a.length === 0 || b.length === 0) return null;
  const all = [...a, ...b].sort((x, y) => x - y);
  let d = 0;
  for (const v of all) {
    d = Math.max(d, Math.abs(a.filter((x) => x <= v).length / a.length - b.filter((x) => x <= v).length / b.length));
  }
  const nEff = (a.length * b.length) / (a.length + b.length);
  return { d, pValue: Math.min(1, 2 * Math.exp(-2 * nEff * d * d)) };
}

function featureKey(f: { probe_id?: string; feature_id?: number }): string {
  return f.probe_id ?? String(f.feature_id ?? "");
}

function featureScore(f: { score?: unknown }): number {
  return typeof f.score === "number" ? f.score : Number(f.score);
}

export default function ComparePage() {
  const [signatures, setSignatures] = useState<SignatureSummary[]>([]);
  const [listLoading, setListLoading] = useState(true);
  const [listError, setListError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    searchSignatures({ limit: 100 })
      .then((results) => {
        if (cancelled) return;
        setSignatures(results);
        if (results.length > 0) setAHashkey(results[0].signature_hashkey);
        if (results.length > 1) setBHashkey(results[1].signature_hashkey);
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

  const [aHashkey, setAHashkey] = useState("");
  const [bHashkey, setBHashkey] = useState("");
  const [comparing, setComparing] = useState(false);
  const [compareError, setCompareError] = useState<string | null>(null);
  const [result, setResult] = useState<{
    sigA: SignatureSummary;
    sigB: SignatureSummary;
    fa: { key: string; score: number }[];
    fb: { key: string; score: number }[];
  } | null>(null);

  async function handleCompare() {
    const sigA = signatures.find((s) => s.signature_hashkey === aHashkey);
    const sigB = signatures.find((s) => s.signature_hashkey === bHashkey);
    if (!sigA || !sigB || aHashkey === bHashkey) return;

    setComparing(true);
    setCompareError(null);
    try {
      const [ctxA, ctxB] = await Promise.all([
        getSignatureContext(aHashkey, { maxFeatures: MAX_COMPARE_FEATURES }),
        getSignatureContext(bHashkey, { maxFeatures: MAX_COMPARE_FEATURES }),
      ]);
      setResult({
        sigA,
        sigB,
        fa: ctxA.features.map((f) => ({ key: featureKey(f), score: featureScore(f) })).filter((f) => f.key && Number.isFinite(f.score)),
        fb: ctxB.features.map((f) => ({ key: featureKey(f), score: featureScore(f) })).filter((f) => f.key && Number.isFinite(f.score)),
      });
    } catch (err) {
      setCompareError(err instanceof Error ? err.message : "Could not compare signatures.");
      setResult(null);
    } finally {
      setComparing(false);
    }
  }

  const stats = useMemo(() => {
    if (!result) return null;
    const { fa, fb } = result;
    const na = new Map(fa.map((f) => [f.key, f.score]));
    const nb = new Map(fb.map((f) => [f.key, f.score]));
    const shared = fa.filter((f) => nb.has(f.key));
    const onlyA = fa.filter((f) => !nb.has(f.key));
    const onlyB = fb.filter((f) => !na.has(f.key));
    const union = na.size + nb.size - shared.length;
    const jaccard = union === 0 ? 0 : shared.length / union;

    const scatter = shared.map((f) => ({ feature: f.key, scoreA: f.score, scoreB: nb.get(f.key)! }));
    const corr = pearson(scatter.map((d) => d.scoreA), scatter.map((d) => d.scoreB));
    const ks = ksTest(fa.map((f) => f.score), fb.map((f) => f.score));

    const rows = [
      ...shared.map((f) => ({ feature: f.key, status: "Shared" as const, a: f.score as number | null, b: nb.get(f.key) as number | null })),
      ...onlyA.map((f) => ({ feature: f.key, status: "Only A" as const, a: f.score as number | null, b: null })),
      ...onlyB.map((f) => ({ feature: f.key, status: "Only B" as const, a: null, b: f.score as number | null })),
    ];

    return { shared, onlyA, onlyB, jaccard, scatter, corr, ks, rows };
  }, [result]);

  return (
    <div className="page">
      <PageHeader title="Compare" subtitle="Pairwise signature comparison — overlap, correlation, and distribution." />

      <Card title="Select signatures">
        {listError && <p className="login-error">{listError}</p>}
        {!listLoading && !listError && signatures.length < 2 && (
          <p className="muted-note">Need at least two signatures in the repository to compare.</p>
        )}
        <div className="compare-picker">
          <label className="field">
            <span className="field-label">Signature A</span>
            <select
              className="input"
              value={aHashkey}
              onChange={(e) => { setAHashkey(e.target.value); setResult(null); }}
              disabled={listLoading || signatures.length === 0}
            >
              {signatures.map((s) => (
                <option key={s.signature_hashkey} value={s.signature_hashkey}>{s.signature_name}</option>
              ))}
            </select>
          </label>
          <div className="compare-vs">vs</div>
          <label className="field">
            <span className="field-label">Signature B</span>
            <select
              className="input"
              value={bHashkey}
              onChange={(e) => { setBHashkey(e.target.value); setResult(null); }}
              disabled={listLoading || signatures.length === 0}
            >
              {signatures.map((s) => (
                <option key={s.signature_hashkey} value={s.signature_hashkey}>{s.signature_name}</option>
              ))}
            </select>
          </label>
          <button className="btn btn-primary" disabled={aHashkey === bHashkey || comparing || listLoading} onClick={handleCompare}>
            {comparing ? "Comparing…" : "Compare"}
          </button>
        </div>
        {aHashkey === bHashkey && signatures.length > 1 && <p className="muted-note">Choose two different signatures.</p>}
        {compareError && <p className="login-error">{compareError}</p>}
      </Card>

      {result && stats && (
        <>
          <div className="stat-row">
            <StatCard label="Shared features" value={stats.shared.length} />
            <StatCard label="Only in A" value={stats.onlyA.length} />
            <StatCard label="Only in B" value={stats.onlyB.length} />
            <StatCard label="Jaccard index" value={stats.jaccard.toFixed(2)} />
          </div>

          <div className="compare-grid">
            <Card title="Feature overlap">
              <svg viewBox="0 0 320 170" width="100%" height="170" className="venn">
                <circle cx="124" cy="85" r="70" fill="var(--viz-1)" fillOpacity="0.28" />
                <circle cx="196" cy="85" r="70" fill="var(--viz-5)" fillOpacity="0.28" />
                <text x="78" y="88" textAnchor="middle" className="venn-num">{stats.onlyA.length}</text>
                <text x="160" y="88" textAnchor="middle" className="venn-num">{stats.shared.length}</text>
                <text x="242" y="88" textAnchor="middle" className="venn-num">{stats.onlyB.length}</text>
                <text x="78" y="150" textAnchor="middle" className="venn-label">A only</text>
                <text x="160" y="150" textAnchor="middle" className="venn-label">Shared</text>
                <text x="242" y="150" textAnchor="middle" className="venn-label">B only</text>
              </svg>
            </Card>

            <Card title="Score correlation" subtitle={`Pearson r = ${stats.corr === null ? "N/A" : stats.corr.toFixed(2)}`}>
              {stats.scatter.length === 0 ? (
                <p className="muted-note">No shared features to correlate.</p>
              ) : (
                <ResponsiveContainer width="100%" height={220}>
                  <ScatterChart margin={{ top: 8, right: 16, bottom: 8, left: 0 }}>
                    <CartesianGrid stroke="var(--viz-grid)" />
                    <XAxis type="number" dataKey="scoreA" name="A" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} />
                    <YAxis type="number" dataKey="scoreB" name="B" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} />
                    <Tooltip cursor={{ strokeDasharray: "3 3" }} contentStyle={tooltipStyle} />
                    <Scatter data={stats.scatter} fill="var(--accent)" fillOpacity={0.85} />
                  </ScatterChart>
                </ResponsiveContainer>
              )}
            </Card>
          </div>

          <Card title="Score distribution" subtitle="Two-sample Kolmogorov–Smirnov test">
            <div className="ks-row">
              <div className="ks-stat"><span className="ks-label">D statistic</span><span className="ks-value">{stats.ks ? stats.ks.d.toFixed(3) : "N/A"}</span></div>
              <div className="ks-stat"><span className="ks-label">P-value (approx.)</span><span className="ks-value">{stats.ks ? (stats.ks.pValue < 0.001 ? stats.ks.pValue.toExponential(1) : stats.ks.pValue.toFixed(3)) : "N/A"}</span></div>
              <div className="ks-stat"><span className="ks-label">{result.sigA.signature_name}</span><span className="ks-value ks-value-sm">{result.fa.length} features</span></div>
              <div className="ks-stat"><span className="ks-label">{result.sigB.signature_name}</span><span className="ks-value ks-value-sm">{result.fb.length} features</span></div>
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
                {stats.rows.map((r) => (
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
