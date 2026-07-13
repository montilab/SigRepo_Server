import { useState } from "react";
import { ScatterChart, Scatter, XAxis, YAxis, CartesianGrid, Tooltip, ResponsiveContainer } from "recharts";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import StatGrid from "../components/StatGrid";
import { signatures, signatureFeatures } from "../data/mock";

function pearson(xs: number[], ys: number[]): number | null {
  const n = xs.length;
  if (n < 2) return null;
  const mx = xs.reduce((a, b) => a + b, 0) / n;
  const my = ys.reduce((a, b) => a + b, 0) / n;
  let num = 0;
  let dx2 = 0;
  let dy2 = 0;
  for (let i = 0; i < n; i++) {
    const dx = xs[i] - mx;
    const dy = ys[i] - my;
    num += dx * dy;
    dx2 += dx * dx;
    dy2 += dy * dy;
  }
  if (dx2 === 0 || dy2 === 0) return null;
  return num / Math.sqrt(dx2 * dy2);
}

// Two-sample KS statistic + the standard asymptotic p-value approximation.
function ksTest(a: number[], b: number[]): { d: number; pValue: number } {
  const all = [...a, ...b].sort((x, y) => x - y);
  let d = 0;
  for (const v of all) {
    const cdfA = a.filter((x) => x <= v).length / a.length;
    const cdfB = b.filter((x) => x <= v).length / b.length;
    d = Math.max(d, Math.abs(cdfA - cdfB));
  }
  const nEff = (a.length * b.length) / (a.length + b.length);
  const pValue = Math.min(1, 2 * Math.exp(-2 * nEff * d * d));
  return { d, pValue };
}

export default function ComparePage() {
  const [sigAId, setSigAId] = useState(signatures[0].signature_id);
  const [sigBId, setSigBId] = useState(signatures[4].signature_id);
  const [compared, setCompared] = useState(false);

  const sigA = signatures.find((s) => s.signature_id === sigAId)!;
  const sigB = signatures.find((s) => s.signature_id === sigBId)!;
  const featuresA = signatureFeatures[sigAId] ?? [];
  const featuresB = signatureFeatures[sigBId] ?? [];

  const namesA = new Set(featuresA.map((f) => f.feature_name));
  const namesB = new Set(featuresB.map((f) => f.feature_name));
  const shared = featuresA.filter((f) => namesB.has(f.feature_name));
  const uniqueA = featuresA.filter((f) => !namesB.has(f.feature_name));
  const uniqueB = featuresB.filter((f) => !namesA.has(f.feature_name));
  const union = namesA.size + namesB.size - shared.length;
  const jaccard = union === 0 ? 0 : shared.length / union;

  const scatterData = shared.map((f) => ({
    feature: f.feature_name,
    scoreA: f.score,
    scoreB: featuresB.find((g) => g.feature_name === f.feature_name)!.score,
  }));

  const correlation = pearson(
    scatterData.map((d) => d.scoreA),
    scatterData.map((d) => d.scoreB)
  );

  const ks = ksTest(
    featuresA.map((f) => f.score),
    featuresB.map((f) => f.score)
  );

  const rows = [
    ...shared.map((f) => ({ feature: f.feature_name, status: "Shared", scoreA: f.score, scoreB: featuresB.find((g) => g.feature_name === f.feature_name)!.score })),
    ...uniqueA.map((f) => ({ feature: f.feature_name, status: `Only in ${sigA.signature_name}`, scoreA: f.score, scoreB: null })),
    ...uniqueB.map((f) => ({ feature: f.feature_name, status: `Only in ${sigB.signature_name}`, scoreA: null, scoreB: f.score })),
  ];

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #3a2f5c 0%, #6a4f96 100%)"
        title="Compare"
        description="Compare two signatures by shared/unique features, score correlation, and score distribution similarity."
      />

      <Card title="Select Signatures" helper="Choose two signatures to compare feature-by-feature.">
        <div className="form-grid-2">
          <label className="field">
            <span>Signature A</span>
            <select className="select-input" value={sigAId} onChange={(e) => setSigAId(e.target.value)}>
              {signatures.map((s) => (
                <option key={s.signature_id} value={s.signature_id}>
                  {s.signature_name}
                </option>
              ))}
            </select>
          </label>
          <label className="field">
            <span>Signature B</span>
            <select className="select-input" value={sigBId} onChange={(e) => setSigBId(e.target.value)}>
              {signatures.map((s) => (
                <option key={s.signature_id} value={s.signature_id}>
                  {s.signature_name}
                </option>
              ))}
            </select>
          </label>
        </div>
        <button className="btn btn-primary" style={{ marginTop: 14 }} disabled={sigAId === sigBId} onClick={() => setCompared(true)}>
          Compare
        </button>
        {sigAId === sigBId && <p className="card-helper" style={{ marginTop: 8 }}>Choose two different signatures.</p>}
      </Card>

      {compared && sigAId !== sigBId && (
        <>
          <div className="grid-2">
            <Card title="Feature Overlap">
              <svg viewBox="0 0 300 160" width="100%" height="160">
                <circle cx="115" cy="80" r="68" fill="#1c5d87" opacity="0.32" />
                <circle cx="185" cy="80" r="68" fill="#6a4f96" opacity="0.32" />
                <text x="75" y="60" textAnchor="middle" fontSize="11" fill="#597189">
                  {sigA.signature_name.length > 16 ? sigA.signature_name.slice(0, 14) + "…" : sigA.signature_name}
                </text>
                <text x="225" y="60" textAnchor="middle" fontSize="11" fill="#597189">
                  {sigB.signature_name.length > 16 ? sigB.signature_name.slice(0, 14) + "…" : sigB.signature_name}
                </text>
                <text x="75" y="95" textAnchor="middle" fontSize="22" fontWeight="700" fill="#17324d">
                  {uniqueA.length}
                </text>
                <text x="150" y="95" textAnchor="middle" fontSize="22" fontWeight="700" fill="#17324d">
                  {shared.length}
                </text>
                <text x="225" y="95" textAnchor="middle" fontSize="22" fontWeight="700" fill="#17324d">
                  {uniqueB.length}
                </text>
              </svg>
              <StatGrid stats={[{ label: "Shared", value: shared.length }, { label: "Only A", value: uniqueA.length }, { label: "Only B", value: uniqueB.length }, { label: "Jaccard", value: jaccard.toFixed(2) }]} />
            </Card>

            <Card title="Score Correlation" helper={`Shared-feature scores. Pearson r = ${correlation === null ? "N/A" : correlation.toFixed(2)}.`}>
              {scatterData.length === 0 ? (
                <div className="empty-state">No shared features to correlate.</div>
              ) : (
                <ResponsiveContainer width="100%" height={220}>
                  <ScatterChart margin={{ top: 10, right: 20, bottom: 10, left: 10 }}>
                    <CartesianGrid strokeDasharray="3 3" stroke="#e6edf3" />
                    <XAxis type="number" dataKey="scoreA" name={sigA.signature_name} tick={{ fontSize: 11, fill: "#3f556b" }} />
                    <YAxis type="number" dataKey="scoreB" name={sigB.signature_name} tick={{ fontSize: 11, fill: "#3f556b" }} />
                    <Tooltip cursor={{ strokeDasharray: "3 3" }} />
                    <Scatter data={scatterData} fill="#1c5d87" />
                  </ScatterChart>
                </ResponsiveContainer>
              )}
            </Card>
          </div>

          <Card title="Score Distribution Similarity (KS Test)" helper="Two-sample Kolmogorov-Smirnov test comparing the full score distributions of both signatures.">
            <StatGrid
              stats={[
                { label: "D Statistic", value: ks.d.toFixed(3) },
                { label: "P-value (approx.)", value: ks.pValue < 0.001 ? ks.pValue.toExponential(1) : ks.pValue.toFixed(3) },
                { label: sigA.signature_name, value: `${featuresA.length} features` },
                { label: sigB.signature_name, value: `${featuresB.length} features` },
              ]}
            />
          </Card>

          <Card title="Feature Breakdown">
            <table className="dt-table">
              <thead>
                <tr>
                  <th>Feature</th>
                  <th>Status</th>
                  <th>Score A</th>
                  <th>Score B</th>
                </tr>
              </thead>
              <tbody>
                {rows.map((r) => (
                  <tr key={r.feature + r.status}>
                    <td>{r.feature}</td>
                    <td>
                      <span className={"badge " + (r.status === "Shared" ? "badge-status" : "badge-local")}>{r.status}</span>
                    </td>
                    <td>{r.scoreA === null ? "—" : r.scoreA.toFixed(2)}</td>
                    <td>{r.scoreB === null ? "—" : r.scoreB.toFixed(2)}</td>
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
