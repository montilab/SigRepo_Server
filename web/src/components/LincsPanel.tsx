import { useEffect, useState } from "react";
import { Pill, Info } from "lucide-react";
import Card from "./Card";
import { SkeletonRows } from "./Skeleton";
import { lincsStatus, lincsSearch, ApiError, type LincsResult, type LincsStatus } from "../api/client";

// LINCS connectivity search: which perturbations produce this expression
// pattern, or reverse it. A negative NCS means the perturbation opposes the
// signature, which is the drug-repurposing case.
//
// The reference database is several GB and is not shipped with the image, so
// this asks the server whether the feature is configured before offering it.
// Showing a button that always fails is worse than saying it is off.
export default function LincsPanel({ signatureHashkey }: { signatureHashkey: string }) {
  const [status, setStatus] = useState<LincsStatus | null>(null);
  const [loading, setLoading] = useState(false);
  const [result, setResult] = useState<LincsResult | null>(null);
  const [error, setError] = useState<string | null>(null);
  const [started, setStarted] = useState(false);

  useEffect(() => {
    let cancelled = false;
    lincsStatus()
      .then((s) => { if (!cancelled) setStatus(s); })
      .catch(() => { if (!cancelled) setStatus({ available: false, reason: null }); });
    return () => { cancelled = true; };
  }, []);

  async function run() {
    setStarted(true);
    setLoading(true);
    setError(null);
    try {
      setResult(await lincsSearch(signatureHashkey));
    } catch (err) {
      setError(err instanceof ApiError ? err.message : "Could not reach the server.");
    } finally {
      setLoading(false);
    }
  }

  // Don't render at all until we know — a panel that flashes "unavailable"
  // then becomes available reads as broken.
  if (status === null) return null;

  return (
    <Card className="lincs-card">
      <div className="lincs-head">
        <div className="lincs-title">
          <Pill size={16} />
          <div>
            <h4 className="detail-section-title" style={{ margin: 0 }}>Perturbation connectivity</h4>
            <span className="cell-sub">
              LINCS compounds whose expression profile matches or reverses this signature.
            </span>
          </div>
        </div>
        {status.available && started && !loading && (
          <button className="btn btn-ghost btn-sm" onClick={run}>Refresh</button>
        )}
      </div>

      {!status.available && (
        <div className="lincs-off">
          <Info size={15} />
          <span>{status.reason ?? "LINCS connectivity search is not enabled on this server."}</span>
        </div>
      )}

      {status.available && !started && (
        <div className="lincs-cta">
          <button className="btn btn-primary" onClick={run}>Search LINCS</button>
          <span className="cell-sub">Human transcriptomics signatures only. Takes a moment.</span>
        </div>
      )}

      {loading && <SkeletonRows rows={5} />}
      {error && <div className="empty-state">{error}</div>}

      {!loading && !error && result && (
        <>
          <div className="lincs-summary cell-sub">
            {result.total === 0
              ? `No perturbation scored against this signature's ${result.n_up} up / ${result.n_down} down genes.`
              : `${result.total.toLocaleString()} perturbations scored against ${result.n_up} up / ${result.n_down} down genes.`}
          </div>

          {result.hits.length > 0 && (
            <div className="lincs-tablewrap">
              <table className="lincs-table">
                <thead>
                  <tr>
                    <th>Perturbation</th><th>Cell</th><th className="num">NCS</th>
                    <th className="num">FDR</th><th>Direction</th>
                  </tr>
                </thead>
                <tbody>
                  {result.hits.map((h, i) => {
                    // NCS < 0 means the perturbation opposes the signature.
                    const reverses = (h.NCS ?? 0) < 0;
                    return (
                      <tr key={`${h.pert}-${h.cell}-${i}`}>
                        <td className="lincs-pert">{h.pert}</td>
                        <td className="cell-sub">{h.cell ?? "—"}</td>
                        <td className="num">{h.NCS?.toFixed(3) ?? "—"}</td>
                        <td className="num">{h.WTCS_FDR != null ? h.WTCS_FDR.toExponential(1) : "—"}</td>
                        <td>
                          <span className={reverses ? "lincs-rev" : "lincs-mim"}>
                            {reverses ? "reverses" : "mimics"}
                          </span>
                        </td>
                      </tr>
                    );
                  })}
                </tbody>
              </table>
            </div>
          )}
        </>
      )}
    </Card>
  );
}
