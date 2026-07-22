import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import {
  Dna,
  Users,
  Microscope,
  Beaker,
  ArrowUpRight,
  FlaskConical,
  GitCompare,
  Layers,
} from "lucide-react";
import {
  ResponsiveContainer,
  BarChart,
  Bar,
  Cell,
  XAxis,
  YAxis,
  Tooltip,
  CartesianGrid,
  PieChart,
  Pie,
} from "recharts";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import StatCard from "../components/StatCard";
import Badge from "../components/Badge";
import { getInsights, ApiError, type Insights } from "../api/client";

const VIZ = ["var(--viz-1)", "var(--viz-2)", "var(--viz-3)", "var(--viz-4)", "var(--viz-5)"];
const tooltipStyle = {
  border: "1px solid var(--border)",
  borderRadius: 8,
  boxShadow: "var(--shadow-md)",
  fontSize: 12,
} as const;

export default function DashboardPage() {
  const navigate = useNavigate();
  const [insights, setInsights] = useState<Insights | null>(null);
  const [loading, setLoading] = useState(true);
  const [error, setError] = useState<string | null>(null);

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    setError(null);

    getInsights()
      .then((result) => {
        if (cancelled) return;
        setInsights(result);
      })
      .catch((err: unknown) => {
        if (cancelled) return;
        setInsights(null);
        setError(err instanceof ApiError ? err.message : "Failed to load repository insights.");
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });

    return () => {
      cancelled = true;
    };
  }, []);

  return (
    <div className="page">
      <PageHeader
        title="Dashboard"
        subtitle="Overview and repository-wide composition analytics."
        actions={
          <button className="btn btn-primary" onClick={() => navigate("/signatures")}>
            Browse signatures <ArrowUpRight size={16} />
          </button>
        }
      />

      <div className="stat-row">
        <StatCard label="Total signatures" value={insights?.total_signatures ?? "—"} icon={Dna} />
        <StatCard label="Active users" value={insights?.total_users ?? "—"} icon={Users} />
        <StatCard label="Organisms" value={insights?.total_organisms ?? "—"} icon={Microscope} />
        <StatCard label="Assay types" value={insights?.total_assays ?? "—"} icon={Beaker} />
      </div>

      {error ? (
        <Card>
          <div className="empty-state">{error}</div>
        </Card>
      ) : loading ? (
        <Card>
          <div className="empty-state">Loading...</div>
        </Card>
      ) : (
        <>
          <div className="dash-grid">
            <Card title="Recent signatures" subtitle="Most recently created across the repository" padded={false}>
              <table className="dt-table dt-table-flush">
                <thead>
                  <tr>
                    <th>Signature</th>
                    <th>Assay</th>
                    <th>Owner</th>
                    <th className="dt-right">Created</th>
                  </tr>
                </thead>
                <tbody>
                  {(insights?.recent_signatures ?? []).map((s) => (
                    <tr key={s.signature_hashkey} className="dt-clickable" onClick={() => navigate("/signatures")}>
                      <td>
                        <span className="cell-strong">{s.signature_name}</span>
                      </td>
                      <td>
                        <Badge tone="neutral">{s.assay_type}</Badge>
                      </td>
                      <td>{s.user_name}</td>
                      <td className="dt-right cell-muted">{s.date_created}</td>
                    </tr>
                  ))}
                  {(insights?.recent_signatures ?? []).length === 0 && (
                    <tr>
                      <td colSpan={4} className="cell-muted">No signatures yet.</td>
                    </tr>
                  )}
                </tbody>
              </table>
            </Card>

            <div className="dash-side">
              <Card title="Quick actions">
                <div className="quick-actions">
                  <button className="quick-action" onClick={() => navigate("/annotate")}>
                    <span className="quick-action-icon"><FlaskConical size={16} /></span>
                    <span>
                      <strong>Run enrichment</strong>
                      <small>Annotate a signature with hypeR</small>
                    </span>
                    <ArrowUpRight size={15} className="quick-action-arrow" />
                  </button>
                  <button className="quick-action" onClick={() => navigate("/compare")}>
                    <span className="quick-action-icon"><GitCompare size={16} /></span>
                    <span>
                      <strong>Compare signatures</strong>
                      <small>Overlap &amp; score correlation</small>
                    </span>
                    <ArrowUpRight size={15} className="quick-action-arrow" />
                  </button>
                  <button className="quick-action" onClick={() => navigate("/collections")}>
                    <span className="quick-action-icon"><Layers size={16} /></span>
                    <span>
                      <strong>Manage collections</strong>
                      <small>Group related signatures</small>
                    </span>
                    <ArrowUpRight size={15} className="quick-action-arrow" />
                  </button>
                </div>
              </Card>
            </div>
          </div>

          <div className="insights-grid">
            <Card title="By organism" subtitle="Signature distribution across organisms">
              <ResponsiveContainer width="100%" height={240}>
                <PieChart>
                  <Pie
                    data={insights?.by_organism ?? []}
                    dataKey="value"
                    nameKey="name"
                    innerRadius={54}
                    outerRadius={88}
                    paddingAngle={2}
                    stroke="none"
                  >
                    {(insights?.by_organism ?? []).map((_, i) => (
                      <Cell key={i} fill={VIZ[i % VIZ.length]} />
                    ))}
                  </Pie>
                  <Tooltip contentStyle={tooltipStyle} />
                </PieChart>
              </ResponsiveContainer>
              <div className="legend">
                {(insights?.by_organism ?? []).map((o, i) => (
                  <div className="legend-item" key={o.name}>
                    <span className="legend-dot" style={{ background: VIZ[i % VIZ.length] }} />
                    <span className="legend-label">{o.name}</span>
                    <span className="legend-value">{o.value}</span>
                  </div>
                ))}
              </div>
            </Card>

            <Card title="By assay" subtitle="Signature counts per assay type">
              <ResponsiveContainer width="100%" height={300}>
                <BarChart data={insights?.by_assay ?? []} margin={{ top: 8, right: 8, bottom: 8, left: -8 }}>
                  <CartesianGrid vertical={false} stroke="var(--viz-grid)" />
                  <XAxis
                    dataKey="name"
                    tick={{ fontSize: 11, fill: "var(--text-muted)" }}
                    axisLine={false}
                    tickLine={false}
                    interval={0}
                    angle={-12}
                    textAnchor="end"
                    height={48}
                  />
                  <YAxis tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} allowDecimals={false} />
                  <Tooltip cursor={{ fill: "var(--surface-hover)" }} contentStyle={tooltipStyle} />
                  <Bar dataKey="value" radius={[4, 4, 0, 0]}>
                    {(insights?.by_assay ?? []).map((_, i) => (
                      <Cell key={i} fill={VIZ[i % VIZ.length]} />
                    ))}
                  </Bar>
                </BarChart>
              </ResponsiveContainer>
            </Card>

            <Card title="Top contributors" subtitle="Most active users by visible signatures" className="insights-wide">
              <ResponsiveContainer width="100%" height={220}>
                <BarChart data={insights?.top_contributors ?? []} layout="vertical" margin={{ top: 4, right: 16, bottom: 4, left: 24 }}>
                  <CartesianGrid horizontal={false} stroke="var(--viz-grid)" />
                  <XAxis type="number" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} allowDecimals={false} />
                  <YAxis type="category" dataKey="name" tick={{ fontSize: 11, fill: "var(--text-secondary)" }} axisLine={false} tickLine={false} width={110} />
                  <Tooltip cursor={{ fill: "var(--surface-hover)" }} contentStyle={tooltipStyle} />
                  <Bar dataKey="value" fill="var(--accent)" radius={[0, 4, 4, 0]} barSize={18} />
                </BarChart>
              </ResponsiveContainer>
            </Card>
          </div>
        </>
      )}
    </div>
  );
}
