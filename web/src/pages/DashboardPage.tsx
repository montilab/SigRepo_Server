import { useEffect, useState } from "react";
import { useNavigate } from "react-router-dom";
import { Dna, Users, Microscope, Beaker, ArrowUpRight } from "lucide-react";
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
import Skeleton from "../components/Skeleton";
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
        if (!cancelled) setInsights(result);
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
    <div className="page page-dash">
      <PageHeader
        variant="bar"
        title="Welcome to SigRepo"
        subtitle="A snapshot of the repository."
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
        <Card className="dash-fill-card">
          <div className="empty-state">{error}</div>
        </Card>
      ) : (
        <div className="dash-charts">
          <Card className="dash-chart" title="By organism" subtitle="Distribution across organisms">
            <div className="chart-fill">
              {loading ? (
                <Skeleton width="100%" height="100%" radius={8} />
              ) : (
                <ResponsiveContainer width="100%" height="100%">
                  <PieChart>
                    <Pie
                      data={insights?.by_organism ?? []}
                      dataKey="value"
                      nameKey="name"
                      innerRadius="55%"
                      outerRadius="82%"
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
              )}
            </div>
            <div className="legend legend-compact">
              {(insights?.by_organism ?? []).slice(0, 5).map((o, i) => (
                <div className="legend-item" key={o.name}>
                  <span className="legend-dot" style={{ background: VIZ[i % VIZ.length] }} />
                  <span className="legend-label">{o.name}</span>
                  <span className="legend-value">{o.value}</span>
                </div>
              ))}
            </div>
          </Card>

          <Card className="dash-chart" title="By assay" subtitle="Counts per assay type">
            <div className="chart-fill">
              {loading ? (
                <Skeleton width="100%" height="100%" radius={8} />
              ) : (
                <ResponsiveContainer width="100%" height="100%">
                  <BarChart data={insights?.by_assay ?? []} margin={{ top: 8, right: 8, bottom: 8, left: -12 }}>
                    <CartesianGrid vertical={false} stroke="var(--viz-grid)" />
                    <XAxis
                      dataKey="name"
                      tick={{ fontSize: 11, fill: "var(--text-muted)" }}
                      axisLine={false}
                      tickLine={false}
                      interval={0}
                      angle={-12}
                      textAnchor="end"
                      height={44}
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
              )}
            </div>
          </Card>

          <Card className="dash-chart" title="Top contributors" subtitle="Most active by visible signatures">
            <div className="chart-fill">
              {loading ? (
                <Skeleton width="100%" height="100%" radius={8} />
              ) : (
                <ResponsiveContainer width="100%" height="100%">
                  <BarChart data={insights?.top_contributors ?? []} layout="vertical" margin={{ top: 4, right: 16, bottom: 4, left: 8 }}>
                    <CartesianGrid horizontal={false} stroke="var(--viz-grid)" />
                    <XAxis type="number" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} allowDecimals={false} />
                    <YAxis type="category" dataKey="name" tick={{ fontSize: 11, fill: "var(--text-secondary)" }} axisLine={false} tickLine={false} width={104} />
                    <Tooltip cursor={{ fill: "var(--surface-hover)" }} contentStyle={tooltipStyle} />
                    <Bar dataKey="value" fill="var(--accent)" radius={[0, 4, 4, 0]} barSize={16} />
                  </BarChart>
                </ResponsiveContainer>
              )}
            </div>
          </Card>
        </div>
      )}
    </div>
  );
}
