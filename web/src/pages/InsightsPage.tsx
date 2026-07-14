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
import { Dna, Users, Microscope, Beaker } from "lucide-react";
import { organismCounts, assayCounts, topContributors, repositorySummary } from "../data/mock";

const VIZ = ["var(--viz-1)", "var(--viz-2)", "var(--viz-3)", "var(--viz-4)", "var(--viz-5)"];
const tooltipStyle = {
  border: "1px solid var(--border)",
  borderRadius: 8,
  boxShadow: "var(--shadow-md)",
  fontSize: 12,
} as const;

export default function InsightsPage() {
  return (
    <div className="page">
      <PageHeader title="Insights" subtitle="Repository-wide composition and contribution analytics." />

      <div className="stat-row">
        <StatCard label="Total signatures" value={repositorySummary.total_signatures} icon={Dna} />
        <StatCard label="Active users" value={repositorySummary.total_users} icon={Users} />
        <StatCard label="Organisms" value={repositorySummary.total_organisms} icon={Microscope} />
        <StatCard label="Assay types" value={repositorySummary.total_assays} icon={Beaker} />
      </div>

      <div className="insights-grid">
        <Card title="By organism" subtitle="Signature distribution across organisms">
          <ResponsiveContainer width="100%" height={240}>
            <PieChart>
              <Pie data={organismCounts} dataKey="value" nameKey="name" innerRadius={54} outerRadius={88} paddingAngle={2} stroke="none">
                {organismCounts.map((_, i) => (
                  <Cell key={i} fill={VIZ[i % VIZ.length]} />
                ))}
              </Pie>
              <Tooltip contentStyle={tooltipStyle} />
            </PieChart>
          </ResponsiveContainer>
          <div className="legend">
            {organismCounts.map((o, i) => (
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
            <BarChart data={assayCounts} margin={{ top: 8, right: 8, bottom: 8, left: -8 }}>
              <CartesianGrid vertical={false} stroke="var(--viz-grid)" />
              <XAxis dataKey="name" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} interval={0} angle={-12} textAnchor="end" height={48} />
              <YAxis tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} />
              <Tooltip cursor={{ fill: "var(--surface-hover)" }} contentStyle={tooltipStyle} />
              <Bar dataKey="value" radius={[4, 4, 0, 0]}>
                {assayCounts.map((_, i) => (
                  <Cell key={i} fill={VIZ[i % VIZ.length]} />
                ))}
              </Bar>
            </BarChart>
          </ResponsiveContainer>
        </Card>

        <Card title="Top contributors" subtitle="Most active users by visible signatures" className="insights-wide">
          <ResponsiveContainer width="100%" height={220}>
            <BarChart data={topContributors} layout="vertical" margin={{ top: 4, right: 16, bottom: 4, left: 24 }}>
              <CartesianGrid horizontal={false} stroke="var(--viz-grid)" />
              <XAxis type="number" tick={{ fontSize: 11, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} />
              <YAxis type="category" dataKey="name" tick={{ fontSize: 11, fill: "var(--text-secondary)" }} axisLine={false} tickLine={false} width={110} />
              <Tooltip cursor={{ fill: "var(--surface-hover)" }} contentStyle={tooltipStyle} />
              <Bar dataKey="value" fill="var(--accent)" radius={[0, 4, 4, 0]} barSize={18} />
            </BarChart>
          </ResponsiveContainer>
        </Card>
      </div>
    </div>
  );
}
