import { useNavigate } from "react-router-dom";
import { Dna, Users, Microscope, Beaker, ArrowUpRight, FlaskConical, GitCompare, Layers } from "lucide-react";
import { ResponsiveContainer, BarChart, Bar, Cell, XAxis, Tooltip } from "recharts";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import StatCard from "../components/StatCard";
import Badge from "../components/Badge";
import { signatures, repositorySummary, assayCounts } from "../data/mock";

const VIZ = ["var(--viz-1)", "var(--viz-2)", "var(--viz-3)", "var(--viz-4)", "var(--viz-5)"];

export default function DashboardPage() {
  const navigate = useNavigate();
  const recent = [...signatures].sort((a, b) => b.date_created.localeCompare(a.date_created)).slice(0, 5);

  return (
    <div className="page">
      <PageHeader
        title="Dashboard"
        subtitle="Overview of your signature repository."
        actions={
          <button className="btn btn-primary" onClick={() => navigate("/signatures")}>
            Browse signatures <ArrowUpRight size={16} />
          </button>
        }
      />

      <div className="stat-row">
        <StatCard label="Total signatures" value={repositorySummary.total_signatures} icon={Dna} delta={{ value: "+12 this month", positive: true }} />
        <StatCard label="Active users" value={repositorySummary.total_users} icon={Users} />
        <StatCard label="Organisms" value={repositorySummary.total_organisms} icon={Microscope} />
        <StatCard label="Assay types" value={repositorySummary.total_assays} icon={Beaker} />
      </div>

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
              {recent.map((s) => (
                <tr key={s.signature_id} className="dt-clickable" onClick={() => navigate("/signatures")}>
                  <td>
                    <span className="cell-strong">{s.signature_name}</span>
                    <span className="cell-sub">{s.organism} · {s.phenotype}</span>
                  </td>
                  <td>
                    <Badge tone="neutral">{s.assay_type}</Badge>
                  </td>
                  <td>{s.user_name}</td>
                  <td className="dt-right cell-muted">{s.date_created}</td>
                </tr>
              ))}
            </tbody>
          </table>
        </Card>

        <div className="dash-side">
          <Card title="Composition" subtitle="Signatures by assay type">
            <ResponsiveContainer width="100%" height={140}>
              <BarChart data={assayCounts} margin={{ top: 4, right: 0, bottom: 0, left: 0 }}>
                <XAxis dataKey="name" tick={{ fontSize: 10, fill: "var(--text-muted)" }} axisLine={false} tickLine={false} interval={0} />
                <Tooltip cursor={{ fill: "var(--surface-hover)" }} contentStyle={tooltipStyle} />
                <Bar dataKey="value" radius={[4, 4, 0, 0]}>
                  {assayCounts.map((_, i) => (
                    <Cell key={i} fill={VIZ[i % VIZ.length]} />
                  ))}
                </Bar>
              </BarChart>
            </ResponsiveContainer>
          </Card>

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
    </div>
  );
}

const tooltipStyle = {
  border: "1px solid var(--border)",
  borderRadius: 8,
  boxShadow: "var(--shadow-md)",
  fontSize: 12,
} as const;
