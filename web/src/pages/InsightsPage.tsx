import { BarChart, Bar, XAxis, YAxis, ResponsiveContainer, Tooltip, CartesianGrid } from "recharts";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import StatGrid from "../components/StatGrid";
import { organismCounts, assayCounts, topContributors, repositorySummary } from "../data/mock";

export default function InsightsPage() {
  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #153b59 0%, #28658d 100%)"
        title="Insights"
        description="Repository-wide statistics: signature counts by organism and assay, and the most active contributors."
      />

      <Card title="Repository Snapshot" helper="A quick view of the current repository footprint based on the signatures available to your account.">
        <StatGrid
          stats={[
            { label: "Total Signatures", value: repositorySummary.total_signatures },
            { label: "Active Users", value: repositorySummary.total_users },
            { label: "Organisms", value: repositorySummary.total_organisms },
            { label: "Assay Types", value: repositorySummary.total_assays },
          ]}
        />
      </Card>

      <div className="grid-3">
        <Card title="By Organism" helper="Distribution of signatures across supported organisms.">
          <ResponsiveContainer width="100%" height={260}>
            <BarChart data={organismCounts}>
              <CartesianGrid strokeDasharray="3 3" vertical={false} stroke="#e6edf3" />
              <XAxis dataKey="name" tick={{ fontSize: 11, fill: "#3f556b" }} />
              <YAxis tick={{ fontSize: 11, fill: "#3f556b" }} />
              <Tooltip />
              <Bar dataKey="value" fill="#2d6f8f" radius={[4, 4, 0, 0]} />
            </BarChart>
          </ResponsiveContainer>
        </Card>
        <Card title="By Assay" helper="Breakdown of signatures by assay type.">
          <ResponsiveContainer width="100%" height={260}>
            <BarChart data={assayCounts}>
              <CartesianGrid strokeDasharray="3 3" vertical={false} stroke="#e6edf3" />
              <XAxis dataKey="name" tick={{ fontSize: 10, fill: "#3f556b" }} angle={-15} textAnchor="end" height={50} />
              <YAxis tick={{ fontSize: 11, fill: "#3f556b" }} />
              <Tooltip />
              <Bar dataKey="value" fill="#4f8fb0" radius={[4, 4, 0, 0]} />
            </BarChart>
          </ResponsiveContainer>
        </Card>
        <Card title="Top Contributors" helper="Most active users based on visible signatures.">
          <ResponsiveContainer width="100%" height={260}>
            <BarChart data={topContributors} layout="vertical">
              <CartesianGrid strokeDasharray="3 3" horizontal={false} stroke="#e6edf3" />
              <XAxis type="number" tick={{ fontSize: 11, fill: "#3f556b" }} />
              <YAxis type="category" dataKey="name" tick={{ fontSize: 11, fill: "#3f556b" }} width={90} />
              <Tooltip />
              <Bar dataKey="value" fill="#1c5d87" radius={[0, 4, 4, 0]} />
            </BarChart>
          </ResponsiveContainer>
        </Card>
      </div>
    </div>
  );
}
