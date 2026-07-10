import { useNavigate } from "react-router-dom";
import { Dna, Layers, FlaskConical } from "lucide-react";
import { BarChart, Bar, XAxis, YAxis, ResponsiveContainer, Tooltip, CartesianGrid } from "recharts";
import PageHero from "../components/PageHero";
import Card from "../components/Card";
import StatGrid from "../components/StatGrid";
import { organismCounts, assayCounts, topContributors, repositorySummary } from "../data/mock";

export default function HomePage() {
  const navigate = useNavigate();

  return (
    <div className="page">
      <PageHero
        gradient="linear-gradient(135deg, #153b59 0%, #28658d 100%)"
        title="Welcome to SigRepo"
        description="SigRepo is a collaborative signature repository for browsing, organizing, and annotating biological signatures. Use the workspace below to move quickly between repository exploration, collection management, and downstream enrichment analysis."
        actions={
          <>
            <button className="btn btn-primary" onClick={() => navigate("/signatures")}>
              Browse Signatures
            </button>
            <button className="btn btn-default" onClick={() => navigate("/collections")}>
              Explore Collections
            </button>
            <a className="btn btn-default" href="https://github.com/montilab/SigRepo" target="_blank" rel="noreferrer">
              R Client Docs
            </a>
          </>
        }
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

      <Card title="Quick Actions" helper="Jump directly into the areas of the platform you are most likely to use next.">
        <div className="action-grid">
          <div className="action-card">
            <span className="action-icon">
              <Dna size={22} />
            </span>
            <h4>Signatures</h4>
            <p>Browse, inspect, download, and manage repository signatures.</p>
            <button className="btn btn-primary" onClick={() => navigate("/signatures")}>
              Open Signatures
            </button>
          </div>
          <div className="action-card">
            <span className="action-icon">
              <Layers size={22} />
            </span>
            <h4>Collections</h4>
            <p>Review grouped signatures and manage reusable collection sets.</p>
            <button className="btn btn-primary" onClick={() => navigate("/collections")}>
              Open Collections
            </button>
          </div>
          <div className="action-card">
            <span className="action-icon">
              <FlaskConical size={22} />
            </span>
            <h4>Annotate</h4>
            <p>Launch enrichment and annotation workflows using repository signatures.</p>
            <button className="btn btn-primary" onClick={() => navigate("/annotate")}>
              Open Annotate
            </button>
          </div>
        </div>
      </Card>

      <div className="page-footer">
        <span>Created by the SigRepo Team</span>
        <span>·</span>
        <span>Version 1.0 (React mockup)</span>
        <span>·</span>
        <span>© 2026</span>
      </div>
    </div>
  );
}
