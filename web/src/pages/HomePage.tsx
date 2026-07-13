import { useNavigate } from "react-router-dom";
import { Dna, Layers, FlaskConical, Search, ArrowRight } from "lucide-react";
import { repositorySummary } from "../data/mock";

const actions = [
  {
    icon: Dna,
    title: "Signatures",
    description: "Browse, inspect, and manage repository signatures.",
    to: "/signatures",
  },
  {
    icon: Layers,
    title: "Collections",
    description: "Review grouped signatures and reusable sets.",
    to: "/collections",
  },
  {
    icon: FlaskConical,
    title: "Annotate",
    description: "Run enrichment analysis on a signature.",
    to: "/annotate",
  },
  {
    icon: Search,
    title: "Browsing",
    description: "Search transcriptomic and proteomic references.",
    to: "/browsing",
  },
];

export default function HomePage() {
  const navigate = useNavigate();

  return (
    <div className="page home-page">
      <div className="home-hero-compact">
        <div>
          <h1>Welcome back, cvicnaire</h1>
          <p>Browse, organize, and annotate biological signatures.</p>
        </div>
        <div className="home-hero-compact-actions">
          <button className="btn btn-primary" onClick={() => navigate("/signatures")}>
            Browse Signatures
          </button>
          <button className="btn btn-default" onClick={() => navigate("/collections")}>
            Explore Collections
          </button>
        </div>
      </div>

      <div className="home-stat-strip">
        <div className="home-stat-strip-item">
          <span className="stat-value">{repositorySummary.total_signatures}</span>
          <span className="stat-label">Total Signatures</span>
        </div>
        <div className="home-stat-strip-item">
          <span className="stat-value">{repositorySummary.total_users}</span>
          <span className="stat-label">Active Users</span>
        </div>
        <div className="home-stat-strip-item">
          <span className="stat-value">{repositorySummary.total_organisms}</span>
          <span className="stat-label">Organisms</span>
        </div>
        <div className="home-stat-strip-item">
          <span className="stat-value">{repositorySummary.total_assays}</span>
          <span className="stat-label">Assay Types</span>
        </div>
        <button className="home-stat-strip-link" onClick={() => navigate("/insights")}>
          View Insights <ArrowRight size={14} />
        </button>
      </div>

      <div className="home-actions-row">
        {actions.map((action) => (
          <button className="home-action-tile" key={action.to} onClick={() => navigate(action.to)}>
            <span className="action-icon">
              <action.icon size={20} />
            </span>
            <span className="home-action-tile-text">
              <strong>{action.title}</strong>
              <span>{action.description}</span>
            </span>
          </button>
        ))}
      </div>
    </div>
  );
}
