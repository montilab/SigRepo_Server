export interface Stat {
  label: string;
  value: string | number;
}

export default function StatGrid({ stats }: { stats: Stat[] }) {
  return (
    <div className="stat-grid">
      {stats.map((stat) => (
        <div className="stat-card" key={stat.label}>
          <span className="stat-label">{stat.label}</span>
          <span className="stat-value">{stat.value}</span>
        </div>
      ))}
    </div>
  );
}
