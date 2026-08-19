import type { LucideIcon } from "lucide-react";

export default function StatCard({
  label,
  value,
  icon: Icon,
  delta,
}: {
  label: string;
  value: string | number;
  icon?: LucideIcon;
  delta?: { value: string; positive?: boolean };
}) {
  return (
    <div className="stat-card">
      <div className="stat-card-top">
        <span className="stat-card-label">{label}</span>
        {Icon && (
          <span className="stat-card-icon">
            <Icon size={16} />
          </span>
        )}
      </div>
      <div className="stat-card-value">{value}</div>
      {delta && (
        <div className={"stat-card-delta" + (delta.positive ? " up" : " down")}>{delta.value}</div>
      )}
    </div>
  );
}
