import type { ReactNode } from "react";

export default function PageHero({
  gradient,
  title,
  description,
  actions,
}: {
  gradient: string;
  title: string;
  description: string;
  actions?: ReactNode;
}) {
  return (
    <div className="page-hero" style={{ background: gradient }}>
      <h2>{title}</h2>
      <p>{description}</p>
      {actions && <div className="page-hero-actions">{actions}</div>}
    </div>
  );
}
