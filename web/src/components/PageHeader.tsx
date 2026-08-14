import type { ReactNode } from "react";

// Page header, two flavors:
//   variant="hero" (default) -- the blue gradient banner ported from the Shiny
//     app, used on landing-ish pages (Home, etc.).
//   variant="bar" -- a thin, gradient-less title bar (dark text on the page
//     background, a single hairline underneath), for dense/table pages where a
//     tall banner just eats vertical space.
// Same API either way (title / subtitle / actions / optional leading icon).
export default function PageHeader({
  title,
  subtitle,
  actions,
  icon,
  variant = "hero",
}: {
  title: string;
  subtitle?: string;
  actions?: ReactNode;
  icon?: ReactNode;
  variant?: "hero" | "bar";
}) {
  if (variant === "bar") {
    return (
      <header className="page-titlebar">
        <div className="page-titlebar-text">
          {icon && <span className="page-titlebar-icon">{icon}</span>}
          <h1>{title}</h1>
          {subtitle && <span className="page-titlebar-sub">{subtitle}</span>}
        </div>
        {actions && <div className="page-titlebar-actions">{actions}</div>}
      </header>
    );
  }

  return (
    <header className="page-hero">
      <div className="page-hero-text">
        <div className="page-hero-heading">
          {icon && <span className="page-hero-icon">{icon}</span>}
          <h1>{title}</h1>
        </div>
        {subtitle && <p>{subtitle}</p>}
      </div>
      {actions && <div className="page-hero-actions">{actions}</div>}
    </header>
  );
}
