import type { ReactNode } from "react";

export default function Card({
  title,
  subtitle,
  actions,
  padded = true,
  children,
  className,
}: {
  title?: string;
  subtitle?: string;
  actions?: ReactNode;
  padded?: boolean;
  children: ReactNode;
  className?: string;
}) {
  return (
    <section className={"card" + (className ? " " + className : "")}>
      {(title || actions) && (
        <div className="card-head">
          <div>
            {title && <h3 className="card-title">{title}</h3>}
            {subtitle && <p className="card-subtitle">{subtitle}</p>}
          </div>
          {actions && <div className="card-actions">{actions}</div>}
        </div>
      )}
      <div className={padded ? "card-body" : "card-body card-body-flush"}>{children}</div>
    </section>
  );
}
