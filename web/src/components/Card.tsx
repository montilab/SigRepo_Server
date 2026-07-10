import type { ReactNode } from "react";

export default function Card({ title, helper, children }: { title?: string; helper?: string; children: ReactNode }) {
  return (
    <div className="card">
      {title && <h3>{title}</h3>}
      {helper && <p className="card-helper">{helper}</p>}
      {children}
    </div>
  );
}
