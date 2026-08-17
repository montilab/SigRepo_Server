import { useEffect, type ReactNode } from "react";
import { X } from "lucide-react";

export default function Drawer({
  open,
  onClose,
  title,
  subtitle,
  children,
  footer,
  size = "default",
}: {
  open: boolean;
  onClose: () => void;
  title: string;
  subtitle?: string;
  children: ReactNode;
  footer?: ReactNode;
  // "wide" for drawers with dense content (long metadata lists, data
  // tables with many/dynamic columns) that crop or wrap awkwardly at the
  // default width.
  size?: "default" | "wide";
}) {
  useEffect(() => {
    function onKey(e: KeyboardEvent) {
      if (e.key === "Escape") onClose();
    }
    if (open) document.addEventListener("keydown", onKey);
    return () => document.removeEventListener("keydown", onKey);
  }, [open, onClose]);

  return (
    <div className={"drawer-root" + (open ? " drawer-open" : "")} aria-hidden={!open}>
      <div className="drawer-scrim" onClick={onClose} />
      <aside className={"drawer-panel" + (size === "wide" ? " drawer-panel-wide" : "")} role="dialog" aria-modal="true">
        <header className="drawer-head">
          <div>
            <h3 className="drawer-title">{title}</h3>
            {subtitle && <p className="drawer-subtitle">{subtitle}</p>}
          </div>
          <button className="icon-btn" onClick={onClose} title="Close">
            <X size={18} />
          </button>
        </header>
        <div className="drawer-body">{children}</div>
        {footer && <footer className="drawer-foot">{footer}</footer>}
      </aside>
    </div>
  );
}
