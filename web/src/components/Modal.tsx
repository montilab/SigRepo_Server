import { useEffect, useRef, type ReactNode } from "react";
import { X } from "lucide-react";

// A centred modal dialog, for short focused tasks: upload a file, create a
// collection, edit a signature's metadata.
//
// Distinct from Drawer, which slides in from the edge and is for INSPECTING
// something alongside the page it came from (a signature's details while the
// list stays visible behind it). A modal is for a task you finish and dismiss,
// where the page behind is context you do not need.
//
// Replaces the inline expanding panels those forms used to render. An inline
// panel pushes the page content down as it opens, so the table a person was
// reading jumps, and on a short viewport the form's own submit button can open
// below the fold with nothing indicating it is there.
export default function Modal({
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
  // "wide" for forms with side-by-side fields; the default suits a single
  // column of inputs.
  size?: "default" | "wide";
}) {
  const panelRef = useRef<HTMLDivElement | null>(null);
  // Whatever had focus when the modal opened, so it can be handed back on
  // close -- otherwise focus falls to the top of the document and a keyboard
  // user loses their place in the table they opened this from.
  const returnFocusRef = useRef<HTMLElement | null>(null);

  useEffect(() => {
    if (!open) return;

    returnFocusRef.current = document.activeElement as HTMLElement | null;

    function onKey(e: KeyboardEvent) {
      if (e.key === "Escape") {
        onClose();
        return;
      }
      // Keep Tab inside the dialog. Without this the next Tab moves into the
      // page behind, which is inert to the mouse (the scrim covers it) but
      // still reachable by keyboard -- so focus silently leaves the dialog.
      if (e.key !== "Tab" || !panelRef.current) return;
      const focusable = panelRef.current.querySelectorAll<HTMLElement>(
        'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])'
      );
      if (focusable.length === 0) return;
      const first = focusable[0];
      const last = focusable[focusable.length - 1];
      if (e.shiftKey && document.activeElement === first) {
        e.preventDefault();
        last.focus();
      } else if (!e.shiftKey && document.activeElement === last) {
        e.preventDefault();
        first.focus();
      }
    }

    document.addEventListener("keydown", onKey);
    // Move focus in, so Escape and Tab reach the handler above without the
    // user first clicking inside.
    const firstField = panelRef.current?.querySelector<HTMLElement>(
      'input:not([disabled]), select:not([disabled]), textarea:not([disabled]), button:not([disabled])'
    );
    firstField?.focus();

    // The page behind must not scroll while a modal is open -- a wheel gesture
    // over the scrim otherwise moves the list underneath.
    const priorOverflow = document.body.style.overflow;
    document.body.style.overflow = "hidden";

    return () => {
      document.removeEventListener("keydown", onKey);
      document.body.style.overflow = priorOverflow;
      returnFocusRef.current?.focus?.();
    };
  }, [open, onClose]);

  // Unmounted rather than hidden when closed: an inert form left in the tree
  // keeps its inputs in the tab order and its state alive between openings.
  if (!open) return null;

  return (
    <div className="modal-root" role="presentation">
      <div className="modal-scrim" onClick={onClose} />
      <div
        className={"modal-panel" + (size === "wide" ? " modal-panel-wide" : "")}
        role="dialog"
        aria-modal="true"
        aria-label={title}
        ref={panelRef}
      >
        <header className="modal-head">
          <div>
            <h3 className="modal-title">{title}</h3>
            {subtitle && <p className="modal-subtitle">{subtitle}</p>}
          </div>
          <button className="icon-btn" onClick={onClose} title="Close" aria-label="Close">
            <X size={18} />
          </button>
        </header>
        <div className="modal-body">{children}</div>
        {footer && <footer className="modal-foot">{footer}</footer>}
      </div>
    </div>
  );
}
