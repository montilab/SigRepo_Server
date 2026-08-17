import { useState, type ReactNode } from "react";
import Sidebar from "./Sidebar";

const COLLAPSE_KEY = "sr-sidebar-collapsed";

export default function AppShell({ onLogOut, children }: { onLogOut: () => void; children: ReactNode }) {
  const [collapsed, setCollapsed] = useState(() => {
    try {
      return localStorage.getItem(COLLAPSE_KEY) === "1";
    } catch {
      return false;
    }
  });

  function toggle() {
    setCollapsed((c) => {
      const next = !c;
      try {
        localStorage.setItem(COLLAPSE_KEY, next ? "1" : "0");
      } catch {
        /* ignore storage failures */
      }
      return next;
    });
  }

  return (
    <div className={"app-shell" + (collapsed ? " app-shell-collapsed" : "")}>
      <Sidebar onLogOut={onLogOut} collapsed={collapsed} onToggle={toggle} />
      <main className="app-main">{children}</main>
    </div>
  );
}
