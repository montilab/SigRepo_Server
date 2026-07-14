import { useEffect, useState, type ReactNode } from "react";
import Sidebar from "./Sidebar";

export default function AppShell({ onLogOut, children }: { onLogOut: () => void; children: ReactNode }) {
  const [collapsed, setCollapsed] = useState(() => localStorage.getItem("sr-sidebar-collapsed") === "1");

  useEffect(() => {
    localStorage.setItem("sr-sidebar-collapsed", collapsed ? "1" : "0");
  }, [collapsed]);

  return (
    <div className={"app-shell" + (collapsed ? " app-shell-collapsed" : "")}>
      <Sidebar collapsed={collapsed} onToggle={() => setCollapsed((c) => !c)} onLogOut={onLogOut} />
      <main className="app-content">{children}</main>
    </div>
  );
}
