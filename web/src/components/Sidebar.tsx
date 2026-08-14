import { NavLink } from "react-router-dom";
import {
  LayoutDashboard,
  Dna,
  Layers,
  FlaskConical,
  GitCompare,
  Database,
  MessageSquare,
  LogOut,
  PanelLeftClose,
  PanelLeftOpen,
} from "lucide-react";
import { getAuth } from "../api/client";

const NAV = [
  { to: "/dashboard", label: "Home", icon: LayoutDashboard },
  { to: "/signatures", label: "Signatures", icon: Dna },
  { to: "/collections", label: "Collections", icon: Layers },
  { to: "/annotate", label: "Annotate", icon: FlaskConical },
  { to: "/compare", label: "Compare", icon: GitCompare },
  { to: "/browse", label: "Browsing", icon: Database },
  { to: "/feedback", label: "Feedback", icon: MessageSquare },
];

export default function Sidebar({
  onLogOut,
  collapsed,
  onToggle,
}: {
  onLogOut: () => void;
  collapsed: boolean;
  onToggle: () => void;
}) {
  const auth = getAuth();
  const userName = auth?.user_name ?? "Guest";
  const role = auth ? auth.user_role.charAt(0).toUpperCase() + auth.user_role.slice(1) : "";
  const initials = userName.slice(0, 2).toUpperCase();

  return (
    <aside className={"sidebar" + (collapsed ? " sidebar-collapsed" : "")}>
      <div className="sidebar-top">
        <div className="brand">
          <div className="brand-mark">SR</div>
          {!collapsed && <span className="brand-name">SigRepo</span>}
        </div>
        <button className="sidebar-toggle" onClick={onToggle} title={collapsed ? "Expand sidebar" : "Collapse sidebar"}>
          {collapsed ? <PanelLeftOpen size={18} /> : <PanelLeftClose size={18} />}
        </button>
      </div>

      <nav className="sidebar-nav">
        {NAV.map(({ to, label, icon: Icon }) => (
          <NavLink
            key={to}
            to={to}
            className={({ isActive }) => "sidebar-item" + (isActive ? " sidebar-item-active" : "")}
            title={collapsed ? label : undefined}
          >
            <Icon size={18} className="sidebar-icon" />
            {!collapsed && <span>{label}</span>}
          </NavLink>
        ))}
      </nav>

      <div className="sidebar-user">
        <div className="avatar">{initials}</div>
        {!collapsed && (
          <div className="user-meta">
            <span className="user-name">{userName}</span>
            <span className="user-role">{role}</span>
          </div>
        )}
        <button className="icon-btn" onClick={onLogOut} title="Log out">
          <LogOut size={16} />
        </button>
      </div>
    </aside>
  );
}
