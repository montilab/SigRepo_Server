import { NavLink } from "react-router-dom";
import {
  LayoutDashboard,
  BarChart3,
  Dna,
  Layers,
  FlaskConical,
  GitCompare,
  Database,
  MessageSquare,
  LogOut,
  PanelLeftClose,
  PanelLeft,
} from "lucide-react";
import { getAuth } from "../api/client";

const NAV = [
  { to: "/dashboard", label: "Dashboard", icon: LayoutDashboard },
  { to: "/insights", label: "Insights", icon: BarChart3 },
  { to: "/signatures", label: "Signatures", icon: Dna },
  { to: "/collections", label: "Collections", icon: Layers },
  { to: "/annotate", label: "Annotate", icon: FlaskConical },
  { to: "/compare", label: "Compare", icon: GitCompare },
  { to: "/browse", label: "Browse", icon: Database },
  { to: "/feedback", label: "Feedback", icon: MessageSquare },
];

export default function Sidebar({
  collapsed,
  onToggle,
  onLogOut,
}: {
  collapsed: boolean;
  onToggle: () => void;
  onLogOut: () => void;
}) {
  const auth = getAuth();
  const userName = auth?.user_name ?? "Guest";
  const role = auth ? auth.user_role.charAt(0).toUpperCase() + auth.user_role.slice(1) : "";
  const initials = userName.slice(0, 2).toUpperCase();

  return (
    <aside className="sidebar">
      <div className="sidebar-head">
        <div className="brand">
          <div className="brand-mark">SR</div>
          {!collapsed && <span className="brand-name">SigRepo</span>}
        </div>
        <button className="icon-btn sidebar-toggle" onClick={onToggle} title={collapsed ? "Expand" : "Collapse"}>
          {collapsed ? <PanelLeft size={17} /> : <PanelLeftClose size={17} />}
        </button>
      </div>

      <nav className="sidebar-nav">
        {NAV.map(({ to, label, icon: Icon }) => (
          <NavLink
            key={to}
            to={to}
            title={collapsed ? label : undefined}
            className={({ isActive }) => "nav-item" + (isActive ? " nav-item-active" : "")}
          >
            <Icon size={18} className="nav-icon" />
            {!collapsed && <span>{label}</span>}
          </NavLink>
        ))}
      </nav>

      <div className="sidebar-foot">
        <div className="user-chip">
          <div className="avatar">{initials}</div>
          {!collapsed && (
            <div className="user-meta">
              <span className="user-name">{userName}</span>
              <span className="user-role">{role}</span>
            </div>
          )}
          {!collapsed && (
            <button className="icon-btn" onClick={onLogOut} title="Log out">
              <LogOut size={16} />
            </button>
          )}
        </div>
        {collapsed && (
          <button className="icon-btn sidebar-logout-collapsed" onClick={onLogOut} title="Log out">
            <LogOut size={16} />
          </button>
        )}
      </div>
    </aside>
  );
}
