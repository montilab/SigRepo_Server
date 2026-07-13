import { NavLink } from "react-router-dom";
import { UserCircle, LogOut } from "lucide-react";

const tabs = [
  { to: "/home", label: "Home" },
  { to: "/insights", label: "Insights" },
  { to: "/signatures", label: "Signatures" },
  { to: "/collections", label: "Collections" },
  { to: "/annotate", label: "Annotate" },
  { to: "/compare", label: "Compare" },
  { to: "/browsing", label: "Browsing" },
  { to: "/feedback", label: "Feedback" },
];

export default function Navbar({ onLogOut }: { onLogOut: () => void }) {
  return (
    <nav className="navbar">
      <div className="navbar-inner">
        <div className="navbar-brand">
          <div className="navbar-logo">SR</div>
          <span className="navbar-title">SigRepo</span>
        </div>

        <div className="navbar-tabs">
          {tabs.map((tab) => (
            <NavLink
              key={tab.to}
              to={tab.to}
              className={({ isActive }) => "navbar-tab" + (isActive ? " navbar-tab-active" : "")}
            >
              {tab.label}
            </NavLink>
          ))}
        </div>

        <div className="navbar-user">
          <button className="navbar-icon-btn" title="Edit profile">
            <UserCircle size={20} />
          </button>
          <span className="navbar-welcome">Welcome, cvicnaire</span>
          <button className="navbar-icon-btn navbar-logout" onClick={onLogOut}>
            <LogOut size={16} />
            <span>Log out</span>
          </button>
        </div>
      </div>
    </nav>
  );
}
