import { useState } from "react";
import { BrowserRouter, Routes, Route, Navigate, Outlet } from "react-router-dom";
import "./App.css";
import { getAuth, logout } from "./api/client";
import AppShell from "./components/AppShell";
import LoginPage from "./pages/LoginPage";
import DashboardPage from "./pages/DashboardPage";
import SignaturesPage from "./pages/SignaturesPage";
import SignatureDetailPage from "./pages/SignatureDetailPage";
import CollectionsPage from "./pages/CollectionsPage";
import AnnotatePage from "./pages/AnnotatePage";
import ComparePage from "./pages/ComparePage";
import BrowsePage from "./pages/BrowsePage";
import FeedbackPage from "./pages/FeedbackPage";
import AssistantPage from "./pages/AssistantPage";

// Wraps everything except standalone pages (e.g. the signature detail page,
// meant to be opened in its own tab with no app chrome) in the navbar shell.
function AppLayout({ onLogOut }: { onLogOut: () => void }) {
  return (
    <AppShell onLogOut={onLogOut}>
      <Outlet />
    </AppShell>
  );
}

export default function App() {
  const [loggedIn, setLoggedIn] = useState(() => getAuth() !== null);

  if (!loggedIn) return <LoginPage onLogIn={() => setLoggedIn(true)} />;

  return (
    <BrowserRouter>
      <Routes>
        <Route path="/signatures/:hashkey" element={<SignatureDetailPage />} />
        <Route
          element={
            <AppLayout
              onLogOut={() => {
                logout();
                setLoggedIn(false);
              }}
            />
          }
        >
          <Route path="/" element={<Navigate to="/dashboard" replace />} />
          <Route path="/dashboard" element={<DashboardPage />} />
          <Route path="/signatures" element={<SignaturesPage />} />
          <Route path="/collections" element={<CollectionsPage />} />
          <Route path="/annotate" element={<AnnotatePage />} />
          <Route path="/compare" element={<ComparePage />} />
          <Route path="/assistant" element={<AssistantPage />} />
          <Route path="/browse" element={<BrowsePage />} />
          <Route path="/feedback" element={<FeedbackPage />} />
          <Route path="*" element={<Navigate to="/dashboard" replace />} />
        </Route>
      </Routes>
    </BrowserRouter>
  );
}
