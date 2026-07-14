import { useState } from "react";
import { BrowserRouter, Routes, Route, Navigate } from "react-router-dom";
import "./App.css";
import AppShell from "./components/AppShell";
import LoginPage from "./pages/LoginPage";
import DashboardPage from "./pages/DashboardPage";
import InsightsPage from "./pages/InsightsPage";
import SignaturesPage from "./pages/SignaturesPage";
import CollectionsPage from "./pages/CollectionsPage";
import AnnotatePage from "./pages/AnnotatePage";
import ComparePage from "./pages/ComparePage";
import BrowsePage from "./pages/BrowsePage";
import FeedbackPage from "./pages/FeedbackPage";

export default function App() {
  const [loggedIn, setLoggedIn] = useState(false);

  if (!loggedIn) return <LoginPage onLogIn={() => setLoggedIn(true)} />;

  return (
    <BrowserRouter>
      <AppShell onLogOut={() => setLoggedIn(false)}>
        <Routes>
          <Route path="/" element={<Navigate to="/dashboard" replace />} />
          <Route path="/dashboard" element={<DashboardPage />} />
          <Route path="/insights" element={<InsightsPage />} />
          <Route path="/signatures" element={<SignaturesPage />} />
          <Route path="/collections" element={<CollectionsPage />} />
          <Route path="/annotate" element={<AnnotatePage />} />
          <Route path="/compare" element={<ComparePage />} />
          <Route path="/browse" element={<BrowsePage />} />
          <Route path="/feedback" element={<FeedbackPage />} />
          <Route path="*" element={<Navigate to="/dashboard" replace />} />
        </Routes>
      </AppShell>
    </BrowserRouter>
  );
}
