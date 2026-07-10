import { useState } from "react";
import { BrowserRouter, Routes, Route, Navigate } from "react-router-dom";
import "./App.css";
import Navbar from "./components/Navbar";
import LoginPage from "./pages/LoginPage";
import HomePage from "./pages/HomePage";
import SignaturesPage from "./pages/SignaturesPage";
import CollectionsPage from "./pages/CollectionsPage";
import AnnotatePage from "./pages/AnnotatePage";
import ComparePage from "./pages/ComparePage";
import BrowsingPage from "./pages/BrowsingPage";
import FeedbackPage from "./pages/FeedbackPage";

export default function App() {
  const [loggedIn, setLoggedIn] = useState(false);

  if (!loggedIn) {
    return <LoginPage onLogIn={() => setLoggedIn(true)} />;
  }

  return (
    <BrowserRouter>
      <Navbar onLogOut={() => setLoggedIn(false)} />
      <main className="app-main">
        <Routes>
          <Route path="/" element={<Navigate to="/home" replace />} />
          <Route path="/home" element={<HomePage />} />
          <Route path="/signatures" element={<SignaturesPage />} />
          <Route path="/collections" element={<CollectionsPage />} />
          <Route path="/annotate" element={<AnnotatePage />} />
          <Route path="/compare" element={<ComparePage />} />
          <Route path="/browsing" element={<BrowsingPage />} />
          <Route path="/feedback" element={<FeedbackPage />} />
          <Route path="*" element={<Navigate to="/home" replace />} />
        </Routes>
      </main>
    </BrowserRouter>
  );
}
