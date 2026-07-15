import { useState } from "react";
import { Eye, EyeOff, ArrowRight, Loader2 } from "lucide-react";
import { login, ApiError } from "../api/client";

export default function LoginPage({ onLogIn }: { onLogIn: () => void }) {
  const [show, setShow] = useState(false);
  const [userName, setUserName] = useState("");
  const [password, setPassword] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);

  async function onSubmit(e: React.FormEvent) {
    e.preventDefault();
    if (busy) return;
    setError(null);
    setBusy(true);
    try {
      await login(userName.trim(), password);
      onLogIn();
    } catch (err) {
      setError(err instanceof ApiError ? err.message : "Could not reach the server.");
      setBusy(false);
    }
  }

  return (
    <div className="login">
      <div className="login-panel">
        <div className="login-brand">
          <div className="brand-mark brand-mark-lg">SR</div>
          <span className="brand-name brand-name-lg">SigRepo</span>
        </div>
        <p className="login-tagline">Browse, organize, and annotate biological signatures.</p>

        <form className="login-form" onSubmit={onSubmit}>
          <label className="field">
            <span className="field-label">Username</span>
            <input
              className="input"
              type="text"
              value={userName}
              onChange={(e) => setUserName(e.target.value)}
              placeholder="Enter username"
              autoFocus
              required
            />
          </label>

          <label className="field">
            <span className="field-label">Password</span>
            <div className="input-affix">
              <input
                className="input"
                type={show ? "text" : "password"}
                value={password}
                onChange={(e) => setPassword(e.target.value)}
                placeholder="Enter password"
                required
              />
              <button type="button" className="input-affix-btn" onClick={() => setShow((s) => !s)}>
                {show ? <EyeOff size={16} /> : <Eye size={16} />}
              </button>
            </div>
          </label>

          {error && <p className="login-error">{error}</p>}

          <button type="submit" className="btn btn-primary btn-block" disabled={busy}>
            {busy ? (
              <>
                <Loader2 size={16} className="spin" /> Signing in…
              </>
            ) : (
              <>
                Sign in <ArrowRight size={16} />
              </>
            )}
          </button>

          <div className="login-links">
            <a href="#">Forgot password?</a>
            <a href="#">Create account</a>
          </div>
        </form>
      </div>
      <p className="login-foot">SigRepo · Montilab</p>
    </div>
  );
}
