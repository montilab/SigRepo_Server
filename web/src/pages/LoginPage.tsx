import { useState } from "react";
import { Eye, EyeOff, ArrowRight, Loader2, ArrowLeft, CheckCircle2 } from "lucide-react";
import { login, register, requestPasswordReset, ApiError } from "../api/client";

// The panel shows one of three things. Registration and reset were part of the
// Shiny portal and are reproduced here: same fields, same rules, and the same
// "an administrator activates your account" flow -- a new account is created
// inactive and cannot sign in until it is approved.
type View = "login" | "register" | "forgot";

function errorText(err: unknown) {
  return err instanceof ApiError ? err.message : "Could not reach the server.";
}

export default function LoginPage({ onLogIn }: { onLogIn: () => void }) {
  const [view, setView] = useState<View>("login");

  return (
    <div className="login">
      <div className="login-panel">
        <div className="login-brand">
          <div className="brand-mark brand-mark-lg">SR</div>
          <span className="brand-name brand-name-lg">SigRepo</span>
        </div>
        <p className="login-tagline">
          {view === "register"
            ? "Request an account. An administrator activates it before you can sign in."
            : view === "forgot"
              ? "We'll email a temporary password to the address on your account."
              : "Browse, organize, and annotate biological signatures."}
        </p>

        {view === "login" && <SignInForm onLogIn={onLogIn} onNavigate={setView} />}
        {view === "register" && <RegisterForm onBack={() => setView("login")} />}
        {view === "forgot" && <ForgotForm onBack={() => setView("login")} />}
      </div>
      <p className="login-foot">SigRepo · Montilab</p>
    </div>
  );
}

function SignInForm({
  onLogIn,
  onNavigate,
}: {
  onLogIn: () => void;
  onNavigate: (v: View) => void;
}) {
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
      setError(errorText(err));
      setBusy(false);
    }
  }

  return (
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
          <button
            type="button"
            className="input-affix-btn"
            onClick={() => setShow((s) => !s)}
            aria-label={show ? "Hide password" : "Show password"}
          >
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
        <button type="button" className="link-btn" onClick={() => onNavigate("forgot")}>
          Forgot password?
        </button>
        <button type="button" className="link-btn" onClick={() => onNavigate("register")}>
          Create account
        </button>
      </div>
    </form>
  );
}

function RegisterForm({ onBack }: { onBack: () => void }) {
  const [show, setShow] = useState(false);
  const [f, setF] = useState({
    userName: "",
    password: "",
    email: "",
    firstName: "",
    lastName: "",
    affiliation: "",
  });
  const [error, setError] = useState<string | null>(null);
  const [done, setDone] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);

  const set = (k: keyof typeof f) => (e: React.ChangeEvent<HTMLInputElement>) =>
    setF((prev) => ({ ...prev, [k]: e.target.value }));

  async function onSubmit(e: React.FormEvent) {
    e.preventDefault();
    if (busy) return;
    setError(null);
    setBusy(true);
    try {
      const message = await register({
        userName: f.userName.trim(),
        password: f.password,
        email: f.email.trim(),
        firstName: f.firstName.trim(),
        lastName: f.lastName.trim(),
        affiliation: f.affiliation.trim(),
      });
      setDone(message);
    } catch (err) {
      setError(errorText(err));
    } finally {
      setBusy(false);
    }
  }

  // The account exists but is inactive at this point, so there is nothing for
  // the user to do here. Show the server's message and send them back rather
  // than leaving a filled-in form that would re-submit into a duplicate error.
  if (done) {
    return (
      <div className="login-form">
        <p className="login-success">
          <CheckCircle2 size={16} /> {done}
        </p>
        <button type="button" className="btn btn-primary btn-block" onClick={onBack}>
          Back to sign in
        </button>
      </div>
    );
  }

  return (
    <form className="login-form" onSubmit={onSubmit}>
      <label className="field">
        <span className="field-label">Username</span>
        <input className="input" type="text" value={f.userName} onChange={set("userName")}
               placeholder="Choose a username" autoFocus required />
      </label>

      <label className="field">
        <span className="field-label">Password</span>
        <div className="input-affix">
          <input className="input" type={show ? "text" : "password"} value={f.password}
                 onChange={set("password")} placeholder="Choose a password" required />
          <button type="button" className="input-affix-btn" onClick={() => setShow((s) => !s)}
                  aria-label={show ? "Hide password" : "Show password"}>
            {show ? <EyeOff size={16} /> : <Eye size={16} />}
          </button>
        </div>
      </label>

      <label className="field">
        <span className="field-label">Email</span>
        <input className="input" type="email" value={f.email} onChange={set("email")}
               placeholder="you@institution.edu" required />
      </label>

      <div className="field-row">
        <label className="field">
          <span className="field-label">First name</span>
          <input className="input" type="text" value={f.firstName} onChange={set("firstName")} />
        </label>
        <label className="field">
          <span className="field-label">Last name</span>
          <input className="input" type="text" value={f.lastName} onChange={set("lastName")} />
        </label>
      </div>

      <label className="field">
        <span className="field-label">Affiliation</span>
        <input className="input" type="text" value={f.affiliation} onChange={set("affiliation")}
               placeholder="Institution or lab" />
      </label>

      {error && <p className="login-error">{error}</p>}

      <button type="submit" className="btn btn-primary btn-block" disabled={busy}>
        {busy ? (
          <>
            <Loader2 size={16} className="spin" /> Submitting…
          </>
        ) : (
          <>
            Request account <ArrowRight size={16} />
          </>
        )}
      </button>

      <div className="login-links login-links-single">
        <button type="button" className="link-btn" onClick={onBack}>
          <ArrowLeft size={14} /> Back to sign in
        </button>
      </div>
    </form>
  );
}

function ForgotForm({ onBack }: { onBack: () => void }) {
  const [identifier, setIdentifier] = useState("");
  const [error, setError] = useState<string | null>(null);
  const [done, setDone] = useState<string | null>(null);
  const [busy, setBusy] = useState(false);

  async function onSubmit(e: React.FormEvent) {
    e.preventDefault();
    if (busy) return;
    setError(null);
    setBusy(true);
    try {
      setDone(await requestPasswordReset(identifier.trim()));
    } catch (err) {
      setError(errorText(err));
    } finally {
      setBusy(false);
    }
  }

  if (done) {
    return (
      <div className="login-form">
        <p className="login-success">
          <CheckCircle2 size={16} /> {done}
        </p>
        <button type="button" className="btn btn-primary btn-block" onClick={onBack}>
          Back to sign in
        </button>
      </div>
    );
  }

  return (
    <form className="login-form" onSubmit={onSubmit}>
      <label className="field">
        <span className="field-label">Username or email</span>
        <input className="input" type="text" value={identifier}
               onChange={(e) => setIdentifier(e.target.value)}
               placeholder="Enter either one" autoFocus required />
      </label>

      {error && <p className="login-error">{error}</p>}

      <button type="submit" className="btn btn-primary btn-block" disabled={busy}>
        {busy ? (
          <>
            <Loader2 size={16} className="spin" /> Sending…
          </>
        ) : (
          <>
            Send temporary password <ArrowRight size={16} />
          </>
        )}
      </button>

      <div className="login-links login-links-single">
        <button type="button" className="link-btn" onClick={onBack}>
          <ArrowLeft size={14} /> Back to sign in
        </button>
      </div>
    </form>
  );
}
