import { useState } from "react";
import { Eye, EyeOff, ArrowRight } from "lucide-react";

export default function LoginPage({ onLogIn }: { onLogIn: () => void }) {
  const [show, setShow] = useState(false);

  return (
    <div className="login">
      <div className="login-panel">
        <div className="login-brand">
          <div className="brand-mark brand-mark-lg">SR</div>
          <span className="brand-name brand-name-lg">SigRepo</span>
        </div>
        <p className="login-tagline">Browse, organize, and annotate biological signatures.</p>

        <form
          className="login-form"
          onSubmit={(e) => {
            e.preventDefault();
            onLogIn();
          }}
        >
          <label className="field">
            <span className="field-label">Username</span>
            <input className="input" type="text" defaultValue="cvicnaire" placeholder="Enter username" />
          </label>

          <label className="field">
            <span className="field-label">Password</span>
            <div className="input-affix">
              <input
                className="input"
                type={show ? "text" : "password"}
                defaultValue="password123"
                placeholder="Enter password"
              />
              <button type="button" className="input-affix-btn" onClick={() => setShow((s) => !s)}>
                {show ? <EyeOff size={16} /> : <Eye size={16} />}
              </button>
            </div>
          </label>

          <button type="submit" className="btn btn-primary btn-block">
            Sign in <ArrowRight size={16} />
          </button>

          <div className="login-links">
            <a href="#">Forgot password?</a>
            <a href="#">Create account</a>
          </div>
        </form>
      </div>
      <p className="login-foot">SigRepo · Prototype UI · Montilab</p>
    </div>
  );
}
