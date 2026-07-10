import { useState } from "react";
import { Eye, EyeOff } from "lucide-react";

export default function LoginPage({ onLogIn }: { onLogIn: () => void }) {
  const [showPassword, setShowPassword] = useState(false);

  return (
    <div className="login-wrapper">
      <div className="login-container">
        <div className="login-form-title">
          <h1>Sign In</h1>
        </div>

        <form
          className="login-form"
          onSubmit={(e) => {
            e.preventDefault();
            onLogIn();
          }}
        >
          <div className="validate-input">
            <label className="login-label">Username</label>
            <input className="login-input" type="text" placeholder="Enter Username" defaultValue="cvicnaire" />
          </div>

          <div className="validate-input">
            <label className="login-label">Password</label>
            <div className="password-container">
              <input
                className="login-input"
                type={showPassword ? "text" : "password"}
                placeholder="Enter Password"
                defaultValue="password123"
              />
              <span className="toggle-password" onClick={() => setShowPassword((s) => !s)}>
                {showPassword ? <EyeOff size={18} /> : <Eye size={18} />}
              </span>
            </div>
          </div>

          <div className="validate-button">
            <button type="submit" className="sign-in-button">
              Login
            </button>
            <a href="#" className="forgot-psw">
              Forgot password?
            </a>
          </div>

          <div className="register">
            <span>
              Don't have an account? <a href="#">Register here!</a>
            </span>
          </div>
        </form>
      </div>
    </div>
  );
}
