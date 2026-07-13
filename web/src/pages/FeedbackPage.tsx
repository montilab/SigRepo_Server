import { useState } from "react";
import Card from "../components/Card";

export default function FeedbackPage() {
  const [submitted, setSubmitted] = useState(false);

  return (
    <div className="page" style={{ paddingTop: 40 }}>
      <Card title="Feedback">
        <form
          onSubmit={(e) => {
            e.preventDefault();
            setSubmitted(true);
          }}
        >
          <label className="field">
            <span>Your Name</span>
            <input className="select-input" placeholder="Your Name" />
          </label>
          <label className="field" style={{ marginTop: 12 }}>
            <span>Your Feedback</span>
            <textarea className="select-input" rows={5} placeholder="Your Feedback" />
          </label>
          <button className="btn btn-primary" type="submit" style={{ marginTop: 14 }}>
            Submit Feedback
          </button>
          {submitted && <p className="card-helper" style={{ marginTop: 10 }}>Feedback sent successfully!</p>}
        </form>
      </Card>
    </div>
  );
}
