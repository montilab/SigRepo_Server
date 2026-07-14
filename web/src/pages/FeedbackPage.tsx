import { useState } from "react";
import { Send, CheckCircle2 } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";

export default function FeedbackPage() {
  const [sent, setSent] = useState(false);

  return (
    <div className="page page-narrow">
      <PageHeader title="Feedback" subtitle="Tell us what's working and what isn't." />

      <Card>
        {sent ? (
          <div className="feedback-sent">
            <CheckCircle2 size={22} className="feedback-sent-icon" />
            <div>
              <strong>Thanks for the feedback!</strong>
              <p>We've received your message and will follow up if needed.</p>
            </div>
          </div>
        ) : (
          <form
            onSubmit={(e) => {
              e.preventDefault();
              setSent(true);
            }}
          >
            <label className="field">
              <span className="field-label">Your name</span>
              <input className="input" placeholder="Optional" />
            </label>
            <label className="field">
              <span className="field-label">Feedback</span>
              <textarea className="input" rows={6} placeholder="Share a bug, request, or idea…" required />
            </label>
            <button type="submit" className="btn btn-primary">
              <Send size={15} /> Send feedback
            </button>
          </form>
        )}
      </Card>
    </div>
  );
}
