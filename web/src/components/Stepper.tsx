import { Check } from "lucide-react";

export default function Stepper({ steps, current }: { steps: string[]; current: number }) {
  return (
    <ol className="stepper">
      {steps.map((step, i) => {
        const state = i < current ? "done" : i === current ? "active" : "todo";
        return (
          <li key={step} className={"step step-" + state}>
            <span className="step-dot">{state === "done" ? <Check size={13} /> : i + 1}</span>
            <span className="step-label">{step}</span>
            {i < steps.length - 1 && <span className="step-line" />}
          </li>
        );
      })}
    </ol>
  );
}
