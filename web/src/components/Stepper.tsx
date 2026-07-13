import { Check } from "lucide-react";

export default function Stepper({ steps, current }: { steps: string[]; current: number }) {
  return (
    <div className="stepper">
      {steps.map((step, i) => {
        const state = i < current ? "done" : i === current ? "active" : "upcoming";
        return (
          <div className="stepper-item" key={step}>
            <div className={"stepper-dot stepper-dot-" + state}>{state === "done" ? <Check size={14} /> : i + 1}</div>
            <span className={"stepper-label stepper-label-" + state}>{step}</span>
            {i < steps.length - 1 && <div className={"stepper-line stepper-line-" + (i < current ? "done" : "upcoming")} />}
          </div>
        );
      })}
    </div>
  );
}
