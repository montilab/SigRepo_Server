import { useEffect, useState, type ReactNode } from "react";
import { useNavigate, useParams } from "react-router-dom";
import { X, Download, Trash2, ShoppingBasket, Copy } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import {
  getSignatureContext,
  deleteSignature,
  getDifexp,
  downloadSignatureExport,
  type SignatureContext,
  type SignatureSummary,
  type DifexpResult,
} from "../api/client";
import Skeleton from "../components/Skeleton";
import RummagenePanel from "../components/RummagenePanel";
import RelatedSignaturesPanel from "../components/RelatedSignaturesPanel";
import LincsPanel from "../components/LincsPanel";
import { addToBasket, isInBasket } from "../basket";
import { canDeleteSignature } from "../permissions";

function formatLabel(key: string): string {
  return key.replace(/_/g, " ").replace(/\b\w/g, (c) => c.toUpperCase());
}

function formatValue(value: unknown): string {
  if (value === null || value === undefined || value === "") return "—";
  return String(value);
}

function hasValue(value: unknown): boolean {
  return value !== null && value !== undefined && value !== "";
}

// Up/down regulated proportion bar (green up, red down) with counts.
function SplitBar({ up, down }: { up: number | null; down: number | null }) {
  const u = up ?? 0;
  const d = down ?? 0;
  const total = u + d;
  const upPct = total > 0 ? (u / total) * 100 : 0;
  const downPct = total > 0 ? (d / total) * 100 : 0;
  return (
    <div className="sig-split">
      <div className="sig-split-legend">
        <span className="sig-split-up">▲ {up ?? "—"} up</span>
        <span className="sig-split-down">{down ?? "—"} down ▼</span>
      </div>
      <div className="sig-split-bar">
        <span className="sig-split-bar-up" style={{ width: `${upPct}%` }} />
        <span className="sig-split-bar-down" style={{ width: `${downPct}%` }} />
      </div>
    </div>
  );
}

interface MetadataField {
  key: string;
  label: string;
  render?: (value: unknown) => ReactNode;
}

// Splits a comma/semicolon-separated field ("aging, senescence, liver") into
// individual pill badges instead of showing one long comma string.
function pills(value: unknown): ReactNode {
  const parts = String(value ?? "")
    .split(/[,;]/)
    .map((s) => s.trim())
    .filter(Boolean);
  if (parts.length === 0) return "—";
  return (
    <div className="pill-list">
      {parts.map((p) => (
        <span className="pill" key={p}>
          {p}
        </span>
      ))}
    </div>
  );
}

// Groups the raw signature row (every column of `signatures`, plus the
// joined organism/phenotype/sample_type/platform_name) into labeled
// sections instead of one flat alphabetical dump. Excludes:
//  - signature_id/organism_id/phenotype_id/platform_id/sample_type_id --
//    internal foreign keys; the joined human-readable columns replace them.
//  - signature_name/assay_type -- already shown in the page title/subtitle.
//  - signature_hashkey -- an internal lookup key, not metadata; shown as a
//    small reference line under the title instead.
// Fields with no value are dropped individually, and a whole section is
// skipped if none of its fields have a value.
// Organism/phenotype/sample/platform/year/direction and the feature/up-down
// stats are surfaced in the summary band up top, so they're intentionally
// omitted from these detail sections to avoid repeating them.
const METADATA_SECTIONS: { title: string; fields: MetadataField[] }[] = [
  {
    title: "Cutoffs & Thresholds",
    fields: [
      { key: "score_cutoff", label: "Score Cutoff" },
      { key: "logfc_cutoff", label: "LogFC Cutoff" },
      { key: "p_value_cutoff", label: "P-value Cutoff" },
      { key: "adj_p_cutoff", label: "Adj. P Cutoff" },
      { key: "cutoff_description", label: "Cutoff Description" },
    ],
  },
  {
    title: "Differential Expression",
    fields: [
      {
        key: "has_difexp",
        label: "Has Difexp",
        render: (v) => <Badge tone={v === 1 ? "success" : "neutral"}>{v === 1 ? "Yes" : "No"}</Badge>,
      },
      { key: "num_of_difexp", label: "Number of Difexp" },
    ],
  },
  {
    title: "Details",
    fields: [
      { key: "covariates", label: "Covariates", render: pills },
      { key: "keywords", label: "Keywords", render: pills },
      { key: "PMID", label: "PMID" },
      { key: "others", label: "Others" },
    ],
  },
  {
    title: "Provenance",
    fields: [
      { key: "user_name", label: "Owner" },
      { key: "date_created", label: "Created" },
      {
        key: "visibility",
        label: "Visibility",
        render: (v) => <Badge tone={v === 1 ? "success" : "neutral"}>{v === 1 ? "Public" : "Private"}</Badge>,
      },
    ],
  },
];

export default function SignatureDetailPage() {
  const { hashkey } = useParams<{ hashkey: string }>();
  const navigate = useNavigate();

  const [context, setContext] = useState<SignatureContext | null>(null);
  const [contextLoading, setContextLoading] = useState(true);
  const [contextError, setContextError] = useState<string | null>(null);

  const [tab, setTab] = useState<"signature" | "difexp">("signature");
  const [difexp, setDifexp] = useState<DifexpResult | null>(null);
  const [difexpLoading, setDifexpLoading] = useState(false);
  const [difexpError, setDifexpError] = useState<string | null>(null);

  const [deleting, setDeleting] = useState(false);
  const [deleteError, setDeleteError] = useState<string | null>(null);
  const [exporting, setExporting] = useState(false);
  const [exportError, setExportError] = useState<string | null>(null);

  useEffect(() => {
    if (!hashkey) return;
    let cancelled = false;
    setContextLoading(true);
    setContextError(null);
    getSignatureContext(hashkey)
      .then((ctx) => {
        if (!cancelled) setContext(ctx);
      })
      .catch((err) => {
        if (!cancelled) setContextError(err instanceof Error ? err.message : "Could not load signature.");
      })
      .finally(() => {
        if (!cancelled) setContextLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [hashkey]);

  async function handleLoadDifexp() {
    if (!hashkey) return;
    setDifexpLoading(true);
    setDifexpError(null);
    try {
      setDifexp(await getDifexp(hashkey));
    } catch (err) {
      setDifexpError(err instanceof Error ? err.message : "Could not load difexp.");
    } finally {
      setDifexpLoading(false);
    }
  }

  async function handleDelete() {
    if (!hashkey || !context) return;
    const name = String(context.signature.signature_name ?? "this signature");
    if (!window.confirm(`Delete "${name}"? This cannot be undone.`)) return;
    setDeleting(true);
    setDeleteError(null);
    try {
      await deleteSignature(hashkey);
      navigate("/signatures");
    } catch (err) {
      setDeleteError(err instanceof Error ? err.message : "Could not delete signature.");
      setDeleting(false);
    }
  }

  async function handleExport() {
    if (!hashkey) return;
    setExporting(true);
    setExportError(null);
    try {
      await downloadSignatureExport(hashkey);
    } catch (err) {
      setExportError(err instanceof Error ? err.message : "Could not export signature.");
    } finally {
      setExporting(false);
    }
  }

  const difexpColumns = difexp && difexp.rows.length > 0 ? Object.keys(difexp.rows[0]) : [];

  const signatureName = context ? String(context.signature.signature_name ?? hashkey) : hashkey ?? "";
  const organism = context ? (context.signature.organism as string | null) : null;
  const assayType = context ? String(context.signature.assay_type ?? "") : "";
  const ownerUserName = context ? String(context.signature.user_name ?? "") : "";

  const sig = context?.signature;
  const num = (v: unknown): number | null => {
    const n = Number(v);
    return Number.isFinite(n) ? n : null;
  };
  const featureCount = context ? context.feature_count ?? num(sig?.feature_count) : null;
  const upCount = num(sig?.num_up_regulated);
  const downCount = num(sig?.num_down_regulated);
  const description = sig && hasValue(sig.description) ? String(sig.description) : null;
  // Categorical facts shown as chips in the summary band.
  const chips: { label: string; value: unknown }[] = sig
    ? [
        { label: "Organism", value: sig.organism },
        { label: "Phenotype", value: sig.phenotype },
        { label: "Sample", value: sig.sample_type },
        { label: "Platform", value: sig.platform_name },
        { label: "Direction", value: sig.direction_type },
        { label: "Year", value: sig.year },
      ].filter((c) => hasValue(c.value))
    : [];

  async function copyHashkey() {
    try {
      await navigator.clipboard.writeText(String(sig?.signature_hashkey ?? hashkey ?? ""));
    } catch {
      /* clipboard blocked -- no-op */
    }
  }

  return (
    <div className="page">
      <PageHeader
        title={contextLoading ? "Loading…" : signatureName}
        subtitle={context ? `${organism ?? "—"} · ${assayType}` : undefined}
        actions={
          <>
            <button className="btn btn-secondary" onClick={() => window.close()} title="Close this tab">
              <X size={15} /> Close
            </button>
            {context && canDeleteSignature(ownerUserName) && (
              <button className="btn btn-secondary" onClick={handleDelete} disabled={deleting}>
                <Trash2 size={15} /> {deleting ? "Deleting…" : "Delete"}
              </button>
            )}
            {context && (
              <button className="btn btn-secondary" onClick={handleExport} disabled={exporting}>
                <Download size={15} /> {exporting ? "Exporting…" : "Export"}
              </button>
            )}
            {context && (
              <button
                className="btn btn-secondary"
                onClick={() => addToBasket(context.signature as unknown as SignatureSummary)}
                disabled={isInBasket(hashkey ?? "")}
              >
                <ShoppingBasket size={15} /> {isInBasket(hashkey ?? "") ? "In Basket" : "Add to Basket"}
              </button>
            )}
            {context && <button className="btn btn-primary">Run enrichment</button>}
          </>
        }
      />

      {contextError && (
        <Card>
          <div className="empty-state">{contextError}</div>
        </Card>
      )}

      {contextLoading && (
        <div className="sig-overview">
          <Skeleton width={180} height={22} radius={999} />
          <Skeleton width="70%" height={16} />
          <div className="sig-chips">
            {Array.from({ length: 5 }).map((_, i) => (
              <Skeleton key={i} width={96} height={30} radius={999} />
            ))}
          </div>
          <Skeleton width="100%" height={72} radius={8} />
          <div className="detail-page-grid">
            <Skeleton width="100%" height={220} radius={8} />
            <Skeleton width="100%" height={220} radius={8} />
          </div>
        </div>
      )}

      {!contextLoading && context && (
        <>
          {deleteError && <p className="login-error">{deleteError}</p>}
          {exportError && <p className="login-error">{exportError}</p>}

          <div className="segmented" style={{ marginBottom: 4 }}>
            <button
              className={"segmented-btn" + (tab === "signature" ? " segmented-btn-active" : "")}
              onClick={() => setTab("signature")}
            >
              Signature
            </button>
            <button
              className={"segmented-btn" + (tab === "difexp" ? " segmented-btn-active" : "")}
              onClick={() => setTab("difexp")}
            >
              Difexp
            </button>
          </div>

          {tab === "signature" && (
            <>
              <div className="sig-overview">
                <button className="sig-hashline" onClick={copyHashkey} title="Copy hashkey">
                  <Copy size={12} /> {String(context.signature.signature_hashkey ?? "")}
                </button>
                {description && <p className="sig-lead">{description}</p>}
                {chips.length > 0 && (
                  <div className="sig-chips">
                    {chips.map((c) => (
                      <span className="sig-chip" key={c.label}>
                        <span className="sig-chip-label">{c.label}</span>
                        <span className="sig-chip-value">{String(c.value)}</span>
                      </span>
                    ))}
                  </div>
                )}
                {(featureCount != null || upCount != null || downCount != null) && (
                  <div className="sig-splitcard">
                    <div className="sig-stat-big">
                      <span className="sig-stat-value">{featureCount ?? "—"}</span>
                      <span className="sig-stat-label">Features</span>
                    </div>
                    <SplitBar up={upCount} down={downCount} />
                  </div>
                )}
              </div>

              <div className="detail-page-grid">
                <div>
                  {METADATA_SECTIONS.map((section) => {
                  const fields = section.fields.filter((f) => hasValue(context.signature[f.key]));
                  if (fields.length === 0) return null;
                  return (
                    <div key={section.title} className="detail-section">
                      <h4 className="detail-section-title">{section.title}</h4>
                      <dl className="detail-list">
                        {fields.map((f) => (
                          <div key={f.key}>
                            <dt>{f.label}</dt>
                            <dd>{f.render ? f.render(context.signature[f.key]) : formatValue(context.signature[f.key])}</dd>
                          </div>
                        ))}
                      </dl>
                    </div>
                  );
                })}
              </div>

              <div className="detail-section">
                <h4 className="detail-section-title">Top features</h4>
                {context.features.length > 0 && (
                  <div className="dt-scroll dt-scroll-bounded" style={{ maxHeight: 560 }}>
                    <table className="dt-table dt-table-flush dt-table-compact">
                      <thead>
                        <tr>
                          <th>Feature</th>
                          <th className="dt-right">Score</th>
                          <th className="dt-right">Direction</th>
                        </tr>
                      </thead>
                      <tbody>
                        {context.features.map((f, i) => {
                          const score = typeof f.score === "number" ? f.score : Number(f.score);
                          const label = f.probe_id ?? String(f.feature_id ?? i);
                          return (
                            <tr key={label}>
                              <td className="cell-strong">{label}</td>
                              <td className="dt-right cell-mono">{Number.isFinite(score) ? score.toFixed(2) : "—"}</td>
                              <td className="dt-right">
                                <Badge tone={score >= 0 ? "success" : "danger"}>{score >= 0 ? "Up" : "Down"}</Badge>
                              </td>
                            </tr>
                          );
                        })}
                      </tbody>
                    </table>
                  </div>
                )}
                {context.features.length === 0 && (
                  <p className="cell-sub" style={{ padding: "10px 14px" }}>No features recorded for this signature.</p>
                )}
              </div>
              </div>

              {hashkey && <RelatedSignaturesPanel signatureHashkey={hashkey} />}
              {hashkey && <RummagenePanel signatureHashkey={hashkey} />}
              {hashkey && <LincsPanel signatureHashkey={hashkey} />}
            </>
          )}

          {tab === "difexp" && (
            <Card>
              {!difexp && (
                <button className="btn btn-secondary" onClick={handleLoadDifexp} disabled={difexpLoading}>
                  {difexpLoading ? "Loading difexp…" : "Load difexp"}
                </button>
              )}
              {difexpError && <p className="login-error">{difexpError}</p>}
              {difexp && difexp.message && <p className="cell-sub">{difexp.message}</p>}
              {difexp && difexp.rows.length > 0 && (
                <div className="detail-section" style={{ marginTop: 12 }}>
                  <div className="dt-scroll dt-scroll-bounded" style={{ maxHeight: 560 }}>
                    <table className="dt-table dt-table-flush dt-table-compact">
                      <thead>
                        <tr>
                          {difexpColumns.map((col) => (
                            <th key={col}>{formatLabel(col)}</th>
                          ))}
                        </tr>
                      </thead>
                      <tbody>
                        {difexp.rows.map((row, i) => (
                          <tr key={i}>
                            {difexpColumns.map((col) => (
                              <td key={col} className="cell-mono">
                                {formatValue(row[col])}
                              </td>
                            ))}
                          </tr>
                        ))}
                      </tbody>
                    </table>
                  </div>
                </div>
              )}
            </Card>
          )}
        </>
      )}
    </div>
  );
}
