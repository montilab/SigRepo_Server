import { useEffect, useMemo, useState, type ReactNode } from "react";
import { useNavigate, useParams } from "react-router-dom";
import { X, Download, Trash2, ShoppingBasket, Copy, Pencil } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Modal from "../components/Modal";
import Badge from "../components/Badge";
import DataTable, { type Column } from "../components/DataTable";
import {
  getSignatureContext,
  deleteSignature,
  getDifexp,
  downloadSignatureExport,
  updateSignature,
  type SignatureContext,
  type SignatureSummary,
  type DifexpResult,
} from "../api/client";
import Skeleton from "../components/Skeleton";
import RummagenePanel from "../components/RummagenePanel";
import RelatedSignaturesPanel from "../components/RelatedSignaturesPanel";
import { addToBasket, isInBasket } from "../basket";
import { canDeleteSignature, canEditSignature } from "../permissions";

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

// `others` is stored as "key: value; key: value" -- the form
// SigRepo:::parseRetrievedOthers() reads back. Rendered as one string it is a
// wall of wrapped text in a narrow column, and the provenance of a pulled
// signature (which paper, which MeSH descriptors attested it) is exactly the
// part a reader most wants and can least easily pick out. Split it back into
// the pairs it already is. A segment with no ":" is kept whole rather than
// dropped, so a value written in some older format still shows up.
function othersRows(value: unknown) {
  const raw = typeof value === "string" ? value.trim() : "";
  if (!raw) return <span className="cell-sub">—</span>;

  const rows = raw.split(";").map((seg) => {
    const s = seg.trim();
    const at = s.indexOf(":");
    return at === -1
      ? { key: null as string | null, value: s }
      : { key: s.slice(0, at).trim(), value: s.slice(at + 1).trim() };
  }).filter((r) => r.value || r.key);

  return (
    <dl className="sig-others">
      {rows.map((r, i) => (
        <div className="sig-others-row" key={`${r.key ?? "note"}-${i}`}>
          {r.key && <dt>{r.key.replace(/_/g, " ")}</dt>}
          <dd>{r.value}</dd>
        </div>
      ))}
    </dl>
  );
}

// A signature can carry placeholder vocabulary rather than a real value:
// phenotype "unknown" and sample/platform "Unknown" are what a source that
// states neither resolves to. Showing them as chips fills the summary band
// with noise that reads like information. Omitting them says the same thing
// more honestly -- the field simply is not there.
function isUnknown(value: unknown): boolean {
  return typeof value === "string" && value.trim().toLowerCase() === "unknown";
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
      { key: "others", label: "Others", render: othersRows },
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

  // ---------- Edit metadata (owner or admin) ----------
  // A PATCH: only fields the form actually changed are sent, so a column the
  // user did not touch is never rewritten. The server enforces the same
  // whitelist -- this form is convenience, not the authority.
  const [showEdit, setShowEdit] = useState(false);
  const [editDescription, setEditDescription] = useState("");
  const [editKeywords, setEditKeywords] = useState("");
  const [editPhenotype, setEditPhenotype] = useState("");
  const [editYear, setEditYear] = useState("");
  const [editPublic, setEditPublic] = useState(false);
  const [saving, setSaving] = useState(false);
  const [editError, setEditError] = useState<string | null>(null);

  function openEdit() {
    const sig = context?.signature ?? {};
    setEditDescription(typeof sig.description === "string" ? sig.description : "");
    setEditKeywords(typeof sig.keywords === "string" ? sig.keywords : "");
    // "unknown" is a placeholder the repository stores when a source states no
    // phenotype; show it as empty so the field reads as unset rather than as a
    // value the user has to delete before typing a real one.
    setEditPhenotype(
      typeof sig.phenotype === "string" && !isUnknown(sig.phenotype) ? sig.phenotype : ""
    );
    setEditYear(sig.year == null ? "" : String(sig.year));
    setEditPublic(Number(sig.visibility) === 1);
    setEditError(null);
    setShowEdit(true);
  }

  function closeEdit() {
    setShowEdit(false);
    setEditError(null);
  }

  async function handleSaveEdit() {
    if (!hashkey) return;
    setSaving(true);
    setEditError(null);
    try {
      const sig = context?.signature ?? {};
      const fields: Record<string, unknown> = {};
      // Send only what changed. Sending everything would rewrite columns the
      // user never looked at, and would blank a field the form renders as ""
      // but the database holds as NULL.
      if (editDescription !== (sig.description ?? "")) fields.description = editDescription;
      if (editKeywords !== (sig.keywords ?? "")) fields.keywords = editKeywords;
      const priorPhenotype =
        typeof sig.phenotype === "string" && !isUnknown(sig.phenotype) ? sig.phenotype : "";
      if (editPhenotype !== priorPhenotype && editPhenotype.trim() !== "") {
        fields.phenotype = editPhenotype.trim();
      }
      const priorYear = sig.year == null ? "" : String(sig.year);
      if (editYear !== priorYear && editYear.trim() !== "") fields.year = Number(editYear);
      if (editPublic !== (Number(sig.visibility) === 1)) fields.visibility = editPublic;

      if (Object.keys(fields).length === 0) {
        setShowEdit(false);
        return;
      }
      await updateSignature(hashkey, fields);
      setShowEdit(false);
      // Re-read rather than patching local state, so what is displayed is what
      // the database actually stored -- phenotype in particular round-trips
      // through a vocabulary table and may come back normalised.
      const refreshed = await getSignatureContext(hashkey);
      setContext(refreshed);
    } catch (err) {
      setEditError(err instanceof Error ? err.message : "Could not save changes.");
    } finally {
      setSaving(false);
    }
  }

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

  // Both tables on this page were plain <table> markup, which meant no column
  // sorting -- the one thing you want on a feature list you are scanning for
  // the strongest scores. DataTable brings sorting, search and paging; the
  // rows just need a stable key, which neither dataset carries.
  const featureRows = useMemo(
    () =>
      (context?.features ?? []).map((f, i) => {
        // A signature can legitimately have no per-feature score: an unordered
        // gene list (a Rummagene pull, any uni-directional signature) carries
        // members without statistics, and the API sends those as JSON null.
        // Guard null BEFORE coercing -- Number(null) is 0, not NaN, so a bare
        // Number(f.score) turns "no score" into a real score of 0.00 and, via
        // the sign test below, asserts a direction of "Up" for every feature.
        // Number(undefined) does give NaN, which is why this only ever showed
        // up once signatures with null scores existed.
        const rawScore = f.score;
        const score =
          rawScore == null
            ? NaN
            : typeof rawScore === "number"
              ? rawScore
              : Number(rawScore);
        return {
          rowId: `${f.probe_id ?? f.feature_id ?? i}::${i}`,
          // Prefer the gene symbol, then the stored identifier, and only fall
          // back to probe_id last. A signature that arrived without its own
          // probe ids carries OmicSignature's positional filler ("feature_1",
          // "feature_10"), which tells a reader nothing -- every Rummagene
          // pull looks like that. The API joins the assay's reference table to
          // supply gene_symbol/feature_name (attach_feature_labels).
          feature:
            (typeof f.gene_symbol === "string" && f.gene_symbol) ||
            (typeof f.feature_name === "string" && f.feature_name) ||
            f.probe_id ||
            String(f.feature_id ?? i),
          // Keep the numeric score on the row so sorting is numeric rather
          // than lexicographic on a formatted string.
          score: Number.isFinite(score) ? score : null,
          // Its own field rather than a second column keyed on `score`:
          // DataTable keys both its sort state and its React list on col.key,
          // so two columns sharing one key collide. Direction is sign(score),
          // not group_label -- group_label holds the contrast name
          // ("Higher in exceptional longevity").
          direction: !Number.isFinite(score) ? "—" : score >= 0 ? "Up" : "Down",
        };
      }),
    [context]
  );

  const featureColumns: Column<(typeof featureRows)[number]>[] = useMemo(
    () => [
      { key: "feature", label: "Feature", render: (r) => <span className="cell-strong">{r.feature}</span> },
      {
        key: "score",
        label: "Score",
        align: "right",
        render: (r) => <span className="cell-mono">{r.score == null ? "—" : r.score.toFixed(2)}</span>,
      },
      {
        key: "direction",
        label: "Direction",
        align: "right",
        filterable: true,
        render: (r) =>
          r.direction === "—" ? "—" : <Badge tone={r.direction === "Up" ? "success" : "danger"}>{r.direction}</Badge>,
      },
    ],
    []
  );

  // difexp columns are whatever the stored table happens to have, so the row
  // type is open. Column<T> keys on `keyof T`, which an index signature
  // satisfies; the explicit type keeps rowId available for rowKey.
  type DifexpRow = Record<string, unknown> & { rowId: string };

  const difexpRows: DifexpRow[] = useMemo(
    () => (difexp?.rows ?? []).map((row, i) => ({ ...row, rowId: `difexp-${i}` })),
    [difexp]
  );

  const difexpTableColumns: Column<DifexpRow>[] = useMemo(
    () =>
      difexpColumns.map((col) => ({
        key: col,
        label: formatLabel(col),
        render: (r: DifexpRow) => <span className="cell-mono">{formatValue(r[col])}</span>,
      })),
    [difexp]
  );

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
      ].filter((c) => hasValue(c.value) && !isUnknown(c.value))
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
            {context && canEditSignature(ownerUserName) && (
              <button className="btn btn-secondary" onClick={openEdit}>
                <Pencil size={15} /> Edit
              </button>
            )}
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

      <Modal
        open={showEdit}
        onClose={closeEdit}
        title="Edit signature"
        subtitle="Metadata only — the feature set and difexp are not changed."
        footer={
          <>
            <button className="btn btn-secondary" onClick={closeEdit}>
              Cancel
            </button>
            <button className="btn btn-primary" onClick={handleSaveEdit} disabled={saving}>
              {saving ? "Saving…" : "Save changes"}
            </button>
          </>
        }
      >
        <div className="field">
          <span className="field-label">Description</span>
          <input className="input" value={editDescription} onChange={(e) => setEditDescription(e.target.value)} />
        </div>
        <div className="field">
          <span className="field-label">Phenotype</span>
          <input
            className="input"
            value={editPhenotype}
            onChange={(e) => setEditPhenotype(e.target.value)}
            placeholder="e.g. arecoline vs. PBS"
          />
        </div>
        <div className="field">
          <span className="field-label">Keywords</span>
          <input
            className="input"
            value={editKeywords}
            onChange={(e) => setEditKeywords(e.target.value)}
            placeholder="comma separated"
          />
        </div>
        <div className="field">
          <span className="field-label">Year</span>
          <input className="input" value={editYear} onChange={(e) => setEditYear(e.target.value)} inputMode="numeric" />
        </div>
        <label className="dt-filter-option" style={{ padding: 0 }}>
          <input type="checkbox" checked={editPublic} onChange={(e) => setEditPublic(e.target.checked)} />
          <span>Public — visible to everyone with an account</span>
        </label>
        {editError && <p className="login-error">{editError}</p>}
      </Modal>


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
                    {/* The split bar reports how the features divide into up-
                        and down-regulated. A signature with no per-feature
                        scores has no such division -- an unordered gene list,
                        which is what every Rummagene pull is -- so it rendered
                        as "0 up / 0 down" and an empty bar, implying a
                        measurement that was never made. Show it only when
                        there is a split to show. */}
                    {(upCount ?? 0) + (downCount ?? 0) > 0 ? (
                      <SplitBar up={upCount} down={downCount} />
                    ) : (
                      <span className="cell-sub">
                        Unordered gene list — no per-feature scores, so no up/down split.
                      </span>
                    )}
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
                  <DataTable
                    columns={featureColumns}
                    rows={featureRows}
                    rowKey="rowId"
                    pageSize={25}
                    searchable
                    scrollable
                    maxHeight={560}
                  />
                )}
                {context.features.length === 0 && (
                  <p className="cell-sub" style={{ padding: "10px 14px" }}>No features recorded for this signature.</p>
                )}
              </div>
              </div>

              {hashkey && <RelatedSignaturesPanel signatureHashkey={hashkey} />}
              {hashkey && <RummagenePanel signatureHashkey={hashkey} />}
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
                  <DataTable
                    columns={difexpTableColumns}
                    rows={difexpRows}
                    rowKey="rowId"
                    pageSize={25}
                    searchable
                    scrollable
                    maxHeight={560}
                  />
                </div>
              )}
            </Card>
          )}
        </>
      )}
    </div>
  );
}
