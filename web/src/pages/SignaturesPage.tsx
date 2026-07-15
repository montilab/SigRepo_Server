import { useEffect, useMemo, useState, type ReactNode } from "react";
import { Plus, Upload, Search, Download, Trash2, ShoppingBasket, X } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import {
  searchSignatures,
  getSignatureContext,
  deleteSignature,
  getDifexp,
  downloadSignatureExport,
  downloadSignatureBasket,
  getAuth,
  type SignatureSummary,
  type SignatureContext,
  type DifexpResult,
} from "../api/client";
import { useBasket, addToBasket, removeFromBasket, clearBasket, isInBasket } from "../basket";

// Client-side approximation of the API's delete_signature() rule (editor/
// admin role, owner unless admin) so the button doesn't appear for requests
// that will just 403. Doesn't know about per-signature signature_access
// grants (an editor explicitly granted access to someone else's signature),
// so it can hide the button in a few cases the API would actually allow --
// the API is the real authority either way.
function canDelete(signature: SignatureSummary): boolean {
  const auth = getAuth();
  if (!auth) return false;
  if (auth.user_role === "admin") return true;
  return auth.user_role === "editor" && auth.user_name === signature.user_name;
}

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

interface MetadataField {
  key: string;
  label: string;
  render?: (value: unknown) => ReactNode;
}

// Groups the raw signature row (every column of `signatures`, plus the
// joined organism/phenotype/sample_type/platform_name) into labeled
// sections instead of one flat alphabetical dump. Excludes:
//  - signature_id/organism_id/phenotype_id/platform_id/sample_type_id --
//    internal foreign keys; the joined human-readable columns replace them.
//  - signature_name/assay_type -- already shown in the drawer title/subtitle.
//  - signature_hashkey -- an internal lookup key, not metadata; shown as a
//    small reference line under the title instead.
// Fields with no value are dropped individually, and a whole section is
// skipped if none of its fields have a value.
const METADATA_SECTIONS: { title: string; fields: MetadataField[] }[] = [
  {
    title: "Classification",
    fields: [
      { key: "organism", label: "Organism" },
      { key: "phenotype", label: "Phenotype" },
      { key: "sample_type", label: "Sample Type" },
      { key: "platform_name", label: "Platform" },
      { key: "direction_type", label: "Direction Type" },
    ],
  },
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
      { key: "num_of_difexp", label: "# Difexp" },
      { key: "num_up_regulated", label: "# Up" },
      { key: "num_down_regulated", label: "# Down" },
    ],
  },
  {
    title: "Details",
    fields: [
      { key: "description", label: "Description" },
      { key: "covariates", label: "Covariates" },
      { key: "keywords", label: "Keywords" },
      { key: "PMID", label: "PMID" },
      { key: "year", label: "Year" },
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

export default function SignaturesPage() {
  const [rows, setRows] = useState<SignatureSummary[]>([]);
  const [loading, setLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [query, setQuery] = useState("");

  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    searchSignatures({ keyword: query || undefined, limit: 100 })
      .then((results) => {
        if (!cancelled) {
          setRows(results);
          setLoadError(null);
        }
      })
      .catch((err) => {
        if (!cancelled) setLoadError(err instanceof Error ? err.message : "Could not load signatures.");
      })
      .finally(() => {
        if (!cancelled) setLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [query]);

  const [active, setActive] = useState<SignatureSummary | null>(null);
  const [tab, setTab] = useState<"signature" | "difexp">("signature");
  const [context, setContext] = useState<SignatureContext | null>(null);
  const [contextLoading, setContextLoading] = useState(false);
  const [deleting, setDeleting] = useState(false);
  const [deleteError, setDeleteError] = useState<string | null>(null);

  const [difexp, setDifexp] = useState<DifexpResult | null>(null);
  const [difexpLoading, setDifexpLoading] = useState(false);
  const [difexpError, setDifexpError] = useState<string | null>(null);

  function selectSignature(row: SignatureSummary) {
    setActive(row);
    setTab("signature");
    setDifexp(null);
    setDifexpError(null);
  }

  async function handleDelete() {
    if (!active) return;
    if (!window.confirm(`Delete "${active.signature_name}"? This cannot be undone.`)) return;
    setDeleting(true);
    setDeleteError(null);
    try {
      await deleteSignature(active.signature_hashkey);
      setRows((prev) => prev.filter((r) => r.signature_hashkey !== active.signature_hashkey));
      setActive(null);
    } catch (err) {
      setDeleteError(err instanceof Error ? err.message : "Could not delete signature.");
    } finally {
      setDeleting(false);
    }
  }

  async function handleLoadDifexp() {
    if (!active) return;
    setDifexpLoading(true);
    setDifexpError(null);
    try {
      setDifexp(await getDifexp(active.signature_hashkey));
    } catch (err) {
      setDifexpError(err instanceof Error ? err.message : "Could not load difexp.");
    } finally {
      setDifexpLoading(false);
    }
  }

  const [exporting, setExporting] = useState(false);
  const [exportError, setExportError] = useState<string | null>(null);

  async function handleExport() {
    if (!active) return;
    setExporting(true);
    setExportError(null);
    try {
      await downloadSignatureExport(active.signature_hashkey);
    } catch (err) {
      setExportError(err instanceof Error ? err.message : "Could not export signature.");
    } finally {
      setExporting(false);
    }
  }

  // ---------- Basket (bulk download), ported from the Shiny app's basket ----------

  const basket = useBasket();
  const [basketOpen, setBasketOpen] = useState(false);
  const [basketDownloading, setBasketDownloading] = useState(false);
  const [basketError, setBasketError] = useState<string | null>(null);

  async function handleDownloadBasket() {
    setBasketDownloading(true);
    setBasketError(null);
    try {
      await downloadSignatureBasket(basket.map((b) => b.signature_hashkey));
    } catch (err) {
      setBasketError(err instanceof Error ? err.message : "Could not download basket.");
    } finally {
      setBasketDownloading(false);
    }
  }

  useEffect(() => {
    if (!active) {
      setContext(null);
      return;
    }
    let cancelled = false;
    setContextLoading(true);
    getSignatureContext(active.signature_hashkey)
      .then((ctx) => {
        if (!cancelled) setContext(ctx);
      })
      .catch(() => {
        if (!cancelled) setContext(null);
      })
      .finally(() => {
        if (!cancelled) setContextLoading(false);
      });
    return () => {
      cancelled = true;
    };
  }, [active]);

  // Every signatures-table column (mysql/schema/signatures.sql), via the
  // human-readable joined names instead of raw foreign keys.
  const columns: Column<SignatureSummary>[] = useMemo(
    () => [
      { key: "signature_name", label: "Signature", render: (r) => <span className="cell-strong">{r.signature_name}</span> },
      { key: "organism", label: "Organism", filterable: true, render: (r) => <span className="cell-italic">{r.organism ?? "—"}</span> },
      { key: "assay_type", label: "Assay", filterable: true, render: (r) => <Badge tone="neutral">{r.assay_type}</Badge> },
      { key: "direction_type", label: "Direction Type", filterable: true },
      { key: "phenotype", label: "Phenotype", filterable: true, render: (r) => r.phenotype ?? "—" },
      { key: "sample_type", label: "Sample Type", filterable: true, render: (r) => r.sample_type ?? "—" },
      { key: "platform_name", label: "Platform", filterable: true, render: (r) => r.platform_name ?? "—" },
      { key: "feature_count", label: "Features", align: "right" },
      {
        key: "has_difexp",
        label: "Has Difexp",
        render: (r) => <Badge tone={r.has_difexp === 1 ? "success" : "neutral"}>{r.has_difexp === 1 ? "Yes" : "No"}</Badge>,
      },
      { key: "num_of_difexp", label: "# Difexp", align: "right", render: (r) => formatValue(r.num_of_difexp) },
      { key: "num_up_regulated", label: "# Up", align: "right", render: (r) => formatValue(r.num_up_regulated) },
      { key: "num_down_regulated", label: "# Down", align: "right", render: (r) => formatValue(r.num_down_regulated) },
      { key: "score_cutoff", label: "Score Cutoff", align: "right", render: (r) => formatValue(r.score_cutoff) },
      { key: "logfc_cutoff", label: "LogFC Cutoff", align: "right", render: (r) => formatValue(r.logfc_cutoff) },
      { key: "p_value_cutoff", label: "P-value Cutoff", align: "right", render: (r) => formatValue(r.p_value_cutoff) },
      { key: "adj_p_cutoff", label: "Adj. P Cutoff", align: "right", render: (r) => formatValue(r.adj_p_cutoff) },
      { key: "cutoff_description", label: "Cutoff Description", render: (r) => formatValue(r.cutoff_description) },
      { key: "covariates", label: "Covariates", render: (r) => formatValue(r.covariates) },
      { key: "keywords", label: "Keywords", render: (r) => formatValue(r.keywords) },
      { key: "PMID", label: "PMID", render: (r) => formatValue(r.PMID) },
      { key: "year", label: "Year", render: (r) => formatValue(r.year) },
      { key: "others", label: "Others", render: (r) => formatValue(r.others) },
      { key: "description", label: "Description", render: (r) => formatValue(r.description) },
      { key: "user_name", label: "Owner" },
      { key: "date_created", label: "Created" },
      {
        key: "visibility",
        label: "Visibility",
        render: (r) => <Badge tone={r.visibility === 1 ? "success" : "neutral"}>{r.visibility === 1 ? "Public" : "Private"}</Badge>,
      },
      { key: "signature_hashkey", label: "Hashkey", render: (r) => <span className="cell-mono">{r.signature_hashkey}</span> },
    ],
    []
  );

  const difexpColumns = useMemo(() => {
    if (!difexp || difexp.rows.length === 0) return [];
    return Object.keys(difexp.rows[0]);
  }, [difexp]);

  return (
    <div className="page">
      <PageHeader
        title="Signatures"
        subtitle={loading ? "Loading signatures…" : `${rows.length} signatures across the repository`}
        actions={
          <>
            <button className="btn btn-secondary" onClick={() => setBasketOpen(true)}>
              <ShoppingBasket size={16} /> Basket ({basket.length})
            </button>
            <button className="btn btn-secondary">
              <Plus size={16} /> Create
            </button>
            <button className="btn btn-primary">
              <Upload size={16} /> Upload
            </button>
          </>
        }
      />

      <Card padded={false}>
        <div className="toolbar">
          <div className="input-affix toolbar-search">
            <Search size={15} className="toolbar-search-icon" />
            <input
              className="input input-flush"
              placeholder="Search signatures…"
              value={query}
              onChange={(e) => setQuery(e.target.value)}
            />
          </div>
        </div>
        {loadError && <p className="login-error" style={{ margin: "0 16px 12px" }}>{loadError}</p>}
        <DataTable
          columns={columns}
          rows={rows}
          rowKey="signature_hashkey"
          selectedKey={active?.signature_hashkey ?? null}
          onSelectRow={selectSignature}
          emptyLabel={loading ? "Loading…" : "No signatures match your filters"}
        />
      </Card>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active?.signature_name ?? ""}
        subtitle={active ? `${active.organism ?? "—"} · ${active.assay_type}` : ""}
        size="wide"
        footer={
          active && (
            <>
              {canDelete(active) && (
                <button className="btn btn-secondary" onClick={handleDelete} disabled={deleting}>
                  <Trash2 size={15} /> {deleting ? "Deleting…" : "Delete"}
                </button>
              )}
              <button className="btn btn-secondary" onClick={handleExport} disabled={exporting}>
                <Download size={15} /> {exporting ? "Exporting…" : "Export"}
              </button>
              <button
                className="btn btn-secondary"
                onClick={() => addToBasket(active)}
                disabled={isInBasket(active.signature_hashkey)}
              >
                <ShoppingBasket size={15} /> {isInBasket(active.signature_hashkey) ? "In Basket" : "Add to Basket"}
              </button>
              <button className="btn btn-primary">Run enrichment</button>
            </>
          )
        }
      >
        {active && (
          <>
            {deleteError && <p className="login-error">{deleteError}</p>}
            {exportError && <p className="login-error">{exportError}</p>}

            <div className="segmented" style={{ marginBottom: 16 }}>
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
                {contextLoading && <p className="cell-sub">Loading metadata…</p>}
                {!contextLoading && context && (
                  <p className="cell-sub cell-mono" style={{ marginBottom: 18 }}>{context.signature.signature_hashkey as string}</p>
                )}
                {!contextLoading && context && METADATA_SECTIONS.map((section) => {
                  const fields = section.fields.filter((f) => hasValue(context.signature[f.key]));
                  if (fields.length === 0) return null;
                  return (
                    <div key={section.title} style={{ marginBottom: 22 }}>
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

                <h4 className="detail-section-title">Top features</h4>
                {contextLoading && <p className="cell-sub">Loading features…</p>}
                {!contextLoading && context && context.features.length > 0 && (
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
                )}
                {!contextLoading && context && context.features.length === 0 && (
                  <p className="cell-sub">No features recorded for this signature.</p>
                )}
              </>
            )}

            {tab === "difexp" && (
              <>
                {!difexp && (
                  <button className="btn btn-secondary" onClick={handleLoadDifexp} disabled={difexpLoading}>
                    {difexpLoading ? "Loading difexp…" : "Load difexp"}
                  </button>
                )}
                {difexpError && <p className="login-error">{difexpError}</p>}
                {difexp && difexp.message && <p className="cell-sub">{difexp.message}</p>}
                {difexp && difexp.rows.length > 0 && (
                  <div className="dt-scroll" style={{ marginTop: 12 }}>
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
                )}
              </>
            )}
          </>
        )}
      </Drawer>

      <Drawer
        open={basketOpen}
        onClose={() => setBasketOpen(false)}
        title="Basket"
        subtitle={`${basket.length} signature${basket.length === 1 ? "" : "s"}`}
        footer={
          basket.length > 0 && (
            <>
              <button className="btn btn-secondary" onClick={clearBasket}>
                Clear Basket
              </button>
              <button className="btn btn-primary" onClick={handleDownloadBasket} disabled={basketDownloading}>
                <Download size={15} /> {basketDownloading ? "Downloading…" : "Download Basket"}
              </button>
            </>
          )
        }
      >
        {basketError && <p className="login-error">{basketError}</p>}
        {basket.length === 0 ? (
          <p className="cell-sub">No signatures in the basket yet. Open a signature and click "Add to Basket".</p>
        ) : (
          <div className="member-list">
            {basket.map((item) => (
              <div className="member-item" key={item.signature_hashkey}>
                <div>
                  <span className="cell-strong">{item.signature_name}</span>
                  <span className="cell-sub">{item.organism ?? "—"} · {item.phenotype ?? "—"}</span>
                </div>
                <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                  <Badge tone="neutral">{item.assay_type}</Badge>
                  <button
                    className="icon-btn"
                    onClick={() => removeFromBasket(item.signature_hashkey)}
                    title="Remove from basket"
                  >
                    <X size={14} />
                  </button>
                </div>
              </div>
            ))}
          </div>
        )}
      </Drawer>
    </div>
  );
}
