import { useEffect, useMemo, useState } from "react";
import { Plus, Upload, Search, Download, Trash2, ShoppingBasket, Eye, X } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { SkeletonRows } from "../components/Skeleton";
import {
  searchSignaturesPage,
  type SignatureSortKey,
  deleteSignature,
  downloadSignatureExport,
  downloadSignatureBasket,
  uploadSignature,
  type SignatureSummary,
} from "../api/client";

const PAGE_SIZE = 25;
import { useBasket, addToBasket, removeFromBasket, clearBasket, isInBasket } from "../basket";
import { canDeleteSignature, canUploadSignature } from "../permissions";

function formatValue(value: unknown): string {
  if (value === null || value === undefined || value === "") return "—";
  return String(value);
}

export default function SignaturesPage() {
  const [rows, setRows] = useState<SignatureSummary[]>([]);
  const [total, setTotal] = useState(0);
  const [page, setPage] = useState(0); // 0-based
  const [loading, setLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);
  const [query, setQuery] = useState("");
  const [refreshTick, setRefreshTick] = useState(0);
  // Sort lives here, not in the table: it is part of the query the server
  // answers, so it has to survive alongside keyword and page.
  const [sortBy, setSortBy] = useState<SignatureSortKey>("signature_name");
  const [sortDir, setSortDir] = useState<"asc" | "desc">("asc");

  // Server-side pagination (DT `server = TRUE`): fetch only the current page
  // from the API instead of pulling every signature up front. `query` and
  // `page` both drive the fetch; a new search resets back to page 0.
  useEffect(() => {
    let cancelled = false;
    setLoading(true);
    searchSignaturesPage({ keyword: query || undefined, limit: PAGE_SIZE, offset: page * PAGE_SIZE, sortBy, sortDir })
      .then(({ rows: results, total: totalCount }) => {
        if (!cancelled) {
          setRows(results);
          setTotal(totalCount);
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
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [query, page, refreshTick, sortBy, sortDir]);

  // Mirrors the Shiny app's pattern: clicking a row just selects it and
  // reveals an action bar (View/Export/Add to Basket/Delete) above the
  // table -- it doesn't navigate anywhere by itself. Only "View" opens the
  // dedicated detail tab.
  const [active, setActive] = useState<SignatureSummary | null>(null);

  const [deleting, setDeleting] = useState(false);
  const [deleteError, setDeleteError] = useState<string | null>(null);
  const [exporting, setExporting] = useState(false);
  const [exportError, setExportError] = useState<string | null>(null);

  async function handleDelete() {
    if (!active) return;
    if (!window.confirm(`Delete "${active.signature_name}"? This cannot be undone.`)) return;
    setDeleting(true);
    setDeleteError(null);
    try {
      await deleteSignature(active.signature_hashkey);
      setActive(null);
      setRefreshTick((t) => t + 1);
    } catch (err) {
      setDeleteError(err instanceof Error ? err.message : "Could not delete signature.");
    } finally {
      setDeleting(false);
    }
  }

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

  // ---------- Upload (re-add a signature from an /signatures/export .rds file) ----------

  const [showUpload, setShowUpload] = useState(false);
  const [uploadFile, setUploadFile] = useState<File | null>(null);
  const [uploadVisible, setUploadVisible] = useState(false);
  const [uploading, setUploading] = useState(false);
  const [uploadError, setUploadError] = useState<string | null>(null);

  async function handleUpload() {
    if (!uploadFile) return;
    setUploading(true);
    setUploadError(null);
    try {
      await uploadSignature(uploadFile, uploadVisible);
      setShowUpload(false);
      setUploadFile(null);
      setUploadVisible(false);
      setRefreshTick((t) => t + 1);
    } catch (err) {
      setUploadError(err instanceof Error ? err.message : "Could not upload signature.");
    } finally {
      setUploading(false);
    }
  }

  // A curated, at-a-glance subset of the signatures table. The full metadata
  // (cutoffs, difexp counts, description, PMID, hashkey, etc.) lives on the
  // signature detail view -- this table is for scanning/finding, not for
  // showing every column.
  const columns: Column<SignatureSummary>[] = useMemo(
    () => [
      { key: "signature_name", label: "Signature", render: (r) => <span className="cell-strong">{r.signature_name}</span> },
      { key: "organism", label: "Organism", filterable: true, render: (r) => <span className="cell-italic">{r.organism ?? "—"}</span> },
      { key: "assay_type", label: "Assay", filterable: true, render: (r) => <Badge tone="neutral">{r.assay_type}</Badge> },
      { key: "direction_type", label: "Direction Type", filterable: true },
      { key: "phenotype", label: "Phenotype", filterable: true, render: (r) => r.phenotype ?? "—" },
      { key: "sample_type", label: "Sample Type", filterable: true, render: (r) => r.sample_type ?? "—" },
      { key: "platform_name", label: "Platform", filterable: true, render: (r) => r.platform_name ?? "—" },
      { key: "year", label: "Year", render: (r) => formatValue(r.year) },
      { key: "user_name", label: "Owner" },
      {
        key: "visibility",
        label: "Visibility",
        filterable: true,
        render: (r) => <Badge tone={r.visibility === 1 ? "success" : "neutral"}>{r.visibility === 1 ? "Public" : "Private"}</Badge>,
      },
    ],
    []
  );

  return (
    <div className="page">
      <PageHeader
        variant="bar"
        title="Signatures"
        subtitle={loading ? "Loading signatures…" : `${total} signatures across the repository`}
        actions={
          <>
            <button className="btn btn-secondary" onClick={() => setBasketOpen(true)}>
              <ShoppingBasket size={16} /> Basket ({basket.length})
            </button>
            <button className="btn btn-secondary">
              <Plus size={16} /> Create
            </button>
            {canUploadSignature() && (
              <button className="btn btn-primary" onClick={() => setShowUpload((s) => !s)}>
                <Upload size={16} /> Upload
              </button>
            )}
          </>
        }
      />

      {showUpload && (
        <Card title="Upload signature">
          <p className="cell-sub" style={{ marginBottom: 12 }}>
            Upload an .rds file produced by a signature's "Export" download to re-add it under your account.
          </p>
          <div className="field">
            <span className="field-label">Signature file (.rds)</span>
            <input
              className="input"
              type="file"
              accept=".rds"
              onChange={(e) => setUploadFile(e.target.files?.[0] ?? null)}
            />
          </div>
          <label className="dt-filter-option" style={{ marginTop: 12, padding: 0 }}>
            <input type="checkbox" checked={uploadVisible} onChange={(e) => setUploadVisible(e.target.checked)} />
            <span>Public</span>
          </label>
          {uploadError && <p className="login-error">{uploadError}</p>}
          <div style={{ display: "flex", gap: 8, marginTop: 16 }}>
            <button className="btn btn-primary" disabled={!uploadFile || uploading} onClick={handleUpload}>
              {uploading ? "Uploading…" : "Upload"}
            </button>
            <button
              className="btn btn-secondary"
              onClick={() => {
                setShowUpload(false);
                setUploadFile(null);
                setUploadError(null);
              }}
            >
              Cancel
            </button>
          </div>
        </Card>
      )}

      <Card padded={false}>
        <div className="toolbar">
          <div className="input-affix toolbar-search">
            <Search size={15} className="toolbar-search-icon" />
            <input
              className="input input-flush"
              placeholder="Search signatures…"
              value={query}
              onChange={(e) => {
                setQuery(e.target.value);
                setPage(0);
              }}
            />
          </div>
        </div>

        {active && (
          <div className="signature-action-bar">
            <div className="signature-action-name">
              <span className="signature-action-label">Selected</span>
              <span className="cell-strong">{active.signature_name}</span>
            </div>
            <div className="signature-action-buttons">
              <a
                className="btn btn-primary"
                href={`/signatures/${active.signature_hashkey}`}
                target="_blank"
                rel="noopener"
              >
                <Eye size={15} /> View
              </a>
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
              {canDeleteSignature(active.user_name) && (
                <button className="btn btn-secondary" onClick={handleDelete} disabled={deleting}>
                  <Trash2 size={15} /> {deleting ? "Deleting…" : "Delete"}
                </button>
              )}
              <button className="icon-btn" onClick={() => setActive(null)} title="Clear selection">
                <X size={16} />
              </button>
            </div>
          </div>
        )}
        {deleteError && <p className="login-error" style={{ margin: "0 16px 12px" }}>{deleteError}</p>}
        {exportError && <p className="login-error" style={{ margin: "0 16px 12px" }}>{exportError}</p>}

        {loadError && <p className="login-error" style={{ margin: "0 16px 12px" }}>{loadError}</p>}
        {loading && rows.length === 0 ? (
          <SkeletonRows rows={10} cols={6} />
        ) : (
          <DataTable
            columns={columns}
            rows={rows}
            rowKey="signature_hashkey"
            selectedKey={active?.signature_hashkey ?? null}
            onSelectRow={setActive}
            emptyLabel="No signatures match your filters"
            scrollable
            maxHeight={560}
            searchable={false}
            serverPagination={{ page, pageSize: PAGE_SIZE, total, onPageChange: setPage }}
            serverSort={{
              sortBy,
              sortDir,
              onSortChange: (key, dir) => {
                setSortBy(key as SignatureSortKey);
                setSortDir(dir);
                // Re-sorting reorders the whole result set, so the old page
                // number no longer points at anything meaningful.
                setPage(0);
              },
            }}
          />
        )}
      </Card>

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
