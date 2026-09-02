import { useEffect, useState } from "react";
import { FolderPlus, Trash2, X, Plus } from "lucide-react";
import PageHeader from "../components/PageHeader";
import Card from "../components/Card";
import Modal from "../components/Modal";
import Badge from "../components/Badge";
import Drawer from "../components/Drawer";
import DataTable, { type Column } from "../components/DataTable";
import { SkeletonRows } from "../components/Skeleton";
import {
  searchCollections,
  getCollectionDetail,
  createCollection,
  deleteCollection,
  addSignatureToCollection,
  removeSignatureFromCollection,
  searchSignatures,
  getAuth,
  type CollectionSummary,
  type CollectionDetail,
  type SignatureSummary,
} from "../api/client";

// Client-side approximation of the API's own rule: only editor/admin can
// create/delete/modify collections, and non-admins need to own the
// collection or hold an owner/editor grant (which the client doesn't know
// about) -- the API is the real authority either way.
function canManage(role: string | undefined, owner: string, userName: string | undefined): boolean {
  if (!role || !userName) return false;
  if (role === "admin") return true;
  return role === "editor" && userName === owner;
}

export default function CollectionsPage() {
  const auth = getAuth();
  const [collections, setCollections] = useState<CollectionSummary[]>([]);
  const [loading, setLoading] = useState(true);
  const [loadError, setLoadError] = useState<string | null>(null);

  function refreshList() {
    setLoading(true);
    searchCollections()
      .then((results) => {
        setCollections(results);
        setLoadError(null);
      })
      .catch((err) => setLoadError(err instanceof Error ? err.message : "Could not load collections."))
      .finally(() => setLoading(false));
  }

  useEffect(refreshList, []);

  const [active, setActive] = useState<CollectionSummary | null>(null);
  const [detail, setDetail] = useState<CollectionDetail | null>(null);
  const [detailLoading, setDetailLoading] = useState(false);
  const [detailError, setDetailError] = useState<string | null>(null);

  function refreshDetail(hashkey: string) {
    setDetailLoading(true);
    getCollectionDetail(hashkey)
      .then((d) => {
        setDetail(d);
        setDetailError(null);
      })
      .catch((err) => setDetailError(err instanceof Error ? err.message : "Could not load collection."))
      .finally(() => setDetailLoading(false));
  }

  useEffect(() => {
    if (!active) {
      setDetail(null);
      return;
    }
    refreshDetail(active.collection_hashkey);
    // eslint-disable-next-line react-hooks/exhaustive-deps
  }, [active?.collection_hashkey]);

  // ---------- New collection form ----------

  const [showCreate, setShowCreate] = useState(false);
  const [newName, setNewName] = useState("");
  const [newDescription, setNewDescription] = useState("");
  const [newVisible, setNewVisible] = useState(false);
  const [creating, setCreating] = useState(false);
  const [createError, setCreateError] = useState<string | null>(null);

  // Single dismissal path, so Cancel, Escape, the scrim and the X all clear
  // the form identically. The old Cancel button only flipped showCreate, so a
  // half-typed name and any error survived into the next opening.
  function closeCreate() {
    setShowCreate(false);
    setNewName("");
    setNewDescription("");
    setNewVisible(false);
    setCreateError(null);
  }

  async function handleCreate() {
    setCreating(true);
    setCreateError(null);
    try {
      await createCollection(newName.trim(), newDescription.trim(), newVisible);
      setShowCreate(false);
      setNewName("");
      setNewDescription("");
      setNewVisible(false);
      refreshList();
    } catch (err) {
      setCreateError(err instanceof Error ? err.message : "Could not create collection.");
    } finally {
      setCreating(false);
    }
  }

  // ---------- Delete collection ----------

  const [deleting, setDeleting] = useState(false);
  const [deleteError, setDeleteError] = useState<string | null>(null);

  async function handleDelete() {
    if (!active) return;
    if (!window.confirm(`Delete "${active.collection_name}"? This cannot be undone.`)) return;
    setDeleting(true);
    setDeleteError(null);
    try {
      await deleteCollection(active.collection_hashkey);
      setActive(null);
      refreshList();
    } catch (err) {
      setDeleteError(err instanceof Error ? err.message : "Could not delete collection.");
    } finally {
      setDeleting(false);
    }
  }

  // ---------- Membership ----------

  const [allSignatures, setAllSignatures] = useState<SignatureSummary[]>([]);
  const [addHashkey, setAddHashkey] = useState("");
  const [memberBusy, setMemberBusy] = useState(false);
  const [memberError, setMemberError] = useState<string | null>(null);

  useEffect(() => {
    searchSignatures({ limit: 100 })
      .then(setAllSignatures)
      .catch(() => {
        /* the add-signature picker just stays empty */
      });
  }, []);

  const memberHashkeys = new Set((detail?.signatures ?? []).map((s) => s.signature_hashkey));
  const addableSignatures = allSignatures.filter((s) => !memberHashkeys.has(s.signature_hashkey));

  async function handleAddSignature() {
    if (!active || !addHashkey) return;
    setMemberBusy(true);
    setMemberError(null);
    try {
      await addSignatureToCollection(active.collection_hashkey, addHashkey);
      setAddHashkey("");
      refreshDetail(active.collection_hashkey);
      refreshList();
    } catch (err) {
      setMemberError(err instanceof Error ? err.message : "Could not add signature.");
    } finally {
      setMemberBusy(false);
    }
  }

  async function handleRemoveSignature(signatureHashkey: string) {
    if (!active) return;
    setMemberBusy(true);
    setMemberError(null);
    try {
      await removeSignatureFromCollection(active.collection_hashkey, signatureHashkey);
      refreshDetail(active.collection_hashkey);
      refreshList();
    } catch (err) {
      setMemberError(err instanceof Error ? err.message : "Could not remove signature.");
    } finally {
      setMemberBusy(false);
    }
  }

  const columns: Column<CollectionSummary>[] = [
    {
      key: "collection_name",
      label: "Collection",
      render: (r) => (
        <div>
          <span className="cell-strong">{r.collection_name}</span>
          {r.description && <span className="cell-sub">{r.description}</span>}
        </div>
      ),
    },
    { key: "num_signatures", label: "Signatures", align: "right", render: (r) => <span className="cell-mono">{r.num_signatures}</span> },
    { key: "user_name", label: "Owner" },
    {
      key: "visibility",
      label: "Visibility",
      filterable: true,
      render: (r) => <Badge tone={r.visibility === 1 ? "success" : "neutral"}>{r.visibility === 1 ? "Public" : "Private"}</Badge>,
    },
  ];

  return (
    <div className="page">
      <PageHeader
        title="Collections"
        subtitle={loading ? "Loading collections…" : `${collections.length} collections`}
        actions={
          <button className="btn btn-primary" onClick={() => setShowCreate((s) => !s)}>
            <FolderPlus size={16} /> New collection
          </button>
        }
      />

      <Modal
        open={showCreate}
        onClose={closeCreate}
        title="New collection"
        footer={
          <>
            <button className="btn btn-secondary" onClick={closeCreate}>
              Cancel
            </button>
            <button className="btn btn-primary" disabled={!newName.trim() || creating} onClick={handleCreate}>
              {creating ? "Creating…" : "Create"}
            </button>
          </>
        }
      >
        <div className="field">
          <span className="field-label">Name</span>
          <input className="input" value={newName} onChange={(e) => setNewName(e.target.value)} placeholder="Collection name" />
        </div>
        <div className="field">
          <span className="field-label">Description</span>
          <input className="input" value={newDescription} onChange={(e) => setNewDescription(e.target.value)} placeholder="Optional description" />
        </div>
        <label className="dt-filter-option" style={{ padding: 0 }}>
          <input type="checkbox" checked={newVisible} onChange={(e) => setNewVisible(e.target.checked)} />
          <span>Public</span>
        </label>
        {createError && <p className="login-error">{createError}</p>}
      </Modal>

      {loadError && <p className="login-error">{loadError}</p>}

      <Card padded={false}>
        {loading && collections.length === 0 ? (
          <SkeletonRows rows={6} cols={4} />
        ) : (
          <DataTable
            columns={columns}
            rows={collections}
            rowKey="collection_hashkey"
            selectedKey={active?.collection_hashkey ?? null}
            onSelectRow={setActive}
            emptyLabel="No collections yet"
            scrollable
            maxHeight={460}
          />
        )}
      </Card>

      <Drawer
        open={active !== null}
        onClose={() => setActive(null)}
        title={active?.collection_name ?? ""}
        subtitle={active ? `${active.num_signatures} signatures` : ""}
        footer={
          active &&
          canManage(auth?.user_role, active.user_name, auth?.user_name) && (
            <button className="btn btn-secondary btn-block" onClick={handleDelete} disabled={deleting}>
              <Trash2 size={15} /> {deleting ? "Deleting…" : "Delete collection"}
            </button>
          )
        }
      >
        {active && (
          <>
            {deleteError && <p className="login-error">{deleteError}</p>}
            <dl className="detail-list">
              <div><dt>Owner</dt><dd>{active.user_name}</dd></div>
              <div><dt>Visibility</dt><dd><Badge tone={active.visibility === 1 ? "success" : "neutral"}>{active.visibility === 1 ? "Public" : "Private"}</Badge></dd></div>
              <div><dt>Created</dt><dd>{active.date_created}</dd></div>
            </dl>
            {active.description && <p className="detail-desc">{active.description}</p>}

            <h4 className="detail-section-title">Member signatures</h4>
            {detailLoading && <p className="cell-sub">Loading…</p>}
            {detailError && <p className="login-error">{detailError}</p>}
            {memberError && <p className="login-error">{memberError}</p>}

            {!detailLoading && detail && (
              <div className="member-list">
                {detail.signatures.length === 0 && <p className="cell-sub">No signatures in this collection yet.</p>}
                {detail.signatures.map((s) => (
                  <div className="member-item" key={s.signature_hashkey}>
                    <span className="cell-strong">{s.signature_name}</span>
                    <div style={{ display: "flex", alignItems: "center", gap: 8 }}>
                      <Badge tone="neutral">{s.assay_type}</Badge>
                      {canManage(auth?.user_role, active.user_name, auth?.user_name) && (
                        <button
                          className="icon-btn"
                          onClick={() => handleRemoveSignature(s.signature_hashkey)}
                          disabled={memberBusy}
                          title="Remove from collection"
                        >
                          <X size={14} />
                        </button>
                      )}
                    </div>
                  </div>
                ))}
              </div>
            )}

            {canManage(auth?.user_role, active.user_name, auth?.user_name) && (
              <div style={{ display: "flex", gap: 8, marginTop: 14 }}>
                <select className="input" value={addHashkey} onChange={(e) => setAddHashkey(e.target.value)}>
                  <option value="">Add a signature…</option>
                  {addableSignatures.map((s) => (
                    <option key={s.signature_hashkey} value={s.signature_hashkey}>{s.signature_name}</option>
                  ))}
                </select>
                <button className="btn btn-secondary" disabled={!addHashkey || memberBusy} onClick={handleAddSignature}>
                  <Plus size={15} />
                </button>
              </div>
            )}
          </>
        )}
      </Drawer>
    </div>
  );
}
