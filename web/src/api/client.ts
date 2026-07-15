// Typed client for the SigRepo Plumber API.
//
// One quirk this module still absorbs: single-value fields are auto-unboxed
// to scalars (one distinct organism comes back as "Homo sapiens", not
// ["Homo sapiens"]), so toArray() normalizes list-shaped fields back to
// arrays. (The API previously also double-encoded every body; that's fixed
// server-side in json_response, so no unwrap is needed here.)
//
// In dev, requests go to "/api/*", which Vite proxies to the Plumber server
// (see vite.config.ts) — no CORS. Override with VITE_API_BASE for other setups.

const API_BASE = import.meta.env.VITE_API_BASE ?? "/api";

export class ApiError extends Error {
  status: number;
  constructor(message: string, status: number) {
    super(message);
    this.status = status;
  }
}

// Error bodies come back as [{ MESSAGES: "..." }] (a data.frame row).
function extractMessage(data: unknown): string | null {
  const asRecord = (v: unknown) =>
    v && typeof v === "object" ? (v as Record<string, unknown>) : null;
  if (Array.isArray(data)) {
    const first = asRecord(data[0]);
    if (first && typeof first.MESSAGES === "string") return first.MESSAGES;
  }
  const rec = asRecord(data);
  if (rec && typeof rec.MESSAGES === "string") return rec.MESSAGES;
  return null;
}

async function apiFetch<T>(path: string, init?: RequestInit): Promise<T> {
  const res = await fetch(`${API_BASE}${path}`, init);
  const data = await res.json().catch(() => null);
  if (!res.ok) {
    throw new ApiError(extractMessage(data) ?? `Request failed (${res.status})`, res.status);
  }
  return data as T;
}

function toArray(x: unknown): string[] {
  if (Array.isArray(x)) return x.map(String);
  if (x == null || x === "") return [];
  return [String(x)];
}

// ---------- Auth state ----------

export interface AuthUser {
  user_name: string;
  user_role: string;
  api_key: string;
}

const AUTH_KEY = "sr-auth";
let currentAuth: AuthUser | null = (() => {
  try {
    const raw = localStorage.getItem(AUTH_KEY);
    return raw ? (JSON.parse(raw) as AuthUser) : null;
  } catch {
    return null;
  }
})();

export function getAuth(): AuthUser | null {
  return currentAuth;
}

function setAuth(auth: AuthUser | null) {
  currentAuth = auth;
  if (auth) localStorage.setItem(AUTH_KEY, JSON.stringify(auth));
  else localStorage.removeItem(AUTH_KEY);
}

function requireApiKey(): string {
  if (!currentAuth) throw new ApiError("Not signed in.", 401);
  return currentAuth.api_key;
}

// ---------- Endpoints ----------

export async function login(userName: string, password: string): Promise<AuthUser> {
  const auth = await apiFetch<AuthUser>("/login", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ user_name: userName, password }),
  });
  setAuth(auth);
  return auth;
}

export function logout() {
  setAuth(null);
}

export interface Vocabulary {
  organism: string[];
  phenotype: string[];
  sample_type: string[];
  platform: string[];
  assay_type: string[];
}

export async function getVocabulary(): Promise<Vocabulary> {
  const raw = await apiFetch<Record<string, unknown>>(
    `/vocabulary?api_key=${encodeURIComponent(requireApiKey())}`
  );
  return {
    organism: toArray(raw.organism),
    phenotype: toArray(raw.phenotype),
    sample_type: toArray(raw.sample_type),
    platform: toArray(raw.platform),
    assay_type: toArray(raw.assay_type),
  };
}

// Mirrors the full `signatures` table (mysql/schema/signatures.sql), plus
// the joined organism/phenotype/sample_type/platform_name lookups and a
// computed feature_count. See search_signatures() in api/lib/signature.R.
export interface SignatureSummary {
  signature_id: number;
  signature_hashkey: string;
  signature_name: string;
  direction_type: string;
  assay_type: string;
  organism: string | null;
  phenotype: string | null;
  sample_type: string | null;
  platform_name: string | null;
  covariates: string | null;
  description: string | null;
  score_cutoff: number | null;
  logfc_cutoff: number | null;
  p_value_cutoff: number | null;
  adj_p_cutoff: number | null;
  cutoff_description: string | null;
  keywords: string | null;
  PMID: number | null;
  year: number | null;
  others: string | null;
  has_difexp: 0 | 1;
  num_of_difexp: number | null;
  num_up_regulated: number | null;
  num_down_regulated: number | null;
  user_name: string;
  date_created: string;
  visibility: 0 | 1;
  feature_count: number;
}

export interface SearchSignaturesParams {
  organism?: string;
  phenotype?: string;
  assay_type?: string;
  keyword?: string;
  limit?: number;
}

export async function searchSignatures(params: SearchSignaturesParams = {}): Promise<SignatureSummary[]> {
  const query = new URLSearchParams({ api_key: requireApiKey() });
  if (params.organism) query.set("organism", params.organism);
  if (params.phenotype) query.set("phenotype", params.phenotype);
  if (params.assay_type) query.set("assay_type", params.assay_type);
  if (params.keyword) query.set("keyword", params.keyword);
  if (params.limit) query.set("limit", String(params.limit));

  const raw = await apiFetch<{ count: number; signatures: SignatureSummary[] }>(
    `/signatures/search?${query.toString()}`
  );
  return raw.signatures ?? [];
}

export interface SignatureFeature {
  probe_id?: string;
  feature_id?: number;
  score?: number;
  group_label?: string;
  [key: string]: unknown;
}

export interface SignatureContext {
  signature: Record<string, unknown>;
  feature_count: number;
  features: SignatureFeature[];
}

export async function getSignatureContext(
  signatureHashkey: string,
  options: { includeFeatures?: boolean; maxFeatures?: number } = {}
): Promise<SignatureContext> {
  const raw = await apiFetch<{ context: SignatureContext }>("/read/signature_context", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      signature_hashkey: signatureHashkey,
      include_features: options.includeFeatures ?? true,
      max_features: options.maxFeatures ?? 50,
    }),
  });
  return raw.context;
}

export async function deleteSignature(signatureHashkey: string): Promise<void> {
  await apiFetch<{ MESSAGES: string }>(
    `/signatures/delete?api_key=${encodeURIComponent(requireApiKey())}&signature_hashkey=${encodeURIComponent(signatureHashkey)}`,
    { method: "DELETE" }
  );
}

export interface DifexpRow {
  [key: string]: unknown;
}

export interface DifexpResult {
  rows: DifexpRow[];
  message?: string;
}

// /get_difexp is one of the few routes that still hand-rolls
// jsonlite::toJSON and is genuinely double-encoded on purpose -- the SigRepo
// R client double-decodes it too (see createOmicSignature.R), so the wire
// format can't change here without breaking that. The body is
// ["<json string>"]; the inner string is either the difexp rows or a single
// { MESSAGES: "..." } row if none exist.
export async function getDifexp(signatureHashkey: string): Promise<DifexpResult> {
  const res = await fetch(
    `${API_BASE}/get_difexp?api_key=${encodeURIComponent(requireApiKey())}&signature_hashkey=${encodeURIComponent(signatureHashkey)}`
  );
  const wrapped = await res.json().catch(() => null);
  if (!res.ok) {
    throw new ApiError(extractMessage(wrapped) ?? `Request failed (${res.status})`, res.status);
  }

  const inner = Array.isArray(wrapped) && typeof wrapped[0] === "string" ? JSON.parse(wrapped[0]) : wrapped;
  const rows: DifexpRow[] = Array.isArray(inner) ? inner : [];

  if (rows.length === 1 && Object.keys(rows[0]).length === 1 && typeof rows[0].MESSAGES === "string") {
    return { rows: [], message: rows[0].MESSAGES as string };
  }
  return { rows };
}

// ---------- Collections ----------

export interface CollectionSummary {
  collection_id: number;
  collection_name: string;
  description: string | null;
  user_name: string;
  visibility: 0 | 1;
  date_created: string;
  collection_hashkey: string;
  num_signatures: number;
}

export interface CollectionMemberSignature {
  signature_hashkey: string;
  signature_name: string;
  organism: string | null;
  phenotype: string | null;
  assay_type: string;
  visibility: 0 | 1;
}

export interface CollectionDetail {
  collection: Record<string, unknown>;
  signatures: CollectionMemberSignature[];
}

export async function searchCollections(keyword?: string): Promise<CollectionSummary[]> {
  const query = new URLSearchParams({ api_key: requireApiKey() });
  if (keyword) query.set("keyword", keyword);
  const raw = await apiFetch<{ count: number; collections: CollectionSummary[] }>(
    `/collections/search?${query.toString()}`
  );
  return raw.collections ?? [];
}

export async function getCollectionDetail(collectionHashkey: string): Promise<CollectionDetail> {
  return apiFetch<CollectionDetail>("/collections/detail", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ api_key: requireApiKey(), collection_hashkey: collectionHashkey }),
  });
}

export async function createCollection(
  collectionName: string,
  description: string,
  visibility: boolean
): Promise<{ collection_hashkey: string }> {
  return apiFetch<{ collection_hashkey: string }>("/collections/create", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      collection_name: collectionName,
      description,
      visibility,
    }),
  });
}

export async function deleteCollection(collectionHashkey: string): Promise<void> {
  await apiFetch<{ MESSAGES: string }>(
    `/collections/delete?api_key=${encodeURIComponent(requireApiKey())}&collection_hashkey=${encodeURIComponent(collectionHashkey)}`,
    { method: "DELETE" }
  );
}

export async function addSignatureToCollection(collectionHashkey: string, signatureHashkey: string): Promise<void> {
  await apiFetch<{ MESSAGES: string }>("/collections/signatures/add", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      collection_hashkey: collectionHashkey,
      signature_hashkey: signatureHashkey,
    }),
  });
}

export async function removeSignatureFromCollection(collectionHashkey: string, signatureHashkey: string): Promise<void> {
  await apiFetch<{ MESSAGES: string }>(
    `/collections/signatures/remove?api_key=${encodeURIComponent(requireApiKey())}&collection_hashkey=${encodeURIComponent(collectionHashkey)}&signature_hashkey=${encodeURIComponent(signatureHashkey)}`,
    { method: "DELETE" }
  );
}

// ---------- Annotate (gene set enrichment) ----------

// Real species list msigdbr supports (static/local on the server, no
// network) -- matches the Shiny app's species picker.
export async function getMsigdbSpecies(): Promise<string[]> {
  const raw = await apiFetch<{ species: string[] }>(
    `/annotate/msigdb-species?api_key=${encodeURIComponent(requireApiKey())}`
  );
  return raw.species ?? [];
}

// The fixed Collection/Subcollection matrix with human-readable labels,
// matching the Shiny app's picker (see api/lib/msigdb_cache.R).
export interface MsigdbCollectionOption {
  collection: string;
  collection_label: string;
  subcollection: string;
}

export async function getMsigdbCollections(): Promise<MsigdbCollectionOption[]> {
  const raw = await apiFetch<{ collections: MsigdbCollectionOption[] }>(
    `/annotate/msigdb-collections?api_key=${encodeURIComponent(requireApiKey())}`
  );
  return raw.collections ?? [];
}

export interface FetchGenesetsParams {
  species: string;
  collection: string;
  subcollection?: string;
}

export interface GenesetsReadiness {
  n_genesets: number;
  source: "cache" | "live";
}

// Mirrors the Shiny app's separate "Fetch Genesets" step: resolves (from
// the on-disk cache, or live if the server allows it) before enrichment is
// runnable, so the UI can show a readiness status instead of silently
// re-resolving on every run.
export async function fetchGenesets(params: FetchGenesetsParams): Promise<GenesetsReadiness> {
  return apiFetch<GenesetsReadiness>("/annotate/genesets", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      species: params.species,
      collection: params.collection,
      subcollection: params.subcollection ?? "",
    }),
  });
}

export interface EnrichmentResultRow {
  label: string;
  pval: number;
  fdr: number;
  // Query size (gene count) for whichever signature this hit came from --
  // NOT an identifier. See signature_label for that.
  signature: number;
  geneset: number;
  overlap: number;
  background: number;
  hits: string;
  // Which input signature this hit came from; disambiguated with " (2)",
  // " (3)", etc. if two selected signatures share a signature_name.
  signature_label: string;
}

export interface EnrichmentRunSignature {
  signature_hashkey: string;
  signature_name: string;
  label: string;
  n_query: number;
}

export interface EnrichmentSkippedSignature {
  signature_hashkey: string;
  signature_name: string | null;
  reason: string;
  message: string | null;
}

export interface EnrichmentRun {
  test: "hypergeometric" | "kstest";
  collection: string;
  subcollection: string;
  fdr: number;
  geneset_source: "cache" | "live";
  // hyp_dots() rendered server-side to a PNG (hypeR's own dotplot, not a
  // reimplementation, merged into one combined plot for multi-signature
  // runs), as a data: URI ready for an <img src>.
  dotplot_png: string | null;
  // Signatures that were actually run (a signature is dropped here, and
  // listed in `skipped` instead, if e.g. a kstest run was requested but it
  // has no stored difexp).
  signatures: EnrichmentRunSignature[];
  skipped: EnrichmentSkippedSignature[];
  results: EnrichmentResultRow[];
}

export interface RunAnnotationParams {
  signatureHashkeys: string[];
  test: "hypergeometric" | "kstest";
  species?: string;
  collection: string;
  subcollection?: string;
  fdr: number;
}

export async function runAnnotation(params: RunAnnotationParams): Promise<EnrichmentRun> {
  const raw = await apiFetch<EnrichmentRun>("/annotate/run", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      signature_hashkeys: params.signatureHashkeys,
      test: params.test,
      species: params.species ?? "Homo sapiens",
      collection: params.collection,
      subcollection: params.subcollection ?? "",
      fdr: params.fdr,
    }),
  });
  return { ...raw, results: raw.results ?? [], signatures: raw.signatures ?? [], skipped: raw.skipped ?? [] };
}

// ---------- Signature export / basket download ----------

// Extracts a filename from a Content-Disposition header, falling back to
// `fallback` if the header is missing or doesn't have one.
function filenameFromContentDisposition(res: Response, fallback: string): string {
  const header = res.headers.get("Content-Disposition") ?? "";
  const match = header.match(/filename="?([^";]+)"?/i);
  return match ? match[1] : fallback;
}

async function downloadBlob(path: string, init: RequestInit | undefined, fallbackFilename: string): Promise<void> {
  const res = await fetch(`${API_BASE}${path}`, init);
  if (!res.ok) {
    const data = await res.json().catch(() => null);
    throw new ApiError(extractMessage(data) ?? `Request failed (${res.status})`, res.status);
  }
  const blob = await res.blob();
  const filename = filenameFromContentDisposition(res, fallbackFilename);
  const url = URL.createObjectURL(blob);
  const link = document.createElement("a");
  link.href = url;
  link.download = filename;
  document.body.appendChild(link);
  link.click();
  link.remove();
  URL.revokeObjectURL(url);
}

export async function downloadSignatureExport(signatureHashkey: string): Promise<void> {
  await downloadBlob(
    `/signatures/export?api_key=${encodeURIComponent(requireApiKey())}&signature_hashkey=${encodeURIComponent(signatureHashkey)}`,
    undefined,
    `signature_${signatureHashkey}.rds`
  );
}

export async function downloadSignatureBasket(signatureHashkeys: string[]): Promise<void> {
  await downloadBlob(
    "/signatures/export-batch",
    {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ api_key: requireApiKey(), signature_hashkeys: signatureHashkeys }),
    },
    "signature_basket.zip"
  );
}

// Re-adds a signature from an .rds file shaped like /signatures/export's own
// output (list(metadata, signature, difexp)) -- the "Upload" counterpart to
// downloadSignatureExport(). api_key/visibility go in the query string, not
// the multipart body: Plumber's multi+rds parser combination doesn't bind
// plain text fields to route parameters, only the file part.
export async function uploadSignature(
  file: File,
  visibility: boolean
): Promise<{ signature_hashkey: string; MESSAGES: string }> {
  const query = new URLSearchParams({ api_key: requireApiKey(), visibility: String(visibility) });
  const body = new FormData();
  body.append("signature_file", file);
  return apiFetch<{ signature_hashkey: string; MESSAGES: string }>(`/signatures/upload?${query.toString()}`, {
    method: "POST",
    body,
  });
}
