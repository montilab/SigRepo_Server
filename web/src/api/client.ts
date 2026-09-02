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

// Anything holding per-user state needs to know when the signed-in user
// changes, not just when the app first loads -- a login, a logout, or a switch
// between accounts in the same tab all happen without a page reload. The
// basket subscribes to this; see web/src/basket.ts.
const authListeners = new Set<(auth: AuthUser | null) => void>();

export function onAuthChange(listener: (auth: AuthUser | null) => void): () => void {
  authListeners.add(listener);
  return () => {
    authListeners.delete(listener);
  };
}

function setAuth(auth: AuthUser | null) {
  const previousUser = currentAuth?.user_name ?? null;
  currentAuth = auth;
  if (auth) localStorage.setItem(AUTH_KEY, JSON.stringify(auth));
  else localStorage.removeItem(AUTH_KEY);
  if ((auth?.user_name ?? null) !== previousUser) {
    authListeners.forEach((listener) => listener(auth));
  }
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

export interface RegistrationInput {
  userName: string;
  password: string;
  email: string;
  firstName?: string;
  lastName?: string;
  affiliation?: string;
}

// Both of these return the server's own message rather than a fixed string:
// registration reports whether the admin notification also went out, and the
// reset reply is deliberately identical whether or not the account exists, so
// neither can be summarised safely on the client.
export async function register(input: RegistrationInput): Promise<string> {
  const data = await apiFetch<{ MESSAGES?: string | string[] }>("/register", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      user_name: input.userName,
      password: input.password,
      user_email: input.email,
      user_first: input.firstName ?? "",
      user_last: input.lastName ?? "",
      user_affiliation: input.affiliation ?? "",
    }),
  });
  return extractMessage(data) ?? "Registration submitted.";
}

export async function requestPasswordReset(identifier: string): Promise<string> {
  const data = await apiFetch<{ MESSAGES?: string | string[] }>("/forgot_password", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ identifier }),
  });
  return extractMessage(data) ?? "If that account exists, a temporary password has been sent.";
}

export interface GeneSearchHit {
  signature_hashkey: string;
  signature_name: string;
  assay_type: string | null;
  organism: string | null;
  phenotype: string | null;
  n_overlap: number;
  n_signature_genes: number;
  n_query_genes: number;
  jaccard: number;
  matched_genes: string | null;
}

export interface GeneSearchResult {
  query_size: number;
  source_signature: string | null;
  total: number;
  hits: GeneSearchHit[];
}

// Find signatures by the genes they contain. Pass `genes` to search a list, or
// `signatureHashkey` to use that signature's own genes -- the server resolves
// them and excludes the source signature from its own results.
export async function searchSignaturesByGenes(input: {
  genes?: string[];
  signatureHashkey?: string;
  limit?: number;
  minOverlap?: number;
}): Promise<GeneSearchResult> {
  return apiFetch<GeneSearchResult>("/signatures/search_by_genes", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      genes: input.genes ?? [],
      signature_hashkey: input.signatureHashkey ?? "",
      limit: input.limit ?? 20,
      min_overlap: input.minOverlap ?? 1,
    }),
  });
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

export interface NamedCount {
  name: string;
  value: number;
}

export interface RecentSignature {
  signature_hashkey: string;
  signature_name: string;
  assay_type: string;
  user_name: string;
  date_created: string;
}

export interface Insights {
  total_signatures: number;
  total_users: number;
  total_organisms: number;
  total_assays: number;
  by_organism: NamedCount[];
  by_assay: NamedCount[];
  top_contributors: NamedCount[];
  recent_signatures: RecentSignature[];
}

export async function getInsights(recentLimit = 5): Promise<Insights> {
  const query = new URLSearchParams({ api_key: requireApiKey(), recent_limit: String(recentLimit) });
  return apiFetch<Insights>(`/insights?${query.toString()}`);
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

export type SignatureSortKey =
  | "signature_name" | "organism" | "assay_type" | "direction_type" | "phenotype"
  | "sample_type" | "platform_name" | "year" | "user_name" | "visibility";

export interface SearchSignaturesParams {
  organism?: string;
  phenotype?: string;
  assay_type?: string;
  keyword?: string;
  limit?: number;
  offset?: number;
  sortBy?: SignatureSortKey;
  sortDir?: "asc" | "desc";
}

export interface SignaturesPage {
  rows: SignatureSummary[];
  total: number;
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

// Server-side paginated variant (DT `server = TRUE`): asks for one page and
// returns both that page's rows and the TOTAL count of matching rows, so the
// caller can render pager controls without pulling every row up front.
export async function searchSignaturesPage(params: SearchSignaturesParams = {}): Promise<SignaturesPage> {
  const query = new URLSearchParams({ api_key: requireApiKey() });
  if (params.organism) query.set("organism", params.organism);
  if (params.phenotype) query.set("phenotype", params.phenotype);
  if (params.assay_type) query.set("assay_type", params.assay_type);
  if (params.keyword) query.set("keyword", params.keyword);
  if (params.limit != null) query.set("limit", String(params.limit));
  if (params.offset != null) query.set("offset", String(params.offset));
  // Sorting is applied server-side: the client only ever holds one page, so
  // sorting here would reorder within the page and look like it had sorted the
  // whole repository.
  if (params.sortBy) query.set("sort_by", params.sortBy);
  if (params.sortDir) query.set("sort_dir", params.sortDir);

  const raw = await apiFetch<{ count: number; signatures: SignatureSummary[] }>(
    `/signatures/search?${query.toString()}`
  );
  return { rows: raw.signatures ?? [], total: Number(raw.count) || 0 };
}

export interface LeadingEdge {
  geneset_label: string;
  signature_name: string | null;
  n_total: number;
  es_score: number;
  es_index: number;
  es_direction: "positive" | "negative";
  n_leading: number;
  leading_edge_genes: string[];
  hit_positions: number[];
  curve: { position: number[]; running_score: number[] };
}

// Running-enrichment curve for ONE gene set. Fetched per gene set rather than
// with the run: a run returns hundreds of rows and only the one a reader opens
// needs a curve.
export async function fetchLeadingEdge(params: {
  signatureHashkey: string;
  genesetLabel: string;
  species: string;
  collection: string;
  subcollection?: string;
}): Promise<LeadingEdge> {
  return apiFetch<LeadingEdge>("/annotate/leading_edge", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      signature_hashkey: params.signatureHashkey,
      geneset_label: params.genesetLabel,
      species: params.species,
      collection: params.collection,
      subcollection: params.subcollection ?? "",
    }),
  });
}

// ---------- Compare (OmicSignature::compare_omic_signatures) ----------

// A labeled similarity matrix: values[i][j] is row i vs col j (null where the
// comparison couldn't be computed, e.g. a KS column with no difexp).
export interface CompareMatrix {
  rows: string[];
  cols: string[];
  values: (number | null)[][];
}

// One label pairing carries the measures relevant to the method: overlap ->
// jaccard/pvalue/counts; ks_rank/ks_score -> score/pvalue.
export interface ComparePairing {
  jaccard?: CompareMatrix;
  pvalue?: CompareMatrix;
  counts?: CompareMatrix;
  score?: CompareMatrix;
  // Per-signature retained feature-set size (split out of the counts matrix,
  // which carries it as an extra "size" row/col server-side).
  sizes?: { name: string; size: number | null }[];
}

export type CompareMeasure = "jaccard" | "pvalue" | "counts" | "score";

// Which real group_label each "level" resolved to, per signature and per input
// list -- so levels can be named honestly instead of guessed from "level1".
export interface CompareLabelOrder {
  list: string;
  signature: string;
  levels: string[];
}

// One signature in matrix row/col order, so the UI can map a clicked heatmap
// cell back to real signatures (for the GSEA leading-edge drill-down).
export interface CompareSignatureMeta {
  name: string;
  hashkey: string;
  direction_type: string | null;
}

export interface CompareResult {
  method: string;
  primary_measure: CompareMeasure;
  measures: CompareMeasure[];
  pairings: string[];
  comparisons: Record<string, ComparePairing>;
  signatures: CompareSignatureMeta[];
  // Set only for a two-list run: matrices are rectangular, rows = query
  // (signatures), cols = reference.
  reference_signatures: CompareSignatureMeta[] | null;
  two_list: boolean;
  label_order: CompareLabelOrder[] | null;
  skipped: string[];
}

// GSEA enrichment-plot data for one geneset-vs-ranking pair.
export interface LeadingEdgeResult {
  geneset_name: string;
  ranking_name: string;
  geneset_label: string;
  ranking_label: string;
  ranking_contrast: string;
  n_ranked: number;
  n_geneset: number;
  NES: number | null;
  pvalue: number | null;
  ES: number | null;
  leading_edge: string[];
  curve: { rank: number; ES: number }[];
  ticks: number[];
}

export interface LeadingEdgeParams {
  geneset_hashkey: string;
  ranking_hashkey: string;
  geneset_level?: number;
  ranking_level?: number;
  score_cutoff?: number;
  adj_p_cutoff?: number;
  min_features?: number;
}

export async function compareLeadingEdge(params: LeadingEdgeParams): Promise<LeadingEdgeResult> {
  return apiFetch<LeadingEdgeResult>("/signatures/compare/leading_edge", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ api_key: requireApiKey(), ...params }),
  });
}

export interface CompareParams {
  signature_hashkeys: string[];
  // When set, runs a two-list (query vs reference) comparison instead of
  // comparing signature_hashkeys against themselves.
  reference_hashkeys?: string[];
  method: string;
  score_cutoff?: number;
  adj_p_cutoff?: number;
  min_features?: number;
  max_feature?: number;
  // {hashkey: [level1, level2]} -- explicit level matching for signatures whose
  // group_labels differ (e.g. treated/control vs up/down).
  label_pairing?: Record<string, string[]>;
  label_pairing2?: Record<string, string[]>;
  adjust?: boolean;
  gsea_score?: "NES" | "ES";
}

export async function compareSignatures(params: CompareParams): Promise<CompareResult> {
  const raw = await apiFetch<CompareResult>("/signatures/compare", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ api_key: requireApiKey(), ...params }),
  });
  // Single-element fields auto-unbox server-side; normalize the list-shaped ones.
  const asArray = <T,>(v: T[] | T | null | undefined): T[] =>
    Array.isArray(v) ? v : v == null ? [] : [v];
  return {
    ...raw,
    measures: asArray(raw.measures),
    pairings: asArray(raw.pairings),
    signatures: asArray(raw.signatures),
    skipped: asArray(raw.skipped),
    reference_signatures: raw.reference_signatures ? asArray(raw.reference_signatures) : null,
    label_order: raw.label_order ? asArray(raw.label_order) : null,
  };
}

export interface SignatureFeature {
  probe_id?: string;
  feature_id?: number;
  score?: number;
  group_label?: string;
  // Joined in by the API from the assay's reference table
  // (attach_feature_labels), so a reader sees the gene rather than
  // OmicSignature's positional "feature_1" filler. gene_symbol is absent for
  // assay types whose reference table has no symbol column.
  feature_name?: string | null;
  gene_symbol?: string | null;
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

// ---------- Rummagene (literature-mined gene sets) ----------

export interface RummageneHit {
  term: string;
  description: string | null;
  n_geneset: number | null;
  n_overlap: number | null;
  odds_ratio: number | null;
  pvalue: number | null;
  adj_pvalue: number | null;
  n_sets: number | null;
  pmcid: string | null;
  title: string | null;
  doi: string | null;
  year: number | null;
  pmc_url: string | null;
}

export interface RummageneResult {
  total_count: number;
  query_size: number;
  hits: RummageneHit[];
  signature_name?: string | null;
}

// Enrich a signature's genes (resolved server-side from its hashkey) against
// Rummagene's ~1M literature-mined gene sets, returning matches + PMC links.
export async function rummageneEnrich(params: {
  signatureHashkey?: string;
  genes?: string[];
  limit?: number;
}): Promise<RummageneResult> {
  const raw = await apiFetch<RummageneResult>("/rummagene/enrich", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({
      api_key: requireApiKey(),
      signature_hashkey: params.signatureHashkey,
      genes: params.genes,
      limit: params.limit ?? 25,
    }),
  });
  // A single hit can auto-unbox server-side; normalize hits back to an array.
  return { ...raw, hits: Array.isArray(raw.hits) ? raw.hits : raw.hits ? [raw.hits] : [] };
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

  // --- hypeR-GEM runs only ---
  // GEM reports the genes it hit under `gene_hits` rather than `hits`, and
  // additionally reports which metabolites mapped onto them -- the step the
  // hypeR path does not have. The weighted method replaces the plain
  // `overlap` count with `weighted_overlap`, so both are optional here.
  gene_hits?: string;
  metabolite_hits?: string;
  num_met_hits?: number;
  ratio_met_hits?: number;
  weighted_overlap?: number;
}

export interface EnrichmentRunSignature {
  signature_hashkey: string;
  signature_name: string;
  label: string;
  n_query: number;
  // How many gene sets passed the cutoff for THIS signature.
  n_enriched: number;
  // hypeR's own hyp$info, verbatim: hypeR version, signature head/size/type,
  // geneset collection, background, and the cutoffs and test used. Free-form
  // on purpose -- it is a reproducibility record whose keys are hypeR's to
  // choose, and the GEM path fills the same slot with its own equivalents.
  info: Record<string, string>;
  // This signature's own enriched gene sets. Previously every signature's
  // results arrived interleaved in one array keyed by signature_label.
  results: EnrichmentResultRow[];
}

export interface EnrichmentSkippedSignature {
  signature_hashkey: string;
  signature_name: string | null;
  reason: string;
  message: string | null;
}

export type EnrichmentTest = "hypergeometric" | "kstest" | "gsea" | "gem_hypergeo" | "gem_weighted";

export interface EnrichmentRun {
  test: EnrichmentTest;
  collection: string;
  subcollection: string;
  fdr: number;
  geneset_source: "cache" | "live";
  // Signatures that were actually run (a signature is dropped here, and
  // listed in `skipped` instead, if e.g. a kstest run was requested but it
  // has no stored difexp).
  // One entry per signature that ran, each carrying its own info and results
  // -- the multihyp shape hypeR::rctbl_mhyp() renders.
  signatures: EnrichmentRunSignature[];
  skipped: EnrichmentSkippedSignature[];

  // --- hypeR-GEM runs only ---
  // Which metabolite identifier the metabolic model was keyed on, and how far
  // the metabolite -> gene mapping got. Worth surfacing: a GEM run with a
  // healthy metabolite count but few mapped genes is thin for a reason the
  // p-values alone do not show.
  reference_key?: string;
  gem_method?: "weighted" | "unweighted";
  n_metabolites?: number;
  n_genes?: number;
}

export interface RunAnnotationParams {
  signatureHashkeys: string[];
  test: EnrichmentTest;
  species?: string;
  collection: string;
  subcollection?: string;
  fdr: number;
  // GEM only. Directional models distinguish the metabolites a reaction
  // consumes from the ones it produces; ignored by the hypeR tests.
  gemDirectional?: boolean;
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
      gem_directional: params.gemDirectional ?? true,
    }),
  });
  return {
    ...raw,
    signatures: (raw.signatures ?? []).map((s) => ({ ...s, results: s.results ?? [] })),
    skipped: raw.skipped ?? [],
  };
}

// The dot plot is no longer part of the run response -- it is fetched on
// demand, so a run does not pay for a figure nobody opens. Fetched as a blob
// and handed to the caller through downloadBlob's synthetic-click pattern
// (see below) rather than exposed as a plain URL: a rendered <a href> would
// put api_key in the DOM, where link hover, "Copy Link Address", view-source
// and DevTools can all read it, and it would persist in history and the
// Downloads-list entry. Routing it through fetch() instead means the key
// only ever travels as a request, never as an attribute.
export async function downloadDotplot(params: {
  signatureHashkeys: string[];
  test: EnrichmentTest;
  species?: string;
  collection: string;
  subcollection?: string;
  fdr: number;
}): Promise<void> {
  const q = new URLSearchParams({
    api_key: requireApiKey(),
    signature_hashkeys: params.signatureHashkeys.join(","),
    test: params.test,
    species: params.species ?? "Homo sapiens",
    collection: params.collection,
    subcollection: params.subcollection ?? "",
    fdr: String(params.fdr),
  });
  await downloadBlob(`/annotate/dotplot?${q.toString()}`, undefined, "enrichment_dotplot.png");
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

export interface RummageneCatalogRow {
  rummagene_catalog_id: number;
  term: string;
  pmcid: string;
  pmid: string | null;
  title: string | null;
  year: number | null;
  doi: string | null;
  description: string | null;
  organism: string;
  assay_type: string;
  mesh_evidence: string;
  n_genes: number;
}

export type RummageneCatalogSortKey =
  | "term" | "title" | "year" | "n_genes" | "organism" | "assay_type";

export interface RummageneCatalogParams {
  q?: string;
  organism?: string;
  assay_type?: string;
  year_min?: number;
  year_max?: number;
  n_genes_min?: number;
  n_genes_max?: number;
  limit?: number;
  offset?: number;
  sortBy?: RummageneCatalogSortKey;
  sortDir?: "asc" | "desc";
}

export interface RummageneCatalogPage {
  rows: RummageneCatalogRow[];
  total: number;
}

// Server-side paged, exactly like searchSignaturesPage: the client holds one
// page, so sorting and filtering must happen server-side or they would only
// reorder the visible page and look like they had sorted the whole catalog.
export async function searchRummageneCatalog(
  params: RummageneCatalogParams = {}
): Promise<RummageneCatalogPage> {
  const query = new URLSearchParams({ api_key: requireApiKey() });
  if (params.q) query.set("q", params.q);
  if (params.organism) query.set("organism", params.organism);
  if (params.assay_type) query.set("assay_type", params.assay_type);
  if (params.year_min != null) query.set("year_min", String(params.year_min));
  if (params.year_max != null) query.set("year_max", String(params.year_max));
  if (params.n_genes_min != null) query.set("n_genes_min", String(params.n_genes_min));
  if (params.n_genes_max != null) query.set("n_genes_max", String(params.n_genes_max));
  if (params.limit != null) query.set("limit", String(params.limit));
  if (params.offset != null) query.set("offset", String(params.offset));
  if (params.sortBy) query.set("sort_by", params.sortBy);
  if (params.sortDir) query.set("sort_dir", params.sortDir);

  const raw = await apiFetch<{ count: number; rows: RummageneCatalogRow[] }>(
    `/rummagene/catalog?${query.toString()}`
  );
  return { rows: raw.rows ?? [], total: Number(raw.count) || 0 };
}

// The detail route returns a curated subset of fields, not the row plus
// genes: get_rummagene_catalog_entry() (api/lib/rummagene_catalog.R)
// hand-builds a list of exactly these twelve fields. rummagene_catalog_id and
// n_genes are absent -- not null -- so this deliberately does not extend
// RummageneCatalogRow.
export interface RummageneCatalogEntry {
  term: string;
  pmcid: string;
  pmid: string | null;
  title: string | null;
  year: number | null;
  doi: string | null;
  description: string | null;
  organism: string;
  assay_type: string;
  mesh_evidence: string;
  gene_symbols: string[];
  feature_names: string[];
}

// Fetched only when a row is expanded. The list endpoint omits the gene columns
// on purpose: at ~135k rows they would dominate every page response.
//
// gene_symbols/feature_names go through toArray(): the API serializes with
// auto_unbox = TRUE (api/api.R) and the server builds both fields with plain
// strsplit(...)[[1]], so a one-gene entry comes back as a bare JSON string
// ("TP53") rather than a one-element array -- the same unboxing toArray()
// already normalizes for getVocabulary() above.
export async function getRummageneCatalogEntry(term: string): Promise<RummageneCatalogEntry> {
  const query = new URLSearchParams({ api_key: requireApiKey(), term });
  const raw = await apiFetch<RummageneCatalogEntry>(`/rummagene/catalog/entry?${query.toString()}`);
  return { ...raw, gene_symbols: toArray(raw.gene_symbols), feature_names: toArray(raw.feature_names) };
}

// The server returns signature_name alongside the hashkey so a caller (e.g. a
// confirmation toast) doesn't have to look it up separately.
export async function pullRummageneSignature(
  term: string
): Promise<{ signature_hashkey: string; signature_name: string }> {
  return apiFetch<{ signature_hashkey: string; signature_name: string }>("/rummagene/pull", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ api_key: requireApiKey(), term }),
  });
}
