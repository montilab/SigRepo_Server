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

export interface SignatureSummary {
  signature_hashkey: string;
  signature_name: string;
  organism: string | null;
  phenotype: string | null;
  assay_type: string;
  description: string | null;
  visibility: 0 | 1;
  user_name: string;
  date_created: string;
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
