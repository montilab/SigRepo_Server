// Typed client for the SigRepo Plumber API.
//
// Two quirks of that API this module absorbs so pages don't have to:
//   1. Responses are double-encoded — the json serializer wraps a
//      jsonlite::toJSON() string, so the body arrives as ["<json string>"].
//      unwrap() parses through that to the real object.
//   2. Single-value fields are auto-unboxed to scalars (one distinct
//      organism comes back as "Homo sapiens", not ["Homo sapiens"]).
//      toArray() normalizes list-shaped fields back to arrays.
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

function unwrap(raw: unknown): unknown {
  if (Array.isArray(raw) && raw.length === 1 && typeof raw[0] === "string") {
    try {
      return JSON.parse(raw[0]);
    } catch {
      return raw[0];
    }
  }
  return raw;
}

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
  const data = unwrap(await res.json().catch(() => null));
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
