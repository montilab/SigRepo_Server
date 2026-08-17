// Shared client-side approximations of the API's own authorization rules,
// used to hide actions that would just 403 rather than as the real
// authority -- the API enforces these for real on every request.
import { getAuth } from "./api/client";

// Mirrors delete_signature()'s rule: admin, or editor + owner. Doesn't know
// about per-signature signature_access grants, so it can hide the button in
// a few cases the API would actually allow.
export function canDeleteSignature(ownerUserName: string): boolean {
  const auth = getAuth();
  if (!auth) return false;
  if (auth.user_role === "admin") return true;
  return auth.user_role === "editor" && auth.user_name === ownerUserName;
}

// Mirrors build_signature_from_upload()'s check: editor or admin, any owner.
export function canUploadSignature(): boolean {
  const auth = getAuth();
  return auth?.user_role === "editor" || auth?.user_role === "admin";
}
