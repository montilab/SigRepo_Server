// Client-side "basket" for bulk-downloading signatures -- ported from the
// Shiny app's basket (shiny/modules/signature_module.R), which is also
// purely session-local (a Shiny reactiveVal, never written to the
// database). Persisted to localStorage here so it survives a page reload,
// unlike Shiny's version which is lost when the tab closes.
//
// Scope: the signed-in session, and nothing wider. This is a bookmark list for
// the work in front of you, not saved state -- signing out discards it, the
// same as Shiny's original.
//
// It used to be a single localStorage entry with no owner, which meant it
// outlived the session that built it: log out, sign in as somebody else, and
// their basket was still sitting there. A basket entry carries
// signature_name, organism and phenotype for private signatures
// (visibility 0), so that also disclosed the existence and metadata of
// signatures the new account may have no right to see. (The download itself
// was always authorized server-side against the caller's api_key, so this was
// a UI disclosure, not a way to obtain restricted data.)
//
// sessionStorage rather than localStorage: it still survives a page reload,
// which is the whole reason for persisting at all, but it dies with the tab
// instead of accumulating other people's signature names on a shared machine.
import { useSyncExternalStore } from "react";
import { onAuthChange, type SignatureSummary } from "./api/client";

export interface BasketItem {
  signature_hashkey: string;
  signature_name: string;
  organism: string | null;
  phenotype: string | null;
  assay_type: string;
  visibility: 0 | 1;
}

const BASKET_KEY = "sr-basket";

// Baskets written by earlier builds went to localStorage, where they persist
// until something removes them. Clear that on load so an existing basket does
// not survive this change and keep behaving the old way.
try {
  localStorage.removeItem(BASKET_KEY);
} catch {
  // Private mode or storage disabled: nothing to clean up.
}

const listeners = new Set<() => void>();

function load(): BasketItem[] {
  try {
    const raw = sessionStorage.getItem(BASKET_KEY);
    return raw ? (JSON.parse(raw) as BasketItem[]) : [];
  } catch {
    return [];
  }
}

let basket: BasketItem[] = load();

function persist() {
  try {
    sessionStorage.setItem(BASKET_KEY, JSON.stringify(basket));
  } catch {
    // Storage full or unavailable -- the in-memory basket still works for
    // this page view, so keep going rather than breaking the click.
  }
  listeners.forEach((listener) => listener());
}

// Any change of signed-in user empties the basket: signing out, signing in,
// or switching accounts in the same tab. Emptying on every transition rather
// than keying storage by user is what makes the guarantee simple -- there is
// never a stored basket belonging to somebody who is not currently signed in.
onAuthChange(() => {
  if (basket.length === 0) return;
  basket = [];
  persist();
});

function toBasketItem(sig: SignatureSummary): BasketItem {
  return {
    signature_hashkey: sig.signature_hashkey,
    signature_name: sig.signature_name,
    organism: sig.organism,
    phenotype: sig.phenotype,
    assay_type: sig.assay_type,
    visibility: sig.visibility,
  };
}

export function isInBasket(signatureHashkey: string): boolean {
  return basket.some((b) => b.signature_hashkey === signatureHashkey);
}

// Returns false (no-op) if the signature is already in the basket.
export function addToBasket(sig: SignatureSummary): boolean {
  if (isInBasket(sig.signature_hashkey)) return false;
  basket = [...basket, toBasketItem(sig)];
  persist();
  return true;
}

export function removeFromBasket(signatureHashkey: string) {
  basket = basket.filter((b) => b.signature_hashkey !== signatureHashkey);
  persist();
}

export function clearBasket() {
  basket = [];
  persist();
}

function subscribe(listener: () => void): () => void {
  listeners.add(listener);
  return () => listeners.delete(listener);
}

function getSnapshot(): BasketItem[] {
  return basket;
}

export function useBasket(): BasketItem[] {
  return useSyncExternalStore(subscribe, getSnapshot);
}
