// Client-side "basket" for bulk-downloading signatures -- ported from the
// Shiny app's basket (shiny/modules/signature_module.R), which is also
// purely session-local (a Shiny reactiveVal, never written to the
// database). Persisted to localStorage here so it survives a page reload,
// unlike Shiny's version which is lost when the tab closes.
import { useSyncExternalStore } from "react";
import type { SignatureSummary } from "./api/client";

export interface BasketItem {
  signature_hashkey: string;
  signature_name: string;
  organism: string | null;
  phenotype: string | null;
  assay_type: string;
  visibility: 0 | 1;
}

const BASKET_KEY = "sr-basket";
const listeners = new Set<() => void>();

function load(): BasketItem[] {
  try {
    const raw = localStorage.getItem(BASKET_KEY);
    return raw ? (JSON.parse(raw) as BasketItem[]) : [];
  } catch {
    return [];
  }
}

let basket: BasketItem[] = load();

function persist() {
  localStorage.setItem(BASKET_KEY, JSON.stringify(basket));
  listeners.forEach((listener) => listener());
}

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
