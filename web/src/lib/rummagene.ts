// Shared with RummagenePanel.tsx (a signature's related-gene-sets panel) and
// RummagenePage.tsx (the catalog browse page) -- both need to turn a raw
// Rummagene term into something a person can read, and neither should carry
// its own copy of this.
//
// Rummagene terms look like
// "PMC6819084-elife-47013-supp2.xlsx-IPA_mono_upstream-...". Trim the PMC id
// and the source filename into something a person can read.
export function tidyTerm(term: string): string {
  const parts = term.split("-");
  return parts.length > 2
    ? parts.slice(2).join(" ").replace(/_/g, " ")
    : term.replace(/_/g, " ");
}
