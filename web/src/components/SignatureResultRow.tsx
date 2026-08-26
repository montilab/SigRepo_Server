import { useMemo, useState } from "react";
import { ChevronDown, ChevronRight } from "lucide-react";
import DataTable, { type Column } from "./DataTable";
import LeadingEdgePlot from "./LeadingEdgePlot";
import type { EnrichmentRunSignature } from "../api/client";

// One signature as a collapsed row that expands to its own gene sets, its
// hyp$info, and (for a ranked run) the leading-edge curve.
//
// This is hypeR::rctbl_mhyp()'s shape: a row per signature with an expander,
// rather than a separate table below listing every signature's gene sets
// interleaved. Keeping it in its own component stops AnnotatePage growing
// another hundred lines.
export default function SignatureResultRow({
  signature,
  expanded,
  onToggle,
  isGsea,
  species,
  collection,
  subcollection,
}: {
  signature: EnrichmentRunSignature;
  expanded: boolean;
  onToggle: () => void;
  isGsea: boolean;
  species: string;
  collection: string;
  subcollection?: string;
}) {
  const [openGeneset, setOpenGeneset] = useState<string | null>(null);

  const rows = useMemo(
    () => (signature.results ?? []).map((r, i) => ({ ...r, rowId: `${signature.label}::${r.label}::${i}` })),
    [signature]
  );

  const columns: Column<(typeof rows)[number]>[] = useMemo(
    () => [
      { key: "label", label: "Gene set", render: (r) => <span className="cell-strong">{r.label}</span> },
      {
        key: "fdr",
        label: "FDR",
        align: "right",
        render: (r) => <span className="cell-mono enrich-num">{r.fdr < 0.0001 ? r.fdr.toExponential(1) : r.fdr.toFixed(4)}</span>,
      },
      {
        key: "pval",
        label: "P-value",
        align: "right",
        render: (r) => <span className="cell-mono enrich-num">{r.pval.toExponential(1)}</span>,
      },
      {
        key: "overlap",
        label: "Overlap",
        align: "right",
        render: (r) => <span className="cell-mono">{r.weighted_overlap ?? r.overlap}/{r.geneset}</span>,
      },
      {
        key: "hits",
        label: "Hits",
        render: (r) => {
          const hits = r.gene_hits ?? r.hits;
          return <span className="cell-sub enrich-hits" title={hits}>{hits}</span>;
        },
      },
    ],
    []
  );

  return (
    <div className={`srr${expanded ? " srr-open" : ""}`}>
      <button className="srr-head" onClick={onToggle} aria-expanded={expanded}>
        {expanded ? <ChevronDown size={15} /> : <ChevronRight size={15} />}
        <span className="srr-name">{signature.signature_name}</span>
        <span className="srr-metric"><b>{signature.n_query}</b><span className="cell-sub">features</span></span>
        <span className="srr-metric"><b>{signature.n_enriched}</b><span className="cell-sub">enriched</span></span>
        <span className="srr-metric srr-metric-wide">
          <b>{signature.info?.["Background"] ?? "—"}</b><span className="cell-sub">background</span>
        </span>
      </button>

      {expanded && (
        <div className="srr-body">
          {Object.keys(signature.info ?? {}).length > 0 && (
            <div className="hyp-info">
              {Object.entries(signature.info).map(([k, v]) => (
                <span className="hyp-info-item" key={k}>
                  <span className="hyp-info-key">{k}</span>
                  <span className="hyp-info-val" title={v}>{v}</span>
                </span>
              ))}
            </div>
          )}

          {rows.length === 0 ? (
            <p className="muted-note">No gene sets pass the current FDR cutoff for this signature.</p>
          ) : (
            <DataTable
              columns={columns}
              rows={rows}
              rowKey="rowId"
              pageSize={10}
              selectedKey={openGeneset}
              onSelectRow={isGsea ? (r) => setOpenGeneset(openGeneset === r.rowId ? null : r.rowId) : undefined}
            />
          )}

          {isGsea && openGeneset && (
            <div className="le-wrap">
              <div className="le-head">
                <h4 className="detail-section-title" style={{ margin: 0 }}>
                  {rows.find((r) => r.rowId === openGeneset)?.label}
                </h4>
                <button className="btn btn-ghost btn-sm" onClick={() => setOpenGeneset(null)}>Close</button>
              </div>
              <LeadingEdgePlot
                signatureHashkey={signature.signature_hashkey}
                genesetLabel={rows.find((r) => r.rowId === openGeneset)?.label ?? ""}
                species={species}
                collection={collection}
                subcollection={subcollection}
              />
            </div>
          )}
        </div>
      )}
    </div>
  );
}
