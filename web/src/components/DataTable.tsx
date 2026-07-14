import { useState, type ReactNode } from "react";
import { ChevronDown, ChevronUp, ChevronsUpDown } from "lucide-react";

export interface Column<T> {
  key: keyof T;
  label: string;
  align?: "left" | "right";
  render?: (row: T) => ReactNode;
}

export default function DataTable<T extends object>({
  columns,
  rows,
  rowKey,
  selectedKey,
  onSelectRow,
  pageSize = 8,
  emptyLabel = "No rows to display",
}: {
  columns: Column<T>[];
  rows: T[];
  rowKey: keyof T;
  selectedKey?: string | null;
  onSelectRow?: (row: T) => void;
  pageSize?: number;
  emptyLabel?: string;
}) {
  const [sortKey, setSortKey] = useState<keyof T | null>(null);
  const [sortAsc, setSortAsc] = useState(true);
  const [page, setPage] = useState(0);

  const sorted = [...rows].sort((a, b) => {
    if (!sortKey) return 0;
    const av = a[sortKey];
    const bv = b[sortKey];
    if (typeof av === "number" && typeof bv === "number") return sortAsc ? av - bv : bv - av;
    return sortAsc ? String(av ?? "").localeCompare(String(bv ?? "")) : String(bv ?? "").localeCompare(String(av ?? ""));
  });

  const pageCount = Math.max(1, Math.ceil(sorted.length / pageSize));
  const safePage = Math.min(page, pageCount - 1);
  const paged = sorted.slice(safePage * pageSize, safePage * pageSize + pageSize);

  function toggleSort(key: keyof T) {
    if (sortKey === key) setSortAsc((a) => !a);
    else {
      setSortKey(key);
      setSortAsc(true);
    }
  }

  return (
    <div className="dt">
      <div className="dt-scroll">
        <table className="dt-table">
          <thead>
            <tr>
              {columns.map((col) => (
                <th
                  key={String(col.key)}
                  className={col.align === "right" ? "dt-right" : ""}
                  onClick={() => toggleSort(col.key)}
                >
                  <span className="dt-th">
                    {col.label}
                    {sortKey === col.key ? (
                      sortAsc ? (
                        <ChevronUp size={13} />
                      ) : (
                        <ChevronDown size={13} />
                      )
                    ) : (
                      <ChevronsUpDown size={13} className="dt-sort-idle" />
                    )}
                  </span>
                </th>
              ))}
            </tr>
          </thead>
          <tbody>
            {paged.map((row) => {
              const key = String(row[rowKey]);
              return (
                <tr
                  key={key}
                  className={
                    (onSelectRow ? "dt-clickable" : "") + (selectedKey === key ? " dt-selected" : "")
                  }
                  onClick={() => onSelectRow?.(row)}
                >
                  {columns.map((col) => (
                    <td key={String(col.key)} className={col.align === "right" ? "dt-right" : ""}>
                      {col.render ? col.render(row) : String(row[col.key] ?? "")}
                    </td>
                  ))}
                </tr>
              );
            })}
            {paged.length === 0 && (
              <tr>
                <td colSpan={columns.length} className="dt-empty">
                  {emptyLabel}
                </td>
              </tr>
            )}
          </tbody>
        </table>
      </div>
      {sorted.length > pageSize && (
        <div className="dt-foot">
          <span className="dt-count">
            {safePage * pageSize + 1}–{Math.min(sorted.length, safePage * pageSize + pageSize)} of {sorted.length}
          </span>
          <div className="dt-pager">
            <button className="btn btn-ghost btn-sm" disabled={safePage === 0} onClick={() => setPage(safePage - 1)}>
              Previous
            </button>
            <button
              className="btn btn-ghost btn-sm"
              disabled={safePage >= pageCount - 1}
              onClick={() => setPage(safePage + 1)}
            >
              Next
            </button>
          </div>
        </div>
      )}
    </div>
  );
}
