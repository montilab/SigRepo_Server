import { useState } from "react";

export interface Column<T> {
  key: keyof T;
  label: string;
  render?: (row: T) => React.ReactNode;
}

export default function DataTable<T extends object>({
  columns,
  rows,
  rowKey,
  selectedKey,
  onSelectRow,
}: {
  columns: Column<T>[];
  rows: T[];
  rowKey: keyof T;
  selectedKey?: string | null;
  onSelectRow?: (row: T) => void;
}) {
  const [sortKey, setSortKey] = useState<keyof T | null>(null);
  const [sortAsc, setSortAsc] = useState(true);
  const [page, setPage] = useState(0);
  const pageSize = 5;

  const sorted = [...rows].sort((a, b) => {
    if (!sortKey) return 0;
    const av = String(a[sortKey] ?? "");
    const bv = String(b[sortKey] ?? "");
    return sortAsc ? av.localeCompare(bv) : bv.localeCompare(av);
  });

  const pageCount = Math.max(1, Math.ceil(sorted.length / pageSize));
  const paged = sorted.slice(page * pageSize, page * pageSize + pageSize);

  function toggleSort(key: keyof T) {
    if (sortKey === key) {
      setSortAsc(!sortAsc);
    } else {
      setSortKey(key);
      setSortAsc(true);
    }
  }

  return (
    <div className="dt-wrapper">
      <table className="dt-table">
        <thead>
          <tr>
            {columns.map((col) => (
              <th key={String(col.key)} onClick={() => toggleSort(col.key)}>
                {col.label}
                {sortKey === col.key && <span className="dt-sort-arrow">{sortAsc ? " ▲" : " ▼"}</span>}
              </th>
            ))}
          </tr>
        </thead>
        <tbody>
          {paged.map((row) => {
            const key = String(row[rowKey]);
            const isSelected = selectedKey === key;
            return (
              <tr
                key={key}
                className={isSelected ? "dt-row-selected" : ""}
                onClick={() => onSelectRow?.(row)}
              >
                {columns.map((col) => (
                  <td key={String(col.key)}>{col.render ? col.render(row) : String(row[col.key] ?? "")}</td>
                ))}
              </tr>
            );
          })}
          {paged.length === 0 && (
            <tr>
              <td colSpan={columns.length} className="dt-empty">
                No rows to display
              </td>
            </tr>
          )}
        </tbody>
      </table>
      <div className="dt-pagination">
        <span>
          Showing {paged.length === 0 ? 0 : page * pageSize + 1}
          {"–"}
          {Math.min(sorted.length, page * pageSize + pageSize)} of {sorted.length}
        </span>
        <div className="dt-pagination-btns">
          <button disabled={page === 0} onClick={() => setPage((p) => Math.max(0, p - 1))}>
            Previous
          </button>
          <button disabled={page >= pageCount - 1} onClick={() => setPage((p) => Math.min(pageCount - 1, p + 1))}>
            Next
          </button>
        </div>
      </div>
    </div>
  );
}
